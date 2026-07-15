library(FactoMineR)
library(lme4)
library(tidyverse)
library(readxl)
library(here)

# Purpose of this code:
# To use principal component analysis for source apportionment, as a sensitivity analysis. 
# Because we have clustering by site and week, several different versions fo the PCA
# will be run, accounting for various clustering. 

# OUTPUT MAP: which code block produces which publication figure/table
# --------------------------------------------------------------------
# Figure S14 -> mainanalysis_wk_pca_w_glmer.png       (weighted, week-clustered GLMER PCA)
# Figure S13 -> sens2c_wk_pca_w_glmer_stationary.png  (stationary sites only)
#
# All PCA loading plots from this script are supplemental -- none belong in
# the main-text results/figures folder. `plot_loadings()` below always saves
# to results/supplemental/figures regardless of object name.
# --------------------------------------------------------------------

# Codebook ----------------------------------------------------------------
codebook <- read_excel(here("data", "codebook.xlsx"))

voc_vars <- codebook %>% 
  #Ignore total BTEX and total Xylenes in favor of individual ones.
  filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)


excluded_compounds <- read_csv("results/supplemental/tables/suppt1_detection_reliability.csv") %>%
  filter(low_reliability == 1 | low_detection_rate == 1) %>%
  pull(variable_name)




# Functions ---------------------------------------------------------------

## Run PCA ----
# Function to run PCA and extract scores and loadings, under various conditions
# Returns a list object with PCA results, loadings, and scores.
runpca <- function(weighted = FALSE, weekcluster = FALSE, model_type = "lmer", include_all = FALSE,
                   stationary_only = FALSE, season = c("all", "winter", "summer")) {
  
  season <- match.arg(season) 
  
  vocs <- vocs %>%
    mutate(
      month_num = month(as.Date(end_date)),
      season_meteo = case_when(
        month_num %in% c(12, 1, 2) ~ "Winter",
        month_num %in% c(6, 7, 8)  ~ "Summer",
        TRUE ~ "Other"
      )
    )
  
  # If stationary sites only, then filter 
  if (stationary_only){
    vocs <- vocs %>% filter(site_type == "stationary")
  }
  
  # Ensure model_type is valid
  if (!model_type %in% c("lmer", "glmer")) {
    stop("Invalid model_type. Must be 'lmer' or 'glmer'.")
  }
  
  # season == "all" needs no filtering
  if (season == "winter"){
    vocs <- vocs %>%
      filter(season_meteo == "Winter")
  } else if (season == "summer"){
    vocs <- vocs %>%
      filter(season_meteo == "Summer")
  }
  
  # Default: don't include the excluded compounds
  if (!include_all){
    #Define new VOC variables that don't include the low-detects
    voc_vars <- codebook %>% 
      #Ignore total BTEX and total Xylenes in favor of individual chemicals.
      filter(!(variable_name %in% c("xylenes", "btex")), 
             !(variable_name %in% excluded_compounds)) %>% 
      pull(variable_name)
    
    #Define dataset minus the low detects
    data <- vocs %>% dplyr::select(-all_of(excluded_compounds))
    
    # if specified: pull all compounds 
  } else {
    data <- vocs
    voc_vars <- codebook %>% 
      #Ignore total BTEX and total Xylenes in favor of individual chemicals.
      filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)
  }
  
  # IMPORTANT FIX: row weights must stay aligned with the exact row order of
  # whatever matrix ultimately gets passed to PCA(). The previous version
  # computed `week_weights` as a standalone vector from a separately
  # group_by()+summarize()'d table -- dplyr's summarize() sorts its output by
  # the grouping keys, which is not guaranteed to match the row order of
  # `vocs_only` (or, in the week-clustering branch, `residual_mat`, which is
  # additionally reshaped through nest()/unnest()/pivot_wider()). If the
  # orders didn't match, PCA's row.w would silently be applied to the wrong
  # observations -- a real risk for the weighted models, including the
  # primary/main analysis.
  # Fix: attach the weight as a *column* on `data` (keyed by site_id +
  # end_date) so it travels through any downstream reshaping already
  # correctly attached to its own row, and pull it from the final object
  # immediately before each call to PCA().
  week_weight_lookup <- data %>% 
    group_by(site_id, end_date) %>%
    summarize(tot_weeks = sum(sample_length), .groups = "drop")
  
  data <- data %>%
    left_join(week_weight_lookup, by = c("site_id", "end_date"))
  
  ## PCA ------------
  
  ### No clustering 
  if (!weekcluster) {
    vocs_only <- data %>% dplyr::select(all_of(voc_vars))
    row_weights <- data$tot_weeks
    
    # Run PCA on raw values without clustering
    rawresults <- if (!weighted) {
      PCA(vocs_only, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(vocs_only, scale.unit = TRUE, graph = FALSE, row.w = row_weights)
    }
    
    # Retain components with eigenvalue > 1
    n_comps <- sum(rawresults$eig[, 1] > 1)
    pca_results <- if (!weighted) {
      PCA(vocs_only, ncp = n_comps, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(vocs_only, ncp = n_comps, scale.unit = TRUE, graph = FALSE, row.w = row_weights)
    }
    
    
  } else {
    ### Week clustering 
    # If weekcluster is TRUE, proceed with residual extraction
    nested_data <- data %>%
      pivot_longer(cols = all_of(voc_vars), names_to = "voc", values_to = "concentration") %>%
      group_by(voc) %>%
      mutate(
        concentration = case_when(
          concentration != 0 ~ concentration,
          concentration == 0 ~ 0.0000001
        )
      ) %>%
      nest()
    
    # Define the model formula based on clustering within week
    model_formula <- concentration ~ (1 | week)
    
    # Run the mixed-effects model
    resids <- nested_data %>%
      mutate(
        model = map(data, ~ {
          if (model_type == "lmer") {
            lmer(model_formula, data = .x)
          } else if (model_type == "glmer") {
            glmer(model_formula, family = Gamma(link = "log"), data = .x,
                  control = glmerControl(optimizer = "bobyqa", 
                                         optCtrl = list(maxfun = 1e6),
                                         check.conv.grad = .makeCC("warning", tol = 0.05, relTol = NULL)
                  )
            )
            
          }
        }),
        residuals = map2(model, data, ~ residuals(.x, type = "response"))
      )
    
    # Extract residuals and reshape into a matrix for PCA
    # NOTE: `tot_weeks` is explicitly carried alongside the site_id:sample_length
    # range so it survives this reshape and stays correctly row-aligned with
    # residual_mat below (rather than being recomputed/re-pulled separately).
    residual_df <- resids %>%
      select(-model) %>%
      unnest(c(data, residuals)) %>%
      pivot_wider(names_from = voc, values_from = residuals,
                  id_cols = c(site_id:sample_length, tot_weeks))
    
    
    # Update voc_vars to reflect any dropped variables due to failed convergence
    voc_vars <- intersect(voc_vars, names(residual_df))
    
    residual_mat <- residual_df %>%
      select(all_of(voc_vars))
    row_weights <- residual_df$tot_weeks
    
    # Run PCA
    rawresults <- if (!weighted) {
      PCA(residual_mat, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(residual_mat, scale.unit = TRUE, graph = FALSE, row.w = row_weights)
    }
    
    # Retain components with eigenvalue > 1
    n_comps <- sum(rawresults$eig[, 1] > 1)
    pca_results <- if (!weighted) {
      PCA(residual_mat, ncp = n_comps, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(residual_mat, ncp = n_comps, scale.unit = TRUE, graph = FALSE, row.w = row_weights)
    }
    
    
  }
  
  
  
  ## Extract loadings -------
  loading_mat <- pca_results$svd$V
  
  #Varimax rotate for ease of interpretation
  rotated_mat <- as.matrix(varimax(loading_mat)$loadings)
  rotated_df <- as.data.frame(unclass(rotated_mat))
  
  colnames(rotated_df) <- colnames(pca_results$var$coord)
  
  loadings <- rotated_df %>%
    mutate(variable_name = rownames(pca_results$var$coord)) %>%
    left_join(., codebook, by = 'variable_name')
  
  pctvar <- data.frame(pca_results$eig) %>%
    mutate(dimension = paste("Dim", row_number(), sep = "."))
  
  ## Build scores -----------
  comp_names <- loadings %>%
    select(starts_with("Dim")) %>%
    names()
  
  
  score_formulae <- map(comp_names, function(comp){
    vars <- loadings$variable_name
    coefs <- loadings[[comp]]
    
    paste(coefs, "*", vars, collapse = " + ")
  })
  
  names(score_formulae) <- comp_names
  
  # If no clustering, build scores on scaled concentrations
  if (!weekcluster) {
    scores <- data %>% 
      # Scale the data
      mutate(across(all_of(voc_vars), ~as.numeric(scale(.x, center = TRUE, scale = TRUE)))) %>%
      mutate(
        !!!map(comp_names, function(comp) {
          expr(!!rlang::parse_expr(score_formulae[[comp]]))
        }) %>%
          set_names(comp_names)
      ) %>%
      select(-(all_of(voc_vars)))
  } else {
    # If clustering, build scores on scaled residuals
    scores <- residual_df %>%
      mutate(across(all_of(voc_vars), ~as.numeric(scale(.x, center = TRUE, scale = TRUE)))) %>%
      mutate(
        !!!map(comp_names, function(comp) {
          expr(!!rlang::parse_expr(score_formulae[[comp]]))
        }) %>%
          set_names(comp_names)
      ) %>%
      select(-(all_of(voc_vars)))
  }
  
  
  ## Return results and scores
  final_result <- list(pca_results, loadings, scores, pctvar)
  names(final_result) <- c("pca_results", "loadings", "scores", "pctvar")
  
  return(final_result)
  
  
}


# Get Data ----------------------------------------------------------------

vocs <- read_csv(here("data", "clean", "dat_ppb.csv")) %>%
  # Only keep individual BTEX and xylenes species
  # Remove flag value and uncertainties
  dplyr::select(-btex, -xylenes, -ends_with("flag"), -ends_with("_unc")) 



# Save Results Path -------------------------------------------------------

saveto <- here("results", "interim_results","pca_results")


# Primary Analysis --------------------------------------------------------
# This analysis employs should employ a multilevel model.
# Residuals are not normally distributed, as required by LMER, but to LMER is used to 
# ensure convergence. A sensitivity analysis tests a gamma distribution.
# Clustering by week is accounted for in this multilevel model, and PCA is ran on the residual variance 
# that remains unexplained after week effects.
# Rows are weighted by the total number of observations at that site over the year, upweighting more-sampled sites.
# Main analysis restricts to compounds with at least 40% REG samples and those with ICC > 0.4.

#Weighted, GLMER
# Figure S14 (supplemental)
wk_pca_w_glmer <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "glmer")
write_rds(wk_pca_w_glmer, here(saveto, "mainanalysis_wk_pca_w_glmer.rds"))

### Include weekly sites only
# Figure S13
wk_pca_w_glmer_stationary <- runpca(weekcluster = TRUE, weighted = TRUE, model_type = "glmer",
                                    stationary_only = TRUE)
write_rds(wk_pca_w_glmer_stationary, here(saveto, "sens2c_wk_pca_w_glmer_stationary.rds"))

# Functions ---------------------------------------------------------------

## Plot Loadings ----
plot_loadings <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  weight_text <- if(str_detect(object_name, "_w")) "Weighted" else NULL
  sitetype_text <- if(str_detect(object_name, "stationary")) "Year-Round Sites Only" else NULL
  cluster_text <- if (str_detect(object_name, "wk_")){
    if (str_detect(object_name, "_glmer")){
      "Adjusted for within-week clustering (Gamma)"
    } else {
      "Adjusted for within-week clustering (Gaussian)"
    }
  } else NULL
  
  detail_text <- paste(compact(list(weight_text, cluster_text, sitetype_text)), collapse = ", ")
  
  df <- result_object$loadings
  pctvar <- result_object$pctvar
  
  
  # Reorder VOC by category_2
  df$category_2 <- factor(
    df$category_2,
    levels = category_levels
  )
  
  plot <- df %>%
    arrange(category_2, voc_name) %>% # Sort by category_2 and then voc_name
    mutate(voc_name = factor(voc_name, levels = unique(voc_name))) %>%# Set the order for voc_name
    pivot_longer(starts_with('Dim'), names_to = "dimension", values_to = "contribution") %>%
    left_join(., pctvar, by = 'dimension') %>%
    mutate(dimension = paste0(str_replace(dimension, "Dim.", "Component "), ": ",signif(percentage.of.variance,2),"%")) %>%
    ggplot() +
    aes(x = voc_name, y = contribution, fill = category_2) + 
    geom_col() +
    facet_wrap(~dimension, ncol = 2) + 
    # theme_bw() + 
    coord_flip() + 
    labs(
      title = paste0("Component Loadings by VOC"),
      subtitle = detail_text,
      y = "Standardized Varimax-Rotated Loading",
      x = "VOC",
      fill = "VOC Group"
    ) +
    paper_theme + 
    theme(
      axis.text.x = element_text(angle = 30),
      legend.key.height = unit(0.3, "in"),
      legend.key.width = unit(0.3, "in")
    )
  
  # All PCA loading plots are supplemental -- none of these belong in the
  # main-text results/figures folder, regardless of object name.
  outpath <- here("results", "supplemental", "figures")
  
  
  ggsave(
    filename = here(outpath, paste0(object_name,".png")),
    plot = plot,
    dpi = 320,
    units = "in",
    width = 9,
    height = 11
  )
  
}



# Get Results -------------------------------------------------------------

resultpath <- here("results", "interim_results", "pca_results")
results <- list.files(resultpath, full.names = TRUE)

# NOTE (reproducibility): this reads and plots *every* .rds file currently
# sitting in `resultpath`, not just the two written above. If older/stale
# .rds files from previous exploratory runs are left in this folder, they'll
# get plotted here too. For a clean, reproducible run, clear this folder (or
# otherwise confirm its contents) before re-running this script end to end.
results_list <- lapply(results,  readRDS)
names(results_list) <- tools::file_path_sans_ext(list.files(resultpath))


# Tabulate Results --------------------------------------------------------
## Tabulating main analysis results only ------
main_results <- results_list[["mainanalysis_wk_pca_w_glmer"]]$scores



# Plot Loadings -----------------------------------------------------------
for (r in names(results_list)) {
  plot_loadings(result_object = results_list[[r]], object_name = r)
}

