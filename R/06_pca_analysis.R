library(FactoMineR)
library(lme4)
library(tidyverse)
library(readxl)
library(here)

########################################################
# Purpose of this code:
# To use principal component analysis for source apportionment. 
# Because we have clustering by site and week, several different versions fo the PCA
# will be run, accounting for various clustering. 
########################################################


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
  
  if (season == "all"){
    vocs <- vocs
  } else if (season == "winter"){
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
    
    setdiff(voc_vars, excluded_compounds)
    
    #Define dataset minus the low detects
    data <- vocs %>% dplyr::select(-all_of(excluded_compounds))
    
    # if specified: pull all compounds 
  } else {
    data <- vocs
    voc_vars <- codebook %>% 
      #Ignore total BTEX and total Xylenes in favor of individual chemicals.
      filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)
  }
  
  week_weights <- data %>% 
    group_by(site_id, end_date) %>%
    summarize(tot_weeks = sum(sample_length)) %>%
    pull(tot_weeks)
  
  ## PCA ------------
  
  ### No clustering 
  if (!weekcluster) {
    vocs_only <- data %>% dplyr::select(all_of(voc_vars))
    # Run PCA on raw values without clustering
    rawresults <- if (!weighted) {
      PCA(vocs_only, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(vocs_only, scale.unit = TRUE, graph = FALSE, row.w = week_weights)
    }
    
    # Retain components with eigenvalue > 1
    n_comps <- sum(rawresults$eig[, 1] > 1)
    pca_results <- if (!weighted) {
      PCA(vocs_only, ncp = n_comps, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(vocs_only, ncp = n_comps, scale.unit = TRUE, graph = FALSE, row.w = week_weights)
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
    residual_df <- resids %>%
      select(-model) %>%
      unnest(c(data, residuals)) %>%
      pivot_wider(names_from = voc, values_from = residuals, id_cols = site_id:sample_length)
    
    
    # Update voc_vars to reflect any dropped variables due to failed convergence
    voc_vars <- intersect(voc_vars, names(residual_df))
    
    residual_mat <- residual_df %>%
      select(all_of(voc_vars))
    
    # Run PCA
    rawresults <- if (!weighted) {
      PCA(residual_mat, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(residual_mat, scale.unit = TRUE, graph = FALSE, row.w = week_weights)
    }
    
    # Retain components with eigenvalue > 1
    n_comps <- sum(rawresults$eig[, 1] > 1)
    pca_results <- if (!weighted) {
      PCA(residual_mat, ncp = n_comps, scale.unit = TRUE, graph = FALSE)
    } else {
      PCA(residual_mat, ncp = n_comps, scale.unit = TRUE, graph = FALSE, row.w = week_weights)
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
wk_pca_w_glmer <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "glmer")
write_rds(wk_pca_w_glmer, here(saveto, "mainanalysis_wk_pca_w_glmer.rds"))

#Weighted, GLMER, Summer
wk_pca_w_glmer_summer <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "glmer", season = "summer")
write_rds(wk_pca_w_glmer_summer, here(saveto, "mainanalysis_wk_pca_w_glmer_summer.rds"))

#Weighted, GLMER, Winter
wk_pca_w_glmer_winter <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "glmer", season = "winter")
write_rds(wk_pca_w_glmer_winter, here(saveto, "mainanalysis_wk_pca_w_glmer_winter.rds"))



# Sensitivity Analyses ----------------------------------------------------
## SA1: Ignore Week Clustering ------
### 1ai. Include all sites, weight by total weeks -----
pca_w <- runpca(weekcluster = FALSE, weighted = TRUE, stationary_only = FALSE)
write_rds(pca_w, here(saveto, "sens1ai_pca_w.rds"))

### 1aii. Include all sites, unweighted -----
pca_uw <- runpca()
write_rds(pca_uw, here(saveto, "sens1aii_pca_uw.rds"))

### 1b. Include weekly sites only -----
pca_stationary <- runpca(weekcluster = FALSE, weighted = FALSE, stationary_only = TRUE)
write_rds(pca_stationary, here(saveto, "sens1b_pca_stationary.rds"))

## SA2: Modify Main Analysis ----
### 2a. Unweighted by total weeks ----
wk_pca_uw_glmer <- runpca(weekcluster = TRUE, weighted = FALSE, model_type = "glmer",
                          stationary_only = FALSE)
write_rds(wk_pca_uw_glmer, here(saveto, "sens2a_wk_pca_uw_glmer.rds"))

### 2b. Use gaussian instead of gamma distribution -----
wk_pca_w_lmer <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "lmer")
write_rds(wk_pca_w_lmer, here(saveto, "sens2b_wk_pca_w_lmer.rds"))

### 2c. Include weekly sites only
wk_pca_w_glmer_stationary <- runpca(weekcluster = TRUE, weighted = TRUE, model_type = "glmer",
                                    stationary_only = TRUE)
write_rds(wk_pca_w_glmer_stationary, here(saveto, "sens2c_wk_pca_w_glmer_stationary.rds"))

# Functions ---------------------------------------------------------------

## Plot Loadings ----
plot_loadings <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  weight_text <- if(str_detect(object_name, "_w")) "Weighted" else NULL
  sitetype_text <- if(str_detect(object_name, "stationary")) "Weekly Sites Only" else NULL
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
  
  # Save sensitivity analyses to supplemental
  if (str_detect(object_name, "mainanalysis")){
    outpath <- here("results","figures")
  } else {
    outpath <- here("results", "supplemental", "figures")
  }
  
  
  ggsave(
    filename = here(outpath, paste0(object_name,".png")),
    plot = plot,
    dpi = 320,
    units = "in",
    width = 9,
    height = 11
  )
  
}

map_avg_scores <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  detect_text <- if(str_detect(object_name, "include_all")) "Includes all compounds" else NULL
  weight_text <- if(str_detect(object_name, "_w")) "Weighted" else NULL
  sitetype_text <- if(str_detect(object_name, "stationary")) "Weekly Sites Only" else NULL
  cluster_text <- if (str_detect(object_name, "wk_")){
    if (str_detect(object_name, "_glmer")){
      "Adjusted for within-week clustering (Gamma)"
    } else {
      "Adjusted for within-week clustering (Gaussian)"
    }
  } else NULL
  
  detail_text <- paste(compact(list(weight_text, cluster_text, detect_text, sitetype_text)), collapse = ", ")
  
  df <- result_object$scores %>%
    group_by(site_id) %>%
    mutate(across(starts_with("Dim."), ~mean(.))) %>%
    ungroup() %>%
    distinct() %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  comp_names <- df %>%
    st_drop_geometry() %>%
    select(starts_with("Dim.")) %>%
    names()
  
  
  # Will save one map for each component
  for (comp in comp_names){
    comp_number <- str_sub(comp, -1, -1)  
    
    q_low  <- round(quantile(df[[comp]], 0.05, na.rm = TRUE),1)
    q_high <- round(quantile(df[[comp]], 0.95, na.rm = TRUE),1)
    q_med <- round(median(df[[comp]], na.rm = TRUE),1)
    
    high_label <- paste0("> ", q_high)
    low_label  <- paste0("< ", q_low)
    
    # To fix upon publication -- outlier values
    df <- df %>%
      mutate(outlier_flag = case_when(df[[comp]] > q_high ~ high_label,
                                      df[[comp]] < q_low ~ low_label,
                                      TRUE ~ NA
      )
      )
    
    
    comp_plot <- ggmap(basemap) +
      # In-range data
      geom_sf(data = df, 
              inherit.aes = FALSE, aes(fill = .data[[comp]],
                                       shape = site_type),
              size = 8, color = "black", stroke = 0.4) + 
      coord_sf(expand = FALSE) + 
      coord_zoom(1.15) + 
      # scale_fill_gradient2(low = "#086788", mid = "darkgrey",high = "#DD1C1A",
      #                       midpoint = q_med) + 
      scale_fill_gradient2(low = "#086788", mid = "darkgrey",high = "#DD1C1A") + 
      scale_shape_manual(
        values = c("stationary" = 21, "rotating" = 22),
        labels = c("stationary" = "Weekly", "rotating" = "Community"),
        name = "Site Type"
      ) +
      labs(
        title = paste0("Average Component ", comp_number, " Score"),
        subtitle = detail_text,
        x = NULL,
        y = NULL,
        fill = paste0("Comp. ",comp_number," Score"),
        shape = "Site Type"
      ) +
      annotation_north_arrow(
        location = "tl", which_north = "grid",
        height = unit(0.6, "in"), width = unit(0.6, "in"),
        style = north_arrow_orienteering(text_size = 30)
      ) +
      annotation_scale(
        data = df,
        unit_category = "imperial",
        location = "br",
        width_hint = 0.5,
        text_cex = 2,
        height = unit(0.2, "cm")
      ) +
      paper_theme + 
      theme(
        plot.background = element_rect("white"),
        #   axis.text.x = element_blank(),
        #   axis.text.y = element_blank(),
        legend.title = element_text(size = 30, lineheight = 0.5)
      ) 
    
    # Save sensitivity analyses to supplemental
    if (str_detect(object_name, "sens")){
      outpath <- here("results","supplemental","figures")
    } else {
      outpath <- here("results", "figures")
    }
    
    
    ggsave(
      plot = comp_plot,
      filename = here(filename = here(outpath, paste0(object_name,"_comp_",comp_number,"_score_map.png"))),
      height = 8, width = 8, units = "in",
      dpi = 320
      
    )
    
  }
  
  
}


# Get Results -------------------------------------------------------------

resultpath <- here("results", "interim_results", "pca_results")
results <- list.files(resultpath, full.names = TRUE)

results_list <- lapply(results,  readRDS)
names(results_list) <- tools::file_path_sans_ext(list.files(resultpath))


# Tabulate Results --------------------------------------------------------
## Tabulating main analysis results only ------
main_results <- results_list[["mainanalysis_wk_pca_w_glmer"]]$scores

## Scores by land-use type -------
pca_scores_landuse <- main_results %>%
  group_by(industrial_20, site_traffic) %>%
  summarize(
    across(
      starts_with("Dim"),
      ~ sprintf("%.2f (%.2f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_csv(pca_scores_landuse, here("results", "tables", "pca_scores_by_landuse.csv"))

## Scores by site -------
pca_scores_site <- main_results %>%
  filter(site_type == "stationary") %>%
  group_by(site_id, site) %>%
  arrange(site_id) %>%
  summarize(
    across(
      starts_with("Dim"),
      ~ sprintf("%.2f (%.2f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_csv(pca_scores_site, here("results", "tables", "pca_scores_by_site.csv"))


# Heatmap of Scores -------------------------------------------------------
## Scores for all sites
pca_scores_site <- main_results %>%
  group_by(site, site_type, land_use, industrial_20, site_traffic, nearby_sources) %>%
  summarize(
    across(
      starts_with("Dim"),
      ~ round(mean(.x, na.rm = TRUE),2))
    ,
    .groups = "drop"
  ) %>%
  rename_with(~str_replace(.x, "Dim.", "Component_")) %>%
  mutate(site_type = case_when(
    site_type == "stationary" ~ "Weekly",
    site_type == "rotating" ~ "Community"
  )) %>%
  relocate(nearby_sources, .after = "Component_4") %>%
  arrange(industrial_20, site_traffic, land_use, site_type, site)


# Create a workbook and add data
wb <- createWorkbook()
addWorksheet(wb, "PCA Scores")

writeData(wb, "PCA Scores", pca_scores_site)

# Get all the component columns
component_cols <- grep("^Component", names(pca_scores_site), value = TRUE)

# Apply red-white-blue gradient to each component column separately
for (col in component_cols) {
  conditionalFormatting(
    wb,
    sheet = "PCA Scores",
    cols = col,
    rows = 2:(nrow(pca_scores_site) + 1),
    style = c("#086788", "white","#DD1C1A"),
    type = "colorScale",
    rule = c(min(pca_scores_site[[col]], na.rm = TRUE), 0, max(pca_scores_site[[col]], na.rm = TRUE))
  )
}

# Save Excel workbook
saveWorkbook(wb, "results/tables/pca_scores_site_conditional.xlsx", overwrite = TRUE)


# Box Plot of Scores ------------------------------------------------------

pca_score_boxplot <- main_results %>%
  mutate(
    site_id = factor(site_id, levels = c(
      main_results %>% filter(site_type == "stationary") %>% arrange(site_id) %>% pull(site_id) %>% unique(),
      main_results %>% filter(site_type == "rotating") %>% arrange(site_id) %>%  pull(site_id) %>% unique()
    ))
  ) %>%
  pivot_longer(Dim.1:Dim.4, names_to = "comp", values_to = "score") %>%
  mutate(comp = str_replace(comp, "Dim.", "Component ")) %>%
  ggplot(aes(x = site_id, y = score, fill = site_type)) +
  geom_boxplot(outlier.color = "black", outlier.size = 0.4, size = 0.4) +
  theme_minimal() +
  labs(
    title = "Scores by Site",
    x = "Site",
    y = "Score",
    fill = "Site Type"
  ) +
  scale_fill_manual(
    values = c(
      "rotating" = "#1f77b4",
      "stationary" = "#ff7f0e"
    ),
    labels = c(
      "rotating" = "Community Sites",
      "stationary" = "Weekly Sites"
    )
  ) +
  paper_theme +
  coord_flip() + 
  facet_wrap(~ comp, scales = "free_x", nrow = 2) +
  theme(legend.position = "bottom")

ggsave(
  here("results", "supplemental", "figures", "pca_score_boxplot.png"),
  plot = pca_score_boxplot,
  width = unit(9, "in"),
  height = unit(8, "in"),
  dpi = 320
)

# Plot Loadings -----------------------------------------------------------
for (r in names(results_list)) {
  plot_loadings(result_object = results_list[[r]], object_name = r)
}


# Map Average Scores ------------------------------------------------------
# for (r in names(results_list)) {
#   map_avg_scores(result_object = results_list[[r]], object_name = r)
# }

