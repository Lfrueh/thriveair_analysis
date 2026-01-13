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
                   stationary_only = FALSE) {
  
  # If stationary sites only, then filter 
  if (stationary_only){
    vocs <- vocs %>% filter(site_type == "stationary")
  }
  
  # Ensure model_type is valid
  if (!model_type %in% c("lmer", "glmer")) {
    stop("Invalid model_type. Must be 'lmer' or 'glmer'.")
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
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))
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

#Weighted, LMER
wk_pca_w_lmer <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "lmer")
write_rds(wk_pca_w_lmer, here(saveto, "mainanalysis_wk_pca_w_lmer.rds"))


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
wk_pca_uw_lmer <- runpca(weekcluster = TRUE, weighted = FALSE, model_type = "lmer",
                                    stationary_only = FALSE)
write_rds(wk_pca_uw_lmer, here(saveto, "sens2a_wk_pca_uw_lmer.rds"))

### 2b. Use gamma instead of gaussian distribution -----
wk_pca_w_glmer <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "glmer")
write_rds(wk_pca_w_glmer, here(saveto, "sens2b_wk_pca_w_glmer.rds"))

### 2c. Include weekly sites only
wk_pca_w_lmer_stationary <- runpca(weekcluster = TRUE, weighted = TRUE, model_type = "lmer",
                                     stationary_only = TRUE)
write_rds(wk_pca_w_lmer_stationary, here(saveto, "sens2c_wk_pca_w_lmer_stationary.rds"))
