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

#Codebook ----
codebook <- read_excel(here("data", "codebook.xlsx"))
voc_vars <- codebook %>% 
  #Ignore total BTEX and total Xylenes in favor of individual ones.
  filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)



#Functions ----
## Run PCA ----
# Function to run PCA and extract scores and loadings, under various conditions
# Returns a list object with PCA results, loadings, and scores.
runpca <- function(weighted = FALSE, weekcluster = FALSE, model_type = "lmer", highdetect = FALSE,
                   reliability_threshold = 0.4) {
  
  # Ensure model_type is valid
  if (!model_type %in% c("lmer", "glmer")) {
    stop("Invalid model_type. Must be 'lmer' or 'glmer'.")
  }
  
  if (!highdetect){
    data <- vocs
    voc_vars <- codebook %>% 
      #Ignore total BTEX and total Xylenes in favor of individual ones.
      filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)
    
  } else {
    low_detects <- read_csv("results/tables/flag_summary.csv") %>%
      filter((flag_type == "reg" & percentage <=40) | (flag_type == "nd" & percentage >=30) ) %>%
      pull(variable_name)
    
    #Define new VOC variables that don't include the low-detects
    voc_vars <- setdiff(voc_vars, low_detects)
    #Define dataset minus the low detects
    data <- vocs %>% select(-all_of(low_detects))
  }
  
  unreliable_vars <- read_csv("results/tables/reliability_results.csv") %>%
    filter(ICC <= reliability_threshold | is.na(ICC)) %>%
    pull(variable)

  
  data <- data %>% select(-any_of(unreliable_vars))
  
  voc_vars <- setdiff(voc_vars, unreliable_vars)
  
  week_weights <- data %>% pull(tot_weeks)
  
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
    
    # Define the model formula based on clustering arguments
    model_formula <- concentration ~ (1 | week)
    
    # Run the mixed-effects model
    resids <- nested_data %>%
      mutate(
        model = map(data, ~ {
          if (model_type == "lmer") {
            lmer(model_formula, data = .x)
          } else if (model_type == "glmer") {
            glmer(model_formula, family = Gamma(link = "log"), data = .x)
          }
        }),
        residuals = map2(model, data, ~ residuals(.x, type = "response"))
      )
    
    # Extract residuals and reshape into a matrix for PCA
    residual_df <- resids %>%
      select(-model) %>%
      unnest(c(data, residuals)) %>%
      pivot_wider(names_from = voc, values_from = residuals, id_cols = site:tot_weeks) 
    
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


#Get data -----
vocs <- read_csv(here("data", "clean", "dat_ppb.csv"), col_select = -1) %>%
  # Only keep individual BTEX and xylenes species
  # Remove flag value
  dplyr::select(-btex, -xylenes, -ends_with('flag')) 

#Analyses ----

saveto <- here("results", "pca_results")
## Main Analysis ----
# This analysis employs a multilevel gamma model (because residuals are not normally distributed as required by lmer).
# Clustering by week is accounted for in this multilevel model, and PCA is ran on the residual variance 
# that remains unexplained after week effects.
# Rows are weighted by the total number of observations at that site over the year, upweighting more-sampled sites.
# Main analysis restricts to compounds with at least 40% REG samples and those with ICC > 0.4.

#Weighted, GLMER
wk_pca_w_glmer <- runpca(weighted = TRUE, weekcluster = TRUE, model_type = "glmer", highdetect = TRUE, 
                         reliability_threshold = 0.4)
write_rds(wk_pca_w_glmer, here(saveto, "mainanalysis_wk_pca_w_glmer.rds"))



# Sensitivity Analyses -----
## SA1: Including low-detect compounds --------
#Weighted, GLMER
wk_lowdetect_pca_w_glmer <- runpca(weekcluster = TRUE, weighted = TRUE, model_type = "glmer", highdetect = FALSE,
                                   reliability_threshold = 0.4)
write_rds(wk_lowdetect_pca_w_glmer, here(saveto, "sens1_wk_lowdetect_pca_w_glmer.rds"))

## SA2: Setting reliability threshold to 0.5 --------
#Weighted, GLMER
wk_pca_w_glmer_icc05 <- runpca(weekcluster = TRUE, weighted = TRUE, model_type = "glmer", highdetect = TRUE,
                                   reliability_threshold = 0.5)
write_rds(wk_pca_w_glmer_icc05 , here(saveto, "sens2_wk_pca_w_glmer_icc05 .rds"))

## SA3: Not weighting by total weeks -----------
#Unweighted, GLMER
wk_pca_uw_glmer <- runpca(weekcluster = TRUE, model_type = "glmer", highdetect = TRUE)
write_rds(wk_pca_uw_glmer, here(saveto, "sens3_wk_pca_uw_glmer.rds"))

## SA4: Using Gaussian, rather than gamma distribution ------
#Weighted, LMER
wk_pca_w_lmer <- runpca(weekcluster = TRUE, weighted = TRUE, model_type = "lmer", highdetect = TRUE)
write_rds(wk_pca_w_lmer, here(saveto, "sens4_wk_pca_w_lmer.rds"))

## SA5: Using Raw VOC concentrations --------
### 5A: Unweighted ----
pca_uw <- runpca(highdetect = TRUE)
write_rds(pca_uw, here(saveto, "sens5a_pca_uw.rds"))

### 5B: Weighted ------
pca_w <- runpca(weighted = TRUE, highdetect = TRUE)
write_rds(pca_w, here(saveto, "sens5b_pca_w.rds"))

