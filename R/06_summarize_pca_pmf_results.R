library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(patchwork)
library(cowplot)
library(ggspatial)
library(ggrepel)
library(ggmap)
library(ggtext)
library(here)
library(scales)
library(openxlsx)

source("R/00_plot_theme.R")  


# Codebook ----------------------------------------------------------------


codebook <- read_excel('data/codebook.xlsx')
voc_vars <- codebook %>% 
  #Ignore total BTEX and total Xylenes in favor of individual ones.
  filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)

site_info <- read_csv("data/site_info.csv")

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


# Plot PMF Results -------------------------------

get_pmf_factors <- function(site_type = c("allsites","stationary")){
  
  if (site_type == "stationary"){
    factor_file <- "stationary_pmf_bs_fpeak.xlsx"
  } else {
    factor_file <- "allsites_pmf_bs_fpeak.xlsx"
  }
  
  
  # Get factor information
  readsheet <- function(sheetname){
    read_excel(file.path("results", "interim_results","pmf_results", factor_file),
               sheet = sheetname) %>%
      janitor::clean_names() %>%
      mutate(across(fpeak_value_conc:bs_95th_pct, ~as.numeric(.x))) %>%
      rename(variable_name = species)
  }
  
  # For a four-factor solution
  sheets <- c("Factor 1", "Factor 2", "Factor 3", "Factor 4", "Factor 5")
  
  pmf_fac <- map(sheets, ~readsheet(.x))
  names(pmf_fac) <- sheets

  
  pmf_fac_results <- bind_rows(pmf_fac, .id = "factor") %>%
    left_join(., codebook, by = "variable_name") %>%
    mutate(total_mass = sum(bs_median_conc)) %>%
    group_by(factor) %>%
    # Calculate % mass explained by each factor
    mutate(
      total_mass_fac = paste0(" (",round(100*sum(bs_median_conc) / total_mass,0),"%)")
    ) %>%
    # Normalize concentration contributions for plotting
    mutate(denominator = sum(bs_median_conc),
           bs_median_conc = 100*bs_median_conc/denominator,
           bs_95th_conc = 100*bs_95th_conc/denominator,
           bs_5th_conc = 100*bs_5th_conc/denominator) %>%
    ungroup() %>%
    mutate(factor_text = paste0(factor, total_mass_fac))
  
  return(pmf_fac_results)
  
  
}


plot_pmf_factors <- function(site_type = "allsites", print = FALSE){
  
  if (site_type == "stationary"){
    subtitle_text = "Weekly sites only"
    
    data <- pmf_factors_stationary
    
  } else {
    subtitle_text = "All sites"
    
    data <- pmf_factors
  }
  
  file_name <- paste0(site_type,"_pmf.png")
  
  data$category_2 <- factor(data$category_2,levels = category_levels)
  
  data <- data %>%
    arrange(category_2, voc_name) %>%
    mutate(voc_name = factor(voc_name, levels = unique(voc_name))) # Set the order for voc_name
  
  # Plot 1: VOC Contribution to Factors
  plot1 <- data %>%
    ggplot() +
    aes(x = voc_name, y = bs_median_conc, fill = category_2) + 
    geom_col() +
    geom_errorbar(
      aes(ymin = bs_5th_conc, ymax = bs_95th_conc),
      width = 0.5,      
      color = "gray50",  
      linewidth = 0.3  
    ) +
    facet_wrap(~factor_text, scales = "free_x", ncol = 2) + 
    coord_flip() + 
    labs(
      title = "Relative VOC Contributions to Each Factor",
      subtitle = subtitle_text,
      y = "Normalized Concentration (%)",
      x = "VOC",
      fill = "VOC Group"
    ) +
    paper_theme + 
    theme(
      axis.text.x = element_text(angle = 30),
      legend.key.height = unit(0.3, "in"),
      legend.key.width = unit(0.3, "in")
    )  
  
  
  ggsave(
    file.path("results", "figures", paste0("fact_contrib_", file_name)),
    plot = plot1,
    width = unit(9, "in"),
    height = unit(11, "in"),
    dpi = 320
  )
  
  # Plot 2: Factor Contributions to Total VOC Mass
  
  plot2 <- data %>%
    ggplot() + 
    aes(x = voc_name, y = bs_median_pct, fill = Factor) + 
    geom_bar(position = position_stack(reverse = TRUE), stat = "identity") + 
    coord_flip() + 
    theme_minimal() +
    scale_fill_manual(
      values = c(
        "#1F3A8A",  # dark blue
        "#F59E0B",  # bright orange
        "turquoise", # light blue
        "#B91C1C",  # deep red
        "#16A34A"  # bright green
      )
    ) + 
    labs(x =  "VOC",y = "Contribution (%)",
         fill = "Factor") +
    paper_theme
    
  
  ggsave(
    file.path("results", "figures", paste0("species_contrib_", file_name)),
    plot = plot2,
    width = unit(8, "in"),
    height = unit(8, "in"),
    dpi = 320
  )
    
    
    
  if(print){
    print(plot1)
    print(plot2)
  }
  
}

# Get results & Add interpretation

## All sites
pmf_factors <- get_pmf_factors("allsites") %>%
  mutate(
   Factor = case_when(
   factor == "Factor 1" ~ "Industrial Solvents",
   factor == "Factor 4" ~ "Gasoline Evaporation",
   factor == "Factor 3" ~ "Background Pollution",
   factor == "Factor 2" ~ "Vehicle exhaust",
   factor == "Factor 5" ~ "Mixed Industry"
   ),
   Factor = factor(Factor, levels = 
                     c("Vehicle exhaust",
                       "Gasoline Evaporation",
                       "Industrial Solvents",
                       "Background Pollution",
                       "Mixed Industry")),
  factor_text = paste(Factor, total_mass_fac)
  ) %>%
  arrange(desc(total_mass_fac)) %>%
  mutate(factor_text = factor(factor_text, levels = unique(factor_text)))
  
plot_pmf_factors("allsites")

## Stationary Sites 
pmf_factors_stationary <- get_pmf_factors("stationary") %>%
  mutate(
    Factor = case_when(
    factor == "Factor 4" ~ "Industrial Solvents",
    factor == "Factor 5" ~ "Gasoline Evaporation",
    factor == "Factor 1" ~ "Background Pollution",
    factor == "Factor 3" ~ "Vehicle Exhaust",
    factor == "Factor 2" ~ "Mixed Industry"
  ),
  factor_text = paste(Factor, total_mass_fac)
  ) %>%
  arrange(desc(total_mass_fac)) %>%
  mutate(factor_text = factor(factor_text, levels = unique(factor_text)))



plot_pmf_factors("stationary")




# Main PMF Analysis: Contributions to Sites ------

pmf_contribs <- read_excel("results/interim_results/pmf_results/allsites_contributions_bs_fpeak.xlsx") %>%
  filter(!is.na(site_id2))



site_contribs <- pmf_contribs %>%
  group_by(site_id2) %>%
  summarize(
    total_mass = sum(factor_1) + sum(factor_2) + sum(factor_3) + sum(factor_4) + sum(factor_5),
    factor_1 = 100*sum(factor_1)/total_mass,
    factor_2 = 100*sum(factor_2/total_mass),
    factor_3 = 100*sum(factor_3)/total_mass,
    factor_4 = 100*sum(factor_4)/total_mass,
    factor_5 = 100*sum(factor_5)/total_mass
  ) %>%
  ungroup() %>%
  pivot_longer(
    factor_1:factor_5, names_to = "factor", values_to = "Contribution"
  ) %>%
  mutate(Factor = case_when(
    factor == "factor_1" ~ "Industrial Solvents",
    factor == "factor_4" ~ "Gasoline Evaporation",
    factor == "factor_3" ~ "Background Pollution",
    factor == "factor_2" ~ "Vehicle Exhaust",
    factor == "factor_5" ~ "Mixed Industry"
  ),
  Factor = factor(Factor, levels = 
                    c("Vehicle Exhaust",
                      "Gasoline Evaporation",
                      "Industrial Solvents",
                      "Background Pollution",
                      "Mixed Industry")),
  site_id = as.numeric(str_remove_all(site_id2, "site_")),
  # Replace negative contributions with zero (since these are medians
  # from bootstrapped samples, sometimes a negative can happen)
  Contribution = case_when(Contribution < 0 ~ 0, TRUE ~ Contribution)
  ) %>%
  left_join(., site_info, by = "site_id") %>%
  mutate(
    industrial_traffic = interaction(paste0(site_traffic," Traffic"),industrial_20, sep = " "),
    site_label = if_else(
      site_type == "stationary",
      paste0("**", site_id, "**"),  # Markdown bold
      as.character(site_id)
    )
  )



site_contrib_plot <- site_contribs %>%
  ggplot() + 
  aes(x = site_label, y = Contribution, fill = Factor) + 
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") + 
  coord_flip() + 
  facet_wrap(~industrial_traffic, scales = "free_y", space = "free_y") +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "#1F3A8A",  # dark blue
      "#F59E0B",  # bright orange
      "turquoise", # light blue
      "#B91C1C",  # deep red
      "#16A34A"  # bright green
    )
  ) + 
  scale_y_continuous(expand = c(0,0)) +
  labs(x =  "Site",y = "Contribution (%)") +
  paper_theme + 
  guides(fill = guide_legend(nrow = 2)) + 
  theme(
    legend.position = "bottom",
    axis.text.y = element_markdown(),
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.spacing = unit(0.5, "lines"),
    plot.margin = margin(l = 10, t = 10, r = 10, unit = "pt") 
  )



ggsave(
  file.path("results", "figures", "sites_factors_allsites.png"),
  plot = site_contrib_plot,
  width = unit(8, "in"),
  height = unit(8, "in"),
  dpi = 320
)

site_ts <- pmf_contribs %>%
  pivot_longer(
    factor_1:factor_5, names_to = "factor", values_to = "cont"
  ) %>%
  mutate(Factor = case_when(
    factor == "factor_1" ~ "Industrial Solvents",
    factor == "factor_4" ~ "Gasoline Evaporation",
    factor == "factor_3" ~ "Background Pollution",
    factor == "factor_2" ~ "Vehicle Exhaust",
    factor == "factor_5" ~ "Mixed Industry"
  ),
  Factor = factor(Factor, levels = 
                    c("Vehicle Exhaust",
                      "Gasoline Evaporation",
                      "Industrial Solvents",
                      "Background Pollution",
                      "Mixed Industry")),
  site_id = as.numeric(str_remove_all(site_id2, "site_")),
  date = lubridate::make_date(year = paste0("20",str_sub(end_date,7,8)),
                              month = str_sub(end_date,1,2),
                              day = str_sub(end_date,4,5))
  ) %>%
  left_join(., site_info, by = "site_id") %>%
  group_by(site_id, end_date) %>%
  mutate(norm_cont = 100*cont/sum(cont)) %>%
  ungroup() %>%
  mutate(
    industrial_traffic = interaction(paste0(site_traffic," Traffic"),industrial_20, sep = " "),
    site_long = paste0(site, "\n", industrial_traffic)
  ) %>%
  mutate(    site_long = factor(
    site_long,
    levels = site_long %>%
      tibble(site_long = ., industrial_traffic = industrial_traffic) %>%
      distinct() %>%
      arrange(industrial_traffic, site_long) %>%
      pull(site_long)
  ))

site_ts_plot <- site_ts %>%
  filter(site_id < 10) %>%
  ggplot() + 
  geom_line(aes(x = date, y = norm_cont, color = Factor)) + 
  scale_color_manual(
    values = c(
      "#1F3A8A",  # dark blue
      "#F59E0B",  # bright orange
      "turquoise", # light blue
      "#B91C1C",  # deep red
      "#16A34A"  # bright green
    )
  ) + 
  facet_wrap(~site_long) + 
  labs(
    x = "Date",
    y = "Relative Contribution (%)"
  ) + 
  paper_theme + 
  theme(
    axis.text.x = element_text(
      angle = 30,
    hjust = 1,
    vjust = 1
  ),
  strip.text = element_text(lineheight=0.3, size = 30)
  ) + 
  scale_x_date(date_labels = "%b %y")



ggsave(
  file.path("results", "figures", "sites_factors_timeseries.png"),
  plot = site_ts_plot,
  width = unit(8, "in"),
  height = unit(8, "in"),
  dpi = 320
)

# Main PMF Analysis: Time Series -----
pmf_timeseries <- pmf_contribs %>%
  filter(as.numeric(str_sub(site_id2, -1))<10) %>%
  group_by(end_date) %>% 
  summarize(across(starts_with("factor"), 
                   ~mean(.x))) %>%
  mutate(month = str_sub(end_date, 1, 2),
         day = str_sub(end_date, 4,5),
         year = paste0("20",str_sub(end_date, 7,8)),
         date = make_date(year, month, day)
  )
  

pmf_timeseries %>%
  select(date, starts_with("factor")) %>%
  pivot_longer(-date, names_to = "factor", values_to = "mass") %>%
  ggplot() + 
  geom_line(aes(x = date, y = mass)) + 
  facet_wrap(~factor)

# Map PMF Results ---------------------------------------------------------

# Create Site Summary Map -------------------------------------------------
## Note that this will take a little while due to landuse 
base <- ggmap(basemap) 

# Former refinery tax parcels from Open Data Philly
refinery <- st_read(here("data", "shp", "refinery.shp")) %>%
  st_transform(crs = 4326)

pmf_contribs <- read_excel("results/interim_results/pmf_results/allsites_contributions_bs_fpeak.xlsx") %>%
  filter(!is.na(site_id2))

site_mass <- pmf_contribs %>%
  group_by(site_id2) %>%
  summarize(
    factor_1 = sum(factor_1),
    factor_2 = sum(factor_2),
    factor_3 = sum(factor_3),
    factor_4 = sum(factor_4),
    factor_5 = sum(factor_5)
  ) %>%
  ungroup() %>%
  pivot_longer(
    factor_1:factor_5, names_to = "factor", values_to = "mass"
  ) %>%
  mutate(Factor = case_when(
    factor == "factor_1" ~ "Industrial Solvents",
    factor == "factor_4" ~ "Gasoline Evaporation",
    factor == "factor_3" ~ "Background Pollution",
    factor == "factor_2" ~ "Vehicle Exhaust",
    factor == "factor_5" ~ "Mixed Industry"
  ),
  Factor = factor(Factor, levels = 
                    c("Vehicle Exhaust",
                      "Gasoline Evaporation",
                      "Industrial Solvents",
                      "Background Pollution",
                      "Mixed Industry")),
  site_id = as.numeric(str_remove_all(site_id2, "site_")),
  # Replace negative contributions with zero (since these are medians
  # from bootstrapped samples, sometimes a negative can happen)
  mass = case_when(mass < 0 ~ 0, TRUE ~ mass)
  ) %>%
  left_join(., site_info, by = "site_id")  %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) 


sources <- st_read(here("data","shp","thriveair_potential_sources.shp")) %>%
  janitor::clean_names() %>%
  mutate(source_type = factor(case_when(
    symbol_id == 0 ~ "Oil & Gas",
    symbol_id == 1 ~ "Auto Salvage",
    symbol_id == 2 ~ "Concrete & Asphalt",
    symbol_id == 3 ~ "Recycling",
    symbol_id == 4 ~ "Auto Repair",
    symbol_id == 5 ~ "Metal Fabrication",
    symbol_id == 6 ~ "Logistics",
    symbol_id == 7 ~ "Dry Cleaning"
  ), levels = c("Oil & Gas","Auto Repair","Auto Salvage",
                "Recycling","Metal Fabrication","Logistics",
                "Concrete & Asphalt","Dry Cleaning")
  )) %>%
  st_transform(crs = 4326)


site_contribs_sf <- site_contribs %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) 

# fake legend for Former Refinery
fake_refinery <- data.frame(x = 1, y = 1)

contrib_map <- ggmap(basemap) + 
  # Dummy refinery point to force legend
  geom_point(
    data = fake_refinery,
    aes(x = x, y = y, fill = "Former Refinery"),
    shape = 22, size = 4, color = NA, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Former Refinery" = "grey80"),
    guide = guide_legend(
      override.aes = list(
        shape = 22,          # square
        fill = "grey80",
        pattern = "stripe",  # if using ggpattern
        pattern_spacing = 0.01,
        pattern_angle = 45
      ),
      order = 2
    )
  ) +
  
  # Former Refinery site
  geom_sf_pattern(data = refinery, 
                  aes(fill = "Former Refinery"), #dummy variable for legend
                  inherit.aes = FALSE, alpha = 0.4,
                  pattern = "stripe", pattern_spacing = 0.01,
                  pattern_alpha = 0.5) + 
  new_scale_fill() +
  # Porential sources
  geom_sf(data = sources, inherit.aes = FALSE,
          aes(fill = source_type),
          shape = 21, color = "black", stroke = 0.04, size = 2) + 
  # Monitor site points
  geom_sf(data = site_contribs_sf, inherit.aes = FALSE,
          aes(size = Contribution),
          fill = "black", stroke = 0.4) + 
  # Site ID annotations with shadow
  # Site ID annotations - white text
  # geom_sf_text(
  #   data = sites_sf,
  #   aes(label = site_id),
  #   size = 11,
  #   fontface = "bold",
  #   color = "white",
  #   inherit.aes = FALSE
  # ) +
  # North arrow
  # annotation_north_arrow(
  #   location = "br",  # bottom-right
  #   height = unit(1.5, "cm"),
  #   width = unit(1.5, "cm"),
  #   which_north = "true",
  #   style = north_arrow_orienteering(text_size = 32,
  #                                    line_width = 1,
  #                                    text_face = "bold")
  # ) +
  coord_zoom(1.15) + 
  labs(
    y = NULL,
    x = NULL
    # title = "Monitoring Sites",
    # subtitle = "Land Use and Traffic Density Considerations"
  ) + 
  paper_theme + 
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.box.spacing = unit(0.3, "cm"), 
    legend.spacing = unit(0.3, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 2),  
    legend.key.size = unit(0.8, "cm"),
    plot.margin = margin(3,3,3,3, "pt") 
  ) + 
  facet_wrap(~Factor)

site_key <- contribs %>%
  st_drop_geometry() %>%
  select(site_id, site) %>%
  mutate(site= iconv(site, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(site = str_wrap(site, width = 21)) %>%
  distinct() %>%
  rename(ID = site_id, Site = site) %>%
  arrange(ID)

key_table <- tableGrob(
  site_key,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontsize = 32, hjust = 0, x = 0.05, lineheight = 0.3),  # left-justify, small left margin
      padding = unit(c(4, 2), "mm")  
    ),
    colhead = list(
      fg_params = list(fontsize = 32, fontface = "bold", hjust = 0, x = 0.05)
    )
  )
)


location_map <- ggmap(basemap) + 
  # Dummy refinery point to force legend
  geom_point(
    data = fake_refinery,
    aes(x = x, y = y, fill = "Former Refinery"),
    shape = 22, size = 4, color = NA, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Former Refinery" = "grey80"),
    guide = guide_legend(
      override.aes = list(
        shape = 22,          # square
        fill = "grey80",
        pattern = "stripe",  # if using ggpattern
        pattern_spacing = 0.01,
        pattern_angle = 45
      ),
      order = 2
    )
  ) +
  # Former Refinery site
  geom_sf_pattern(data = refinery, 
                  aes(fill = "Former Refinery"), #dummy variable for legend
                  inherit.aes = FALSE, alpha = 0.4,
                  pattern = "stripe", pattern_spacing = 0.01,
                  pattern_alpha = 0.5) + 
  new_scale_fill() +
  # Porential sources
  geom_sf(data = sources, inherit.aes = FALSE,
          aes(fill = source_type),
          shape = 21, color = "black", stroke = 0.4, size = 7) + 
  # Monitor site points
  geom_sf(data = site_contribs_sf, inherit.aes = FALSE,
          fill = "black", stroke = 0.4, size = 7) + 
  # Site ID annotations with shadow
  geom_sf_text(
    data = sites_sf,
    aes(label = site_id),
    size = 11,
    fontface = "bold",
    color = "white",
    inherit.aes = FALSE
  ) +
#  North arrow
  annotation_north_arrow(
    location = "br",  # bottom-right
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    which_north = "true",
    style = north_arrow_orienteering(text_size = 32,
                                     line_width = 1,
                                     text_face = "bold")
  ) +
  coord_zoom(1.15) + 
  labs(
    y = NULL,
    x = NULL
    # title = "Monitoring Sites",
    # subtitle = "Land Use and Traffic Density Considerations"
  ) + 
  paper_theme + 
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.box.spacing = unit(0.3, "cm"), 
    legend.spacing = unit(0.3, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 2),  
    legend.key.size = unit(0.8, "cm"),
    plot.margin = margin(3,3,3,3, "pt") 
  ) 

combo_plot <- wrap_elements(key_table) + location_map + plot_layout(widths = c(1,3))  
# plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0),
#                               panel.spacing = unit(0, "pt") ))  # reduce overall margins

ggsave(
  filename = here("results", "figures", "sources_sites.png"),
  plot = combo_plot,
  width = unit(9, "in"),
  height = unit(8, "in")
  
)

ggsave(
  filename = here("results", "figures", "pmf_and_sources.png"),
  plot = contrib_map,
  width = unit(9, "in"),
  height = unit(8, "in")
  
)

