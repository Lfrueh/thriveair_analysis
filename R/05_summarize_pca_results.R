library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(patchwork)
library(cowplot)
library(ggspatial)
library(ggrepel)
library(ggmap)
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



# Functions ---------------------------------------------------------------

## Plot Loadings ----
plot_loadings <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  detect_text <- if(str_detect(object_name, "lowdetect")) "Includes low-detect compounds" else NULL
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
    facet_wrap(~dimension) + 
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
  if (str_detect(object_name, "sens")){
    outpath <- here("results","supplemental","figures")
  } else {
    outpath <- here("results", "figures")
  }
  
  
  ggsave(
    filename = here(outpath, paste0(object_name,".png")),
    plot = plot,
    dpi = 320,
    units = "in",
    width = 8,
    height = 8
  )
  
}

map_avg_scores <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  detect_text <- if(str_detect(object_name, "lowdetect")) "Includes low-detect compounds" else NULL
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
    site = factor(site, levels = c(
      main_results %>% filter(site_type == "stationary") %>% pull(site) %>% unique(),
      main_results %>% filter(site_type == "rotating") %>% pull(site) %>% unique()
    ))
  ) %>%
  pivot_longer(Dim.1:Dim.4, names_to = "comp", values_to = "score") %>%
  mutate(comp = str_replace(comp, "Dim.", "Component ")) %>%
  ggplot(aes(x = site, y = score, fill = site_type)) +
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
  width = unit(8, "in"),
  height = unit(8, "in"),
  dpi = 320
)

# Plot Loadings -----------------------------------------------------------
for (r in names(results_list)) {
  plot_loadings(result_object = results_list[[r]], object_name = r)
}


# Map Average Scores ------------------------------------------------------
for (r in names(results_list)) {
  map_avg_scores(result_object = results_list[[r]], object_name = r)
}



# Time Series Plot + Map for Main Analysis -------------------------------

colors <- c(
  "#E41A1C",  # red
  "#2C3E73",  # dark blue
  "#4DAF4A",  # green
  "#984EA3",  # purple
  "#FF7F00",  # orange
  "#00A0B0",  # dark cyan
  "#A65628",  # brown
  "#F781BF",  # pink
  "#999999"   # gray
)



pca_ts <- main_results %>% 
  pivot_longer(cols = Dim.1:Dim.4, names_to = "comp", values_to = "score") %>%
  mutate(comp = str_replace(comp, "Dim.", "Component ")) %>%
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = score, color = site)) + 
  labs(
    x = "Date",
    y = "Score"
  ) + 
  facet_wrap(~comp, scales = "free_y") + 
  scale_color_manual(values = colors) +
  paper_theme + 
  theme(legend.position = "none")



vocs_sf <- read_csv(here("data", "clean", "dat_mgm3.csv")) %>%
  filter(site_type == "stationary") %>%
  select(site, long, lat) %>%
  distinct() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

site_map <- ggmap(basemap) +
  geom_sf(
    data = vocs_sf,
    inherit.aes = FALSE,
    aes(fill = site),
    shape = 21,
    size = 5,
    stroke = 0.5,
    color = "black"
  ) +
  coord_zoom(1.15) + 
  scale_fill_manual(values = colors) + 
  labs(
    x = NULL,
    y = NULL,
    fill = "Site"
  ) + 
  paper_theme + 
  annotation_north_arrow(
    location = "br",  # bottom-right
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    which_north = "true",
    style = north_arrow_orienteering(text_size = 16,
                                     line_width = 0.5,
                                     text_face = "bold")
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


pca_ts_map <- pca_ts + site_map + guide_area() + 
  plot_layout(
    design = "aa \n bc",
    guides = "collect",
    widths = c(1.3, 1)
  )

ggsave(here("results","figures","pca_main_timeseries_stationary.png"),
       pca_ts_map,
       width = unit(8, "in"),
       height = unit(8, "in"))

