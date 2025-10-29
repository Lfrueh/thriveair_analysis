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

source("R/plot_theme.R")  

# Codebook ----
codebook <- read_excel('data/codebook.xlsx')
voc_vars <- codebook %>% 
  #Ignore total BTEX and total Xylenes in favor of individual ones.
  filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)


#Basemap 
coords <- vocs %>% filter(!is.na(lat)) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  st_coordinates()

center <- c(mean(coords[, "X"]), mean(coords[, "Y"]))

#Zoom in bounds for plotting
xmin <- min(coords[, "X"] - 0.008)
xmax <- max(coords[, "X"] + 0.005)
ymin <- min(coords[, "Y"] - 0.005)
ymax <- max(coords[, "Y"] + 0.005)

basemap <- get_googlemap(center, zoom = 13, maptype = "roadmap",
                         # Turn off unneeded labels
                         style = list(
                           c(feature = "poi", element = "labels.text.fill", visibility = "off"),
                           c(feature = "poi", element = "labels.text.stroke", visibility = "off"),
                           c(feature = "poi", element = "labels.icon", visibility = "off")
                         ))
ggmap(basemap)


# Functions -----
## Plot Loadings ----
plot_loadings <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  detect_text <- if(str_detect(object_name, "lowdetect")) "Includes low-detect compounds" else NULL
  weight_text <- if(str_detect(object_name, "_w")) "Weighted" else NULL
  cluster_text <- if (str_detect(object_name, "wk_")){
    if (str_detect(object_name, "_glmer")){
      "Clustered (Gamma GLMM, log)"
    } else {
      "Clustered (LMM)"
    }
  } else NULL
  
  detail_text <- paste(compact(list(weight_text, cluster_text, detect_text)), collapse = ", ")
  
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
      legend.key.height = unit(2, "pt"),
      legend.key.width = unit(5, "pt")
    )
  
  ggsave(
    filename = here("results", "figures", paste0(object_name,".png")),
    plot = plot,
    dpi = 320,
    units = "in",
    width = 4,
    height = 4
  )
  
}



score_timeseries <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  detect_text <- if(str_detect(object_name, "lowdetect")) "Includes low-detect compounds" else NULL
  weight_text <- if(str_detect(object_name, "_w")) "Weighted" else NULL
  cluster_text <- if (str_detect(object_name, "wk_")){
    if (str_detect(object_name, "_glmer")){
      "Clustered (Gamma GLMM, log)"
    } else {
      "Clustered (LMM)"
    }
  } else NULL
  
  detail_text <- paste(compact(list(weight_text, cluster_text, detect_text)), collapse = ", ")
  
  df <- result_object$scores
  
  comp_names <- df %>%
    select(starts_with("Dim.")) %>%
    names()

  # Will save one plot for each component
  for (comp in comp_names){
    comp_number <- str_sub(comp, -1, -1)  
    
    site_avg <- df %>%
      group_by(site) %>%
      summarize(avg_val = mean(.data[[comp]], na.rm = TRUE))  %>%
      arrange(avg_val)
    
    df <- df %>% 
      mutate(site = factor(site, levels = site_avg$site)) 
    
    dfs <- df %>% filter(site_type == "stationary")
    
    dfr1 <- df %>% filter(site_type == "rotating", between(start_date, as.Date("2023-12-06"), as.Date("2024-02-14")))
      
    dfr2 <- df %>% filter(site_type == "rotating", between(start_date, as.Date("2023-08-09"), as.Date("2023-10-25")))
      
    # Assign colors to sites based on avg_val using custom gradient
    # Interpolate colors from low -> mid -> high
    custom_palette <- colorRampPalette(c("#086788", "darkgrey", "#DD1C1A"))
    colors <- custom_palette(nrow(site_avg))
    
    names(colors) <- site_avg$site
      
    comp_ts <- ggplot() + 
      geom_line(data = dfs, aes(x = start_date, y = .data[[comp]], color = site, group = site),
                linewidth = 0.1) +
      geom_line(data = dfr1, aes(x = start_date, y = .data[[comp]], color = site, group = site, 
                                 show.legend = FALSE),
                linewidth = 0.3, linetype = 2) +
      geom_line(data = dfr2, aes(x = start_date, y = .data[[comp]], color = site, group = site,
                                 show.legend = FALSE),
                linewidth = 0.3, linetype = 2) +
      labs(
        title = paste0("Component ", comp_number, " Score Over Time"),
        subtitle = detail_text,
        x = "Sample Start Date",
        y = paste0("Comp. ",comp_number," Score"),
        color = paste0("Average Comp. ",comp_number," Score")
      ) +
      scale_color_manual(values = colors) +
      paper_theme + 
      theme(
        plot.background = element_rect("white"),
        legend.title = element_text(size = 20, lineheight = 0.5),
        legend.key.height = unit(2, "pt")
        
      ) 
    
    
    ggsave(
      plot = comp_ts,
      filename = here(filename = here("results", "figures", paste0(object_name,"_comp_",comp_number,"_score_timeseries.png"))),
      height = 4, width = 4, units = "in",
      dpi = 320
      
    )
    
  }
  
  
  
  
}

map_avg_scores <- function(result_object, object_name, weighted = NULL, cluster = NULL){
  
  detect_text <- if(str_detect(object_name, "lowdetect")) "Includes low-detect compounds" else NULL
  weight_text <- if(str_detect(object_name, "_w")) "Weighted" else NULL
  cluster_text <- if (str_detect(object_name, "wk_")){
    if (str_detect(object_name, "_glmer")){
      "Clustered (Gamma GLMM, log)"
    } else {
      "Clustered (LMM)"
    }
  } else NULL
  
  detail_text <- paste(compact(list(weight_text, cluster_text, detect_text)), collapse = ", ")
  
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
  
  print(comp_names)
  
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
    geom_sf(data = df, inherit.aes = FALSE, color="black",
            size = 4.5, aes(shape = site_type)) + 
    # In-range data
    geom_sf(data = df, 
            inherit.aes = FALSE, aes(color = .data[[comp]],
                                                         
                                     shape = site_type),
            size = 4) + 
    coord_sf(expand = FALSE) + 
    scale_color_gradient2(low = "#086788", mid = "darkgrey",high = "#DD1C1A",
                          midpoint = q_med) + 
                         # limits = c(q_low, q_high),
                        #  oob = scales::squish) + 
    scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) + 
    scale_shape_manual(
      values = c("stationary" = 16, "rotating" = 18),
      labels = c("stationary" = "Stationary", "rotating" = "Rotating")
    ) + 
    labs(
      title = paste0("Average Component ", comp_number, " Score"),
      subtitle = detail_text,
      x = NULL,
      y = NULL,
      color = paste0("Comp. ",comp_number," Score"),
      shape = "Site Type"
    ) +
    annotation_north_arrow(
      location = "tl", which_north = "grid",
      height = unit(0.3, "in"), width = unit(0.3, "in"),
      style = north_arrow_orienteering(text_size = 20)
    ) +
    annotation_scale(
      data = df,
      unit_category = "imperial",
      location = "br",
      width_hint = 0.5,
      text_cex = 2,
      height = unit(0.1, "cm")
    ) +
    paper_theme + 
    theme(
      plot.background = element_rect("white"),
   #   axis.text.x = element_blank(),
   #   axis.text.y = element_blank(),
      legend.title = element_text(size = 20, lineheight = 0.5)
    ) 
  
  
  ggsave(
    plot = comp_plot,
    filename = here(filename = here("results", "figures", paste0(object_name,"_comp_",comp_number,"_score_map.png"))),
    height = 4, width = 4, units = "in",
    dpi = 320
    
  )
  
  }
  
  
}

# Get results ----
resultpath <- here("results", "pca_results")
results <- list.files(resultpath, full.names = TRUE)

results_list <- lapply(results,  readRDS)
names(results_list) <- tools::file_path_sans_ext(list.files(resultpath))

# Plot loadings ----
for (r in names(results_list)) {
  plot_loadings(result_object = results_list[[r]], object_name = r)
}

# Map average scores ----
for (r in names(results_list)) {
  map_avg_scores(result_object = results_list[[r]], object_name = r)
}

# TO FIX BEFORE PUBLICATION:
# time series probably not even worth it to look at time series since we only sampled for a week or two here and there.
# Time series of scores ----
for (r in names(results_list[1])) {
  score_timeseries(result_object = results_list[[r]], object_name = r)
}


