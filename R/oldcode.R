
# Make time series of PCA results
# Not sure this makes sense since we explicitly want spatial-only relationships

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
        legend.title = element_text(size = 40, lineheight = 0.5),
        legend.key.height = unit(4, "pt")
        
      ) 
    
    
    ggsave(
      plot = comp_ts,
      filename = here(filename = here("results", "figures", paste0(object_name,"_comp_",comp_number,"_score_timeseries.png"))),
      height = 8, width = 8, units = "in",
      dpi = 320
      
    )
    
  }
  
  
  
  
}