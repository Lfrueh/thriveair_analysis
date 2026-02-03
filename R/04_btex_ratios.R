library(readxl)
library(tidyverse)
library(zoo)
library(patchwork)
library(sf)
library(RColorBrewer)
library(here)

########################################################
# Purpose of this code:
# Calculate and visualize BTEX ratios 
########################################################

source("R/00_plot_theme.R")  

# Get Data ----------------------------------------------------------------

## VOCs ----
vocs_raw <- read_csv(here("data", "clean", "dat_mgm3.csv")) %>%
  filter(!is.na(site_id) & !is.na(site)) 

# Create BTEX Ratios of interest:
# toluene / benzene ratio
# (m+p) xylenes / ethylbenzene ratio
vocs <- vocs_raw %>% 
  mutate(tb_ratio = toluene/benzene,
         xe_ratio = m_p_xylene_2/ethylbenzene)

ratio_means_date <- vocs %>%
  group_by(end_date = as.Date(end_date)) %>%
  summarize(across(ends_with("_ratio"), ~mean(., na.rm = TRUE)),
            .groups = "drop") 



## Codebook ----
codebook <- read_excel(here("data", "codebook.xlsx"))

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)


# Table of BTEX Ratios ----------------------------------------------------

## Ratios for all sites
ratio_means_site <- vocs %>%
  group_by(site_type, site) %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f (%.2f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))
    ),
    .groups = "drop"
  )

# Save to supplemental
write_excel_csv(ratio_means_site, here("results", "supplemental", "tables", "ratios_by_site.csv"))

## Ratios by site type
ratio_means_sitetype <- vocs %>%
  group_by(site_type) %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f (%.2f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_excel_csv(ratio_means_sitetype, here("results", "supplemental", "tables", "ratios_by_site_type.csv"))

## Ratios overall
ratio_means_overall <- vocs %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f (%.2f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))
    )
  )

write_excel_csv(ratio_means_overall, here("results", "supplemental", "tables", "ratios_overall.csv"))

## Ratios by land-use type
ratio_means_landuse <- vocs %>%
  group_by(industrial_20, site_traffic) %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f (%.2f)", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_excel_csv(ratio_means_landuse, here("results", "tables", "btex_ratio_landuse.csv"))



# Ratios by Land Use Type -------------------------------------------------

# Maps --------------------------------------------------------------------
# Function to map ratios
map_ratio <- function(data, ratio, season_val = NULL, rotating = NULL){
  df <- data %>%
    mutate(site_type = factor(site_type, levels = c("stationary", "rotating"))) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  # Filter data based on function inputs
  if (!is.null(season_val)){
    df <- df %>%
      filter(season == season_val)
  } 
  
  if (is.null(rotating)){
    df <- df %>%
      filter(site_type == "stationary")
  }
  
  # Compute average ratio per site
  df_avg <- df %>%
    group_by(site, site_type, geometry) %>%
    summarize(
      ratio_val = mean(.data[[ratio]], na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # Plot - build it differently based on rotating
  if (is.null(rotating)) {
    p <- ggmap(basemap) +
      geom_sf(
        data = df_avg,
        inherit.aes = FALSE,
        aes(fill = ratio_val),
        shape = 21,
        size = 3,
        stroke = 0.2,
        color = "black"
      ) +
      coord_sf(expand = FALSE) +
      labs(
        fill = "Mean"
      ) 
      
  } else {
    p <- ggmap(basemap) +
      geom_sf(
        data = df_avg,
        inherit.aes = FALSE,
        aes(fill = ratio_val, shape = site_type),
        size = 3,
        stroke = 0.4,
        color = "black"
      ) +
      scale_shape_manual(
        values = c("stationary" = 21, "rotating" = 22),
        labels = c("stationary" = "Weekly", "rotating" = "Community")
      ) +
      coord_sf(expand = FALSE) +
      labs(
        fill = "Mean",
        shape = "Site Type"
      ) 
  }
  
  p <- p + 
    lock_to_basemap(basemap) + 
    coord_zoom(1.15) + 
    paper_theme +
    labs(
      x = NULL,
      y = NULL
    ) + 
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      legend.spacing.x = unit(2, "pt")  # reduce gap between boxes
      
    ) + 
    guides(
      fill = guide_colorbar(direction = "horizontal",
                            barheight = unit(0.8, "cm"),
                            label.vjust = -1.2),
      shape = guide_legend(
        override.aes = list(size = 2),
        direction = "horizontal",
        label.position = "right"
      )
    )
  
  
  return(p)
  
}



combine_ratio_maps <- function(ratio_val){
  
  if(ratio_val == "tb_ratio"){
    scale_lims <- c(1,3)
  } else{
    scale_lims <- c(5,8)
  }

  
  p_all <- map_ratio(vocs, ratio_val) +
    labs(subtitle = "Year-round") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
      axis.text.y = element_text(size = 20),  # keep y labels on the left plot
    ) +
    scale_fill_viridis_c(limits = scale_lims, option = "turbo")
  
  p_summer <- map_ratio(vocs, ratio_val, "Summer", rotating = 1) +
    labs(subtitle = "Summer") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(), # remove y labels
      axis.ticks.y = element_blank()
    ) +
    scale_fill_viridis_c(limits = scale_lims, option = "turbo")
  
  p_winter <- map_ratio(vocs, ratio_val, "Winter", rotating = 1) +
    labs(subtitle = "Winter") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) + 
    annotation_north_arrow(
      location = "br",  # bottom-right
      height = unit(0.5, "cm"),
      width = unit(0.5, "cm"),
      which_north = "true",
      style = north_arrow_orienteering(text_size = 20,
                                       line_width = 1,
                                       text_face = "bold")
    ) +
    scale_fill_viridis_c(limits = scale_lims, option = "turbo")
  
  
  combined_plot <- (p_all | p_summer | p_winter) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.box.just = "center")
  
  
  # Make ratio labels
   ratio_text <- if (ratio_val == "tb_ratio") {
     expression("Toluene : Benzene")
   } else if (ratio_val == "xe_ratio") {
     expression("m,p-Xylene : Ethylbenzene")
   } else {
     ratio_val  # fallback, just print the ratio string
   }
  
   final_plot <- combined_plot + 
     plot_annotation(
       title = ratio_text,
       theme = theme(
         plot.title = element_text(
           size = 44, 
           hjust = 0.5
         )
       )
     ) 
  
   
   ggsave(
     filename = here("results", "supplemental","figures", paste0(ratio_val,"_map.png")),
     plot = print(final_plot),
     device = "png",
     width = 8,
     height = 8,
     unit = "in"
     
   )
   
  
}

combine_ratio_maps("tb_ratio") 
combine_ratio_maps("xe_ratio") 

# Time Series Plots -------------------------------------------------------

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



tb_ts <- vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = tb_ratio, color = site)) + 
  labs(
    x = "Date",
    y = "T/B Ratio Ratio",
    subtitle = "Toluene:Bezene Ratio"
  ) + 
  scale_color_manual(values = colors) +
  paper_theme + 
  theme(legend.position = "none")

xe_ts <- vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = xe_ratio, color = site)) + 
  labs(
    x = "Date",
    y = "X/E Ratio",
    subtitle = "(m+p)-Xylenes:Ethylbenzene Ratio"
  ) + 
  scale_color_manual(values = colors) +
  paper_theme +
  theme(legend.position = "none")

vocs_sf <- vocs %>%
  filter(site_type == "stationary") %>%
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
    fill = NULL
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
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.5, -0.01),  
    legend.justification = c(0.5, 1)   
  )  +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))




ratio_ts_map <- (tb_ts/xe_ts + plot_layout(guides = "collect")|site_map)  +
  plot_layout(widths = c(1.5, 1))  # TS plots twice as wide as map



ggsave(here("results", "supplemental", "figures", "ratio_ts_map.png"),
       ratio_ts_map,
       width = unit(8, "in"),
       height = unit(8, "in"))




# Ratios for Supplement ---------------------------------------------------
supp_table <- vocs %>%
  group_by(site_type, industrial_20) %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~round(mean(.x, na.rm = TRUE),2)
    ),
    .groups = "drop"
  ) %>%
  mutate(xylenes = o_xylene + m_p_xylene_2, 
      ratio_summary =  
           paste0(round(benzene/ethylbenzene,1)," : ",
                  round(toluene/ethylbenzene, 1)," : ",
                  "1", " : ",
                  round(xylenes/ethylbenzene, 1))
  ) %>%
  arrange(site_type, desc(industrial_20))

write_csv(supp_table, here("results","supplemental","tables","btex_ratio_summary.csv"))
