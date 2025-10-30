library(readxl)
library(tidyverse)
library(zoo)
library(patchwork)
library(sf)

########################################################
# Purpose of this code:
# Calculate and visualize BTEX ratios 
########################################################

source("R/00_plot_theme.R")  

# Get Data ----------------------------------------------------------------

## VOCs ----
vocs_raw <- read_csv(here("data", "clean", "dat_mgm3.csv"), col_select = -1) %>%
  filter(!is.na(site_id) & !is.na(site)) 

# Create BTEX Ratios of interest:
# toluene / benzene ratio
# (m+p) xylenes / ethylbenzene ratio
vocs <- vocs_raw %>% 
  mutate(tb_ratio = toluene/benzene,
         xe_ratio = mpxylene/etbenz)

ratio_means_date <- vocs %>%
  group_by(end_date = as.Date(end_date)) %>%
  summarize(across(ends_with("_ratio"), ~mean(., na.rm = TRUE)),
            .groups = "drop") 

ratio_means_site <- vocs %>%
  group_by(site) %>%
  summarize(across(ends_with("_ratio"), ~mean(., na.rm = TRUE)),
            .groups = "drop") 

## Codebook ----
codebook <- read_excel(here("data", "codebook.xlsx"))

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)






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
  
  # Make ratio labels
  ratio_text <- if (ratio == "tb_ratio") {
    expression("Toluene : Benzene")
  } else if (ratio == "xe_ratio") {
    expression("m,p-Xylene : Ethylbenzene")
  } else {
    ratio  # fallback, just print the ratio string
  }
  
  # Plot - build it differently based on rotating
  if (is.null(rotating)) {
    p <- ggmap(basemap) +
      geom_sf(
        data = df_avg,
        inherit.aes = FALSE,
        aes(fill = ratio_val),
        shape = 21,
        size = 1.5,
        stroke = 0.1,
        color = "black"
      ) +
      coord_sf(expand = FALSE) +
      labs(
        fill = bquote("Average " ~ .(ratio_text))
      )
  } else {
    p <- ggmap(basemap) +
      geom_sf(
        data = df_avg,
        inherit.aes = FALSE,
        aes(fill = ratio_val, shape = site_type),
        size = 1.5,
        stroke = 0.1,
        color = "black"
      ) +
      scale_shape_manual(
        values = c("stationary" = 21, "rotating" = 22),
        labels = c("stationary" = "Weekly", "rotating" = "Community")
      ) +
      coord_sf(expand = FALSE) +
      labs(
        fill = bquote("Average " ~ .(ratio_text)),
        shape = "Site Type"
      ) 
  }
  
  p <- p + 
    lock_to_basemap(basemap) + 
    paper_theme +
    labs(
      x = NULL,
      y = NULL
    ) + 
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      legend.spacing.x = unit(1, "pt")  # reduce gap between boxes
      
    ) + 
    guides(
      fill = guide_colorbar(
        barwidth = unit(3, "cm"),
        barheight = unit(0.25, "cm"),
        label.vjust = 1.5 
      ),
      shape = guide_legend(
        override.aes = list(size = 2), # smaller points in legend
        direction = "horizontal",      # horizontal legend
        label.position = "right",
        keywidth = unit(0.2, "cm"),
        keyheight = unit(0.3, "cm")
      )
    ) 
  
  return(p)
  
}

p_all <- map_ratio(vocs, "tb_ratio") +
  labs(subtitle = "Year-round") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),  # keep y labels on the left plot
  )

p_summer <- map_ratio(vocs, "tb_ratio", "Summer", rotating = 1) +
  labs(subtitle = "Summer") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_blank(), # remove y labels
    axis.ticks.y = element_blank()
  )

p_winter <- map_ratio(vocs, "tb_ratio", "Winter", rotating = 1) +
  labs(subtitle = "Winter") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  annotation_scale(
    location = "bl",  # bottom-left
    width_hint = 0.3,
    text_cex = 0.6
  ) +
  annotation_north_arrow(
    location = "tl",  # top-left
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  )


combined_plot <- (p_all | p_summer | p_winter) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot <- combined_plot + 
  plot_annotation(
    title = "Toluene : Benzene Ratio",
    theme = theme(
      plot.title = element_text(
        size = 20, 
        face = "bold",
        hjust = 0.5
      )
    )
  )


gg_record(
  device = "png",
  width = 4,
  height = 4,
  unit = "in"
)

final_plot

## To do: add save function
## To do: save time series plots.


# Time Series Plots -------------------------------------------------------
## To save
vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = xe_ratio, color = site))






## Old Code
# Average over time
ratio_means %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = tb_ratio))


# Average over time
ratio_means %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = xe_ratio)) 

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = tb_ratio, color = site), se = FALSE)

#
#High X/E ratio → fresher emissions, limited photochemical processing.
#Low X/E ratio → aged air mass, longer transport or higher OH exposure.
#If you measure X/E ≈ 3–4 → consistent with fresh vehicular emissions.
#If you measure X/E ≈ 1–2 → consistent with refinery or petrochemical sources.
#If you measure X/E < 1 → strongly suggests photochemical aging of an initial higher ratio source.
vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = xe_ratio, color = site))

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = pxe_ratio, color = site), se = FALSE)

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = oxe_ratio, color = site), se = FALSE)

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = tb_ratio, color = site), se = FALSE)

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = eb_ratio, color = site), se = FALSE)

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = toluene, color = site), se = FALSE)


vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = etbenz, color = site), se = FALSE)

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = xylenes, color = site), se = FALSE)

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_smooth(aes(x = as.Date(end_date), y = benzene, color = site), se = FALSE) + 
  geom_smooth(aes(x = as.Date(end_date), y = toluene, color = site), se = FALSE, linetype = 2) + 
  geom_smooth(aes(x = as.Date(end_date), y = etbenz, color = site), se = FALSE, linetype = 3) + 
  geom_smooth(aes(x = as.Date(end_date), y = xylenes, color = site), se = FALSE, linetype = 4)

vocs %>% 
  group_by(site) %>%
  select(site, site_type, end_date, benzene, toluene, etbenz, xylenes) %>%
  filter(site_type == "stationary") %>%
  pivot_longer(benzene:xylenes, names_to = "btex", values_to = "concentration") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = concentration, color = site), 
              linewidth = 0.5) + 
  facet_wrap(~btex, nrow = 4, scales = "free_y")


vocs %>% 
  group_by(site) %>%
  select(site, site_type, end_date, ends_with("_ratio")) %>%
  select(-xe_ratio) %>%
  filter(site_type == "stationary") %>%
  pivot_longer(ends_with("_ratio"), names_to = "ratio_type", values_to = "value") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = value, color = site), 
            linewidth = 0.5) + 
  facet_wrap(~ratio_type, nrow = 4, scales = "free_y") + 
  labs(
    x = NULL,
    y = NULL
  ) + 
  paper_theme
  



vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = pxe_ratio, color = site))

vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = oxe_ratio, color = site))


vocs %>% 
  filter(site == "28th & Passyunk") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = mpxylene),
            color = "red") + 
  geom_line(aes(x = as.Date(end_date), y = etbenz),
            color = "blue") +
  geom_line(aes(x = as.Date(end_date), y = pxe_ratio),
            color = "purple")
