library(readxl)
library(tidyverse)
library(zoo)
library(ggspatial)
library(ggrepel)
library(ggmap)
library(showtext)
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


# Generate Basemap --------------------------------------------------------
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
        size = 4,
        stroke = 0.5,
        color = "black"
      ) +
      scale_fill_viridis_c(option = "plasma") +
      coord_sf(expand = FALSE) +
      labs(
        fill = paste0("Average\n", ratio_text),
        title = paste("Site-level average", ratio_text, "ratio"),
        subtitle = season_val,
        x = NULL,
        y = NULL
      )
  } else {
    p <- ggmap(basemap) +
      geom_sf(
        data = df_avg,
        inherit.aes = FALSE,
        aes(fill = ratio_val, shape = site_type),
        size = 4,
        stroke = 0.5,
        color = "black"
      ) +
      scale_shape_manual(
        values = c("stationary" = 21, "rotating" = 22),
        labels = c("stationary" = "Weekly Sites", "rotating" = "Community Sites")
      ) +
      scale_fill_viridis_c(option = "plasma") +
      coord_sf(expand = FALSE) +
      labs(
        fill = paste0("Average\n", ratio_text),
        shape = "Site Type",
        title = paste("Site-level average", ratio_text, "ratio"),
        subtitle = season_val,
        x = NULL,
        y = NULL
      )
  }
  
  p <- p + 
    paper_theme +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold")
    )
  
  return(p)
  
}

map_ratio(vocs, "tb_ratio")
map_ratio(vocs, "tb_ratio", "Summer", rotating = 1)


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
