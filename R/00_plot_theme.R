library(showtext)
library(sysfonts)
library(tidyverse)
library(ggspatial)
library(ggrepel)
library(ggmap)
library(showtext)
library(sf)

########################################################
# Purpose of this code:
# Create a standardized theme for all plots, tables, maps
########################################################



# GGPlot Theme ------------------------------------------------------------


# Add and register Open Sans
font_add_google(name = "Open Sans", family = "Open Sans")
# Automatically use showtext for new plots
showtext_auto(enable = TRUE)

options(
  ggplot2.continuous.colour = function(...) ggplot2::scale_colour_viridis_c(option = "plasma", ...),
  ggplot2.continuous.fill   = function(...) ggplot2::scale_fill_viridis_c(option = "plasma", ...)
)

# Define consistent theme for plots and maps -----------------
## GGPlot themes -----
# Note that this is optimized for an 8inch by 8inch ggsave.
paper_theme <- theme_bw(base_family = "opensans") + # Base theme
  theme(
    # Plot & strip background
    plot.background = element_rect("white"),
    strip.background = element_rect(color = "black", linewidth = 0.2, fill = "gray90"),
    panel.border = element_rect(color = "black",  linewidth = 0.2, fill = NA),
    # Grid & Tick marks
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.2),  
    # Text options
    text = element_text(family = "Open Sans"),
    ## Axis title
    axis.title.x = element_text(family = "Open Sans", size = 40),
    axis.title.y = element_text(family = "Open Sans", size = 40),
    axis.text = element_text(family = "Open Sans", size = 32),
    ## Plot titles
    plot.title = element_text(family = "Open Sans", size = 56),
    plot.subtitle = element_text(family = "Open Sans", size = 40),
    strip.text = element_text(family = "Open Sans", size = 40,
                              margin = margin(t = 2, b = 2)),
    ## Legend options
    legend.title = element_text(family = "Open Sans", size = 40),
    legend.text = element_text(family = "Open Sans", size = 32,
                               margin = margin(l = 1))
  )



# Basemap and Map Theme ---------------------------------------------------
 vocs_raw <- read_csv(here("data", "clean", "dat_mgm3.csv")) %>%
   filter(!is.na(site_id) & !is.na(site)) 
 
# Generate Basemap --------------------------------------------------------
 coords <- readRDS("data/raw/coords.rds")
 
 center <- c(mean(coords[, "X"]), mean(coords[, "Y"]))
 
 #Zoom in bounds for plotting
 xmin <- min(coords[, "X"])
 xmax <- max(coords[, "X"])
 ymin <- min(coords[, "Y"])
 ymax <- max(coords[, "Y"])
 
 basemap <- get_googlemap(center, zoom = 13, maptype = "roadmap",
                          # Turn off unneeded labels
                          style = list(
                            c(feature = "poi", element = "labels.text.fill", visibility = "off"),
                            c(feature = "poi", element = "labels.text.stroke", visibility = "off"),
                            c(feature = "poi", element = "labels.icon", visibility = "off")
                          ))


 
 coord_zoom <- function(zoom = 0.05) {
   
   # Shrink by zoom_pct for tighter zoom
   xrange <- xmax - xmin
   yrange <- ymax - ymin
   
   xmin1 <- xmin + (zoom * xrange)
   xmax1 <- xmax - (zoom * xrange)
   ymin1 <- ymin + (zoom * yrange)
   ymax1 <- ymax - (zoom * yrange)
   
   # Return coord_sf layer
   coord_sf(
     xlim = c(xmin1, xmax1),
     ylim = c(ymin1, ymax1),
     expand = FALSE
   )
 }
 
 

 # Function to force locking to basemap
 
 lock_to_basemap <- function(map) {
   bb <- attr(map, "bb")
   coord_sf(
     xlim = c(bb$ll.lon, bb$ur.lon),
     ylim = c(bb$ll.lat, bb$ur.lat),
     expand = FALSE
   )
 }
