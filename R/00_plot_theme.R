library(showtext)
library(sysfonts)
library(tidyverse)
library(camcorder) # just to preview plots
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
    axis.title.x = element_text(family = "Open Sans", size = 20),
    axis.title.y = element_text(family = "Open Sans", size = 20),
    axis.text = element_text(family = "Open Sans", size = 16),
    ## Plot titles
    plot.title = element_text(family = "Open Sans", size = 28),
    plot.subtitle = element_text(family = "Open Sans", size = 20),
    strip.text = element_text(family = "Open Sans", size = 20,
                              margin = margin(t = 2, b = 2)),
    ## Legend options
    legend.title = element_text(family = "Open Sans", size = 20),
    legend.text = element_text(family = "Open Sans", size = 16,
                               margin = margin(l = 1))
  )



# ## Test paper theme
 p <- cars %>%
   mutate(category = case_when(speed < 10 ~ "A", TRUE ~ "B")) %>%
   ggplot() +
   geom_point(aes(x = dist, y = speed, color = dist)) +
   labs(
     title = "title",
     subtitle = "subtitle",
     color = "legend title"
   ) +
   facet_wrap(~category) +
   paper_theme

 gg_record(
   device = "png",
   width = 4,
   height = 4,
   unit = "in"
 )

 p
 
 

# Basemap and Map Theme ---------------------------------------------------
 vocs_raw <- read_csv(here("data", "clean", "dat_mgm3.csv"), col_select = -1) %>%
   filter(!is.na(site_id) & !is.na(site)) 
 
# Generate Basemap --------------------------------------------------------
 coords <- readRDS("data/clean/coords.rds")
 
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
 
 xrange <- xmax - xmin
 yrange <- ymax - ymin
 
 xmin_zoom <- xmin + 0.05 * xrange
 xmax_zoom <- xmax - 0.05 * xrange
 ymin_zoom <- ymin + 0.05 * yrange
 ymax_zoom <- ymax - 0.05 * yrange
 
 ggmap(basemap) +
   coord_sf(
     xlim = c(xmin_zoom, xmax_zoom),
     ylim = c(ymin_zoom, ymax_zoom),
     expand = FALSE
   )

 
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
