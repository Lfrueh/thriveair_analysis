library(showtext)
library(sysfonts)
library(tidyverse)

########################################################
# Purpose of this code:
# Create a standardized theme for all plots and tables.
########################################################

# Add and register Open Sans
font_add_google(name = "Open Sans", family = "Open Sans")
# Automatically use showtext for new plots
showtext_auto(enable = TRUE)

# Define consistent theme for plots and maps -----------------
## GGPlot themes -----
paper_theme <- theme_bw(base_family = "opensans") +
  theme(
    plot.background = element_rect("white"),
    strip.background = element_rect(color = "black", linewidth = 0.2, fill = "gray"),
    panel.border = element_rect(color = "black",  linewidth = 0.2, fill = NA),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.2),       # controls thickness of tick marks
    # axis.ticks.length = unit(0.2, "cm"),          # controls length of tick marks
    #    panel.background = element_rect("#f6f6e9"),
    text = element_text(family = "Open Sans"),
    axis.title = element_text(family = "Open Sans", size = 22, face = "bold"),
    axis.text = element_text(family = "Open Sans", size = 13),
    legend.text = element_text(family = "Open Sans", size = 16,
                               margin = margin(l = 1)),
    legend.title = element_text(family = "Open Sans", size = 20),
    plot.title = element_text(family = "Open Sans", size = 28),
    plot.subtitle = element_text(family = "Open Sans", size = 18),
    strip.text = element_text(family = "Open Sans", size = 14, ,face = "bold"),
    legend.spacing.y = unit(1, "pt"),
    legend.box.spacing = unit(1, "pt")
  )
