library(readxl)
library(tidyverse)
library(patchwork)
library(sf)
library(here)

# Purpose of this code:
# Calculate and visualize BTEX ratios.

# OUTPUT MAP: which code block produces which publication figure/table
# --------------------------------------------------------------------
# Table 2  (main text)     -> ratios_by_site.csv, ratios_by_site_type.csv,
#                             ratios_overall.csv, btex_ratio_landuse.csv
#                             (these four combine to form Table 2)
# Figure S5 (supplemental) -> ratio_ts_map.png    (ratio time series + site map)
# Table S4  (supplemental) -> btex_ratio_summary.csv
# --------------------------------------------------------------------


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


# ===========================================================================
# TABLE 2 (main text): BTEX ratios by site, site type, overall, and land use
# (four files below combine to form Table 2)
# ===========================================================================

## Ratios for all sites
ratio_medians_site <- vocs %>%
  group_by(site_type, site, site_id) %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f [%.2f, %.2f]",
                median(.x, na.rm = TRUE),
                quantile(.x, 0.25, na.rm = TRUE),
                quantile(.x, 0.75, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_excel_csv(ratio_medians_site, here("results", "tables", "ratios_by_site.csv"))

## Ratios by site type
ratio_medians_sitetype <- vocs %>%
  group_by(site_type) %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f [%.2f, %.2f]",
                median(.x, na.rm = TRUE),
                quantile(.x, 0.25, na.rm = TRUE),
                quantile(.x, 0.75, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_excel_csv(ratio_medians_sitetype, here("results", "tables", "ratios_by_site_type.csv"))

## Ratios overall
ratio_medians_overall <- vocs %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f [%.2f, %.2f]",
                median(.x, na.rm = TRUE),
                quantile(.x, 0.25, na.rm = TRUE),
                quantile(.x, 0.75, na.rm = TRUE))
    )
  )

write_excel_csv(ratio_medians_overall, here("results", "tables", "ratios_overall.csv"))

## Ratios by land-use type
ratio_medians_landuse <- vocs %>%
  group_by(industrial_20, site_traffic) %>%
  summarize(
    across(
      c(ends_with("_ratio"), "benzene", "toluene", "ethylbenzene", "m_p_xylene_2", "o_xylene"),
      ~ sprintf("%.2f [%.2f, %.2f]",
                median(.x, na.rm = TRUE),
                quantile(.x, 0.25, na.rm = TRUE),
                quantile(.x, 0.75, na.rm = TRUE))
    ),
    .groups = "drop"
  )

write_excel_csv(ratio_medians_landuse, here("results", "tables", "btex_ratio_landuse.csv"))


# Time Series Plots -------------------------------------------------------

site_info <- read_csv("data/site_info.csv")

colors <- site_info %>%
  filter(site_type == "stationary") %>%
  distinct(site, site_color) %>%
  with(setNames(site_color, site))

tb_ts <- vocs %>% 
  group_by(site) %>%
  filter(site_type == "stationary") %>%
  ggplot() + 
  geom_line(aes(x = as.Date(end_date), y = tb_ratio, color = site)) + 
  labs(
    x = "Date",
    y = "T/B Ratio Ratio",
    subtitle = "Toluene:Benzene Ratio"
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
  geom_sf_text(
    data = vocs_sf,
    aes(label = site_id),
    size = 9,
    fontface = "bold",
    color = "white",
    inherit.aes = FALSE
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


# ===========================================================================
# FIGURE S5 (supplemental): Ratio time series + site map
# ===========================================================================
ggsave(here("results", "supplemental", "figures", "ratio_ts_map.png"),
       ratio_ts_map,
       width = unit(8, "in"),
       height = unit(8, "in"))




# ===========================================================================
# TABLE S4 (supplemental): BTEX ratio summary by site type x industry
# ===========================================================================
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
