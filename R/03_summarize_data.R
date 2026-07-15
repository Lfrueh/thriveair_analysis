library(tidyverse)
library(readxl)
library(here)
library(lme4) # Dependency to calculate ICC with psych package
library(psych)
library(patchwork)
library(tableone)
library(ggcorrplot)
library(ggtext)
library(ggpattern)
library(ggnewscale)
library(gridExtra)
library(sf)

########################################################
# Purpose of this code:
# To summarize VOC measurement data, including:
## Basic summary statistics
## Visualize distributions
## Correlations
########################################################

source("R/00_plot_theme.R")  


# Get Data ----------------------------------------------------------------

## Codebook ----
codebook <- read_excel(here("data","codebook.xlsx")) %>%
  arrange(category_2no, sort_in_category) %>%
  # Replace hyphen with a zero-width space to facilitate line breaks
  mutate(voc_name = str_replace_all(voc_name, "-", "-\u200b"))

voc_vars <- codebook %>% 
  filter(variable_name != "btex") %>% pull(variable_name)

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)

# Get categories and associated VOC variables
categories <- codebook %>%
  filter(!variable_name %in% c("btex", "xylenes")) %>%
  select(variable_name, voc_name, category_2) %>%
  filter(!is.na(category_2))

category_groups <- split(categories, categories$category_2)

# Force line break every N characters regardless of word boundaries
hard_wrap <- function(x, width = 15) {
  sapply(x, function(label) {
    # Insert newline every 'width' characters
    gsub(paste0("(.{", width, "})"), "\\1\n", label)
  }, USE.NAMES = FALSE)
}

## VOC Data -----
vocs_all <- read_csv("data/clean/dat_mgm3.csv")

vocs <- vocs_all %>% select(-ends_with("_unc"), -ends_with("_unc")) 

colos <- read_csv("data/clean/colos.csv", col_select = -1) %>%
  select(-ends_with("_flag"), -ends_with("_unc"), -starts_with("xylenes"), -btex) 


# Create Site Summary Map -------------------------------------------------
## Note that this will take a little while due to landuse 
base <- ggmap(basemap) 

# Former refinery tax parcels from Open Data Philly
refinery <- st_read(here("data", "shp", "refinery.shp")) %>%
  st_transform(crs = 4326)

# Land use map from OpenData Philly https://opendataphilly.org/datasets/land-use/
# Clipped to study area to ease plotting
landuse <- st_read(here("data", "shp", "landuse_ssw.shp")) %>%
  st_transform(crs = 4326) %>%
  # Filter out roads and water, since these are marked on the basemap
  filter(!c_dig1desc %in% c("5", "8")) %>%
  # Describe landuse categories
  mutate(landuse = case_when(
    c_dig1desc == "1" ~ "Residential",
    c_dig1desc == "2" ~ "Commercial",
    c_dig1desc == "3" ~ "Industrial",
    c_dig1desc == "4" ~ "Institution",
    c_dig1desc == "6" ~ "Recreation",
    c_dig1desc == "7" ~ "Open Space",
    c_dig1desc == "9" ~ "Vacant"
  ))

# Monitoring sites
sites_sf <- vocs %>%
  select(site, site_id, site_type, site_traffic, long, lat) %>%
  distinct() %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

# Color scale for landuse
landuse_colors <- c(
  "Residential" = "#f0e000", 
  "Commercial"  = "#ed6baa", 
  "Industrial"  = "#b8561d", 
  "Institution" = "#b68dcc", 
  "Recreation"  = "#a3de95", 
  "Open Space"  = "#006400", 
  "Vacant"      = "#b2b2b2"
)

# fake legend for Former Refinery
fake_refinery <- data.frame(x = 1, y = 1)


refinery_map <- ggmap(basemap) + 
  # Dummy refinery point to force legend
  geom_point(
    data = fake_refinery,
    aes(x = x, y = y, fill = "Former Refinery"),
    shape = 22, size = 4, color = NA, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Former Refinery" = "grey80"),
    guide = guide_legend(
      override.aes = list(
        shape = 22,          # square
        fill = "grey80",
        pattern = "stripe",  # if using ggpattern
        pattern_spacing = 0.01,
        pattern_angle = 45
      ),
      order = 2
    )
  ) +
  
  # Former Refinery site
  geom_sf_pattern(data = refinery, 
                  aes(fill = "Former Refinery"), #dummy variable for legend
                  inherit.aes = FALSE, alpha = 0.4,
                  pattern = "stripe", pattern_spacing = 0.01,
                  pattern_alpha = 0.5) + 
  new_scale_fill()


sitemap <- refinery_map +
  # Land Use
  geom_sf(data = landuse, aes(fill = landuse), lwd = 0, alpha = 0.4,
          inherit.aes = FALSE) + 
  scale_fill_manual(values = landuse_colors,
                    name = "Land Use",
                    guide = guide_legend(order = 1)
  )  + 
  new_scale_fill() + 
  # Monitor site points
  geom_sf(data = sites_sf, inherit.aes = FALSE,
          aes(shape = site_type, fill = site_traffic),
          color = "black", stroke = 0.4, size = 7) + 
  # Site ID annotations with shadow
  # Site ID annotations - white text
  geom_sf_text(
    data = sites_sf,
    aes(label = site_id),
    size = 11,
    fontface = "bold",
    color = "white",
    inherit.aes = FALSE
  ) +
  scale_shape_manual(
    values = c("stationary" = 21, "rotating" = 22),
    labels = c("stationary" = "Year-Round", "rotating" = "Rotating"),
    name = "Site Type"
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "blue",
      "High" = "red"
    ),
    name = "Traffic",
    guide = guide_legend(
      override.aes = list(
        shape = 21,      # make sure legend keys are filled circles
        color = "black", # outline to match your points
        fill = c("red", "blue")
      )
    )
  ) + 
  # North arrow
  annotation_north_arrow(
    location = "br",  # bottom-right
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    which_north = "true",
    style = north_arrow_orienteering(text_size = 32,
                                     line_width = 1,
                                     text_face = "bold")
  ) +
  coord_zoom(1.15) + 
  labs(
    y = NULL,
    x = NULL
    # title = "Monitoring Sites",
    # subtitle = "Land Use and Traffic Density Considerations"
  ) + 
  paper_theme + 
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.box.spacing = unit(0.3, "cm"), 
    legend.spacing = unit(0.3, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 2),  
    legend.key.size = unit(0.8, "cm"),
    plot.margin = margin(3,3,3,3, "pt") 
  )


site_key <- sites_sf %>%
  st_drop_geometry() %>%
  select(site_id, site) %>%
  mutate(site= iconv(site, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(site = str_wrap(site, width = 21)) %>%
  distinct() %>%
  rename(ID = site_id, Site = site) %>%
  arrange(ID)

key_table <- tableGrob(
  site_key,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontsize = 32, hjust = 0, x = 0.05, lineheight = 0.3),  # left-justify, small left margin
      padding = unit(c(4, 2), "mm")  
    ),
    colhead = list(
      fg_params = list(fontsize = 32, fontface = "bold", hjust = 0, x = 0.05)
    )
  )
)


sitemap_combo <- wrap_elements(key_table) + sitemap + plot_layout(widths = c(1,3))  




## Wind speed ------
speed_levels_inner_to_outer <- c(">45","40 to 45","35 to 40","30 to 35",
                                 "25 to 30","20 to 25","15 to 20",
                                 "10 to 15","5 to 10","≤5")

# Direction order: N first => coord_polar places it at top
dir_order <- c("N","NNE","NE","ENE","E","ESE","SE","SSE",
               "S","SSW","SW","WSW","W","WNW","NW","NNW")

speed_colors <- c(
  "≤5"      = "#EFF7FF",
  "5 to 10" = "#C6DBEF",
  "10 to 15"= "#9ECAE1",
  "15 to 20"= "#6BAED6",
  "20 to 25"= "#4292C6",
  "25 to 30"= "#2171B5",
  "30 to 35"= "#08519C",
  "35 to 40"= "#08306B",
  "40 to 45"= "#05204A",
  ">45"     = "#021226"
)

ring_label <- data.frame(
  direction = factor("N", levels = dir_order),
  y         = 500,
  label     = "500"
)

wind_data <- read_csv("data/raw/wind_speeds.csv") %>%
  pivot_longer(-Category, names_to = "speed", values_to = "count") %>%
  mutate(
    direction = factor(Category, levels = dir_order),
    speed     = factor(str_squish(speed), levels = speed_levels_inner_to_outer)
  )


windrose <- ggplot(wind_data, aes(x = direction, y = count, fill = speed)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.03) +
  # -pi/16 offsets by half a sector so the N bar is centred at 12 o'clock
  coord_polar(start = -pi / 16) +
  scale_fill_manual(
    values = speed_colors,
    name   = "Wind speed\n(miles/hr)",
    guide  = guide_legend(reverse = TRUE,
                          override.aes = list(shape = 21, size = 5))
  ) +
  scale_y_continuous(
    breaks = c(100, 200, 300, 400, 500, 600, 700),
    labels = NULL,
    expand = c(0, 0)
  ) +
  geom_text(
    data        = ring_label,
    aes(x = direction, y = y, label = label),
    inherit.aes = FALSE,
    size        = 8,
    colour      = "black",
    vjust       = -1.5,
    hjust       = -0.5
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  paper_theme + 
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.box.spacing = unit(0.3, "cm"), 
    legend.spacing = unit(0.3, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 2),  
    legend.key.size = unit(0.8, "cm"),
    axis.ticks  = element_blank(),
    legend.title = element_text(lineheight = 0.3),
    plot.margin = margin(3,3,3,3, "pt") ,
    panel.grid.major = element_line(color = "grey75")
  ) 


sources <- st_read(here("data","shp","thriveair_potential_sources.shp")) %>%
  janitor::clean_names() %>%
  filter(symbol_id != 2) %>%
  mutate(source_type = factor(case_when(
    symbol_id == 0 ~ "Oil & Gas",
    symbol_id == 1 ~ "Auto Salvage",
    symbol_id == 3 ~ "Recycling",
    symbol_id == 4 ~ "Auto Repair",
    symbol_id == 5 ~ "Metal Fabrication",
    symbol_id == 6 ~ "Logistics",
    symbol_id == 7 ~ "Dry Cleaning"
  ), levels = c("Oil & Gas","Auto Repair","Auto Salvage",
                "Recycling","Metal Fabrication","Logistics",
                "Dry Cleaning")
  )) %>%
  st_transform(crs = 4326)

location_map <- refinery_map + 
  # Porential sources
  geom_sf(data = sources, inherit.aes = FALSE,
          aes(fill = source_type),
          shape = 21, color = "black", stroke = 0.4, size = 6) + 
  scale_fill_manual(values = c(
    "#FF2B00",
    "#A200FF",
    "#6700A3",
    "#002FFF",
    "#32C200",
    "#FF9B00",
    "#FBFF00"
  ),
  name = "Source Type") +
  
  # Monitor site points
  geom_sf(data = sites_sf, inherit.aes = FALSE,
          fill = "black", stroke = 0.4, size = 7) + 
  # Site ID annotations with shadow
  geom_sf_text(
    data = sites_sf,
    aes(label = site_id),
    size = 11,
    fontface = "bold",
    color = "white",
    inherit.aes = FALSE
  ) +
  #  North arrow
  annotation_north_arrow(
    location = "br",  # bottom-right
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    which_north = "true",
    style = north_arrow_orienteering(text_size = 32,
                                     line_width = 1,
                                     text_face = "bold")
  ) +
  coord_zoom(1.15) + 
  labs(
    y = NULL,
    x = NULL
  ) + 
  paper_theme + 
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.box.spacing = unit(0.3, "cm"), 
    legend.spacing = unit(0.3, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 2),  
    legend.key.size = unit(0.8, "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    plot.margin = margin(3,3,3,3, "pt") 
  ) 




windspeed_combo <- windrose + location_map + plot_layout(widths = c(1,1.5))


figure1 <- sitemap_combo / windspeed_combo

ggsave(
  filename = here("results", "figures", "figure1.png"),
  plot = figure1,
  width = unit(9, "in"),
  height = unit(12, "in")
  
)


# Summarize Detection Rates -----------------------------------------------

summarize_flags <- function(df, siteval = NULL){
  
  if (!is.null(siteval)){
    df <- df %>%
      filter(site_id == siteval)
  } 
  
  n_sample <- df %>% nrow()
  
  result <- df %>%
    mutate(across(ends_with('_flag'), as.character)) %>%
    summarize(across(
      ends_with('_flag'),
      list(
        nd   = ~sum(. == "ND"),
        lod  = ~sum(. == "LOD"),
        reg  = ~sum(. == "REG"),
        ulod = ~sum(. == "ULOD")
      )
    )) %>%
    # pivot counts into long form
    pivot_longer(
      cols = everything(),
      names_to = c("variable_name", "flag_type"),
      names_pattern = "^(.*)_flag_(.*)$",
      values_to = "count"
    ) %>%
    # calculate percentages
    mutate(percentage = sprintf("%.1f", 100 * count / n_sample)) %>%
    # keep individual VOCs only
    filter(!variable_name %in% c("btex", "xylenes")) %>%
    # join in metadata
    left_join(codebook, by = "variable_name") %>%
    arrange(category_2no, sort_in_category)
  
  return(result)
  
}

flag_summary <- summarize_flags(vocs_all)

sites <- unique(vocs_all$site_id)

flag_summary_bysite <- map(
  set_names(sites, sites),
  ~summarize_flags(df = vocs_all, siteval = .x)
)

# Wide format for easier reading as supplemental table
flag_summary_wide <- flag_summary %>%
  select(voc_name, variable_name, percentage, flag_type, category_2) %>%
  pivot_wider(names_from = flag_type, values_from = percentage) %>%
  mutate(
    low_detection_rate = case_when(
      (as.numeric(reg) <= 40 | as.numeric(nd) >= 25)~ 1,
      TRUE ~ 0
    )
  )



# Reliability -------------------------------------------------------------

## Calculate reliability ----
# Make dataset that compares value + co-located value for each VOC
colo_wide <- colos %>%
  mutate(id = paste0(site,"_",week)) %>%
  select(id, sample_type, any_of(voc_vars)) %>%
  pivot_wider(names_from = sample_type, values_from = any_of(voc_vars))

# Helper function: Calculate mean RPD
mean_rpd <- function(x, y) {
  # RPD = |x - y| / ((x + y) / 2) * 100
  # Only calculate for pairs where both are above LOD (optional filter)
  rpd_values <- abs(x - y) / ((x + y) / 2) * 100
  mean(rpd_values, na.rm = TRUE)
}

# Calculate reliability statistics for each pair of values
reliability_results <- map_dfr(intersect(voc_vars, names(colos)), function(v) {
  x <- colo_wide[[paste0(v, "_sample")]]
  y <- colo_wide[[paste0(v, "_duplicate")]]
  
  # Mean RPD
  rpd_val <- mean_rpd(x, y)
  
  # ICC (two-way random, consistency)
  icc_mat <- cbind(x, y)
  icc_val <- ICC(icc_mat, missing = TRUE)$results
  icc_val <- icc_val$ICC[icc_val$type == "ICC2"]  # ICC2 = two-way random, consistency
  if (icc_val == 0) icc_val <- NA 
  
  tibble(
    variable = v,
    mean_RPD = round(rpd_val, 1),
    ICC = round(icc_val,2)
  )
}) %>% 
  left_join(codebook, by = c("variable" = "variable_name")) %>%
  select(voc_name, category_2, mean_RPD, ICC) %>%
  mutate(low_reliability = case_when(
    (mean_RPD >= 50 | ICC <= 0.75) ~ 1,
    TRUE ~ 0
  ))

# Save supplemental table 1 (detection + reliability)
supp_t1 <- left_join(flag_summary_wide, reliability_results, by = c("voc_name", "category_2"))

write_csv(supp_t1, "results/supplemental/tables/suppt1_detection_reliability.csv")

excluded_compounds <- supp_t1 %>%
  filter(low_reliability == 1 | low_detection_rate == 1) %>%
  pull(variable_name)


# Summary Statistics ------------------------------------------------------
## All Compounds ----
### Table ----
# In main text: summarize by weekly vs. community sites, 
# then by industrial indicator

create_t1 <- function(strat_val){
  t1_list <- vocs %>%
    group_by(site_type) %>%  # split by site_type
    group_map(~ {
      # .x is the subset for this site_type
      vars <- voc_vars  # variables to summarize
      cat_vars <- NULL  # specify categorical variables if any
      CreateTableOne(vars = vars,
                     strata = strat_val,
                     data = .x,
                     factorVars = cat_vars,
                     addOverall = TRUE,
                     test = TRUE)  # test = TRUE gives p-values
    })
  
  names(t1_list) <- c("rotating", "stationary")
  
  return(t1_list)
}


# Convert TableOne object to a data.frame for easier manipulation
tidy_t1 <- function(tab) {
  df <- print(tab, nonnormal = voc_vars, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
  
  df <- as.data.frame(df, stringsAsFactors = FALSE) %>%
    rownames_to_column(var = "variable_name_messy") %>%
    mutate(variable_name = 
             str_remove(variable_name_messy, " \\(.*\\)$")) %>%
    left_join(codebook, by = "variable_name") %>%
    arrange(category_2no, sort_in_category)
  return(df)
}

t1_industry <- create_t1("industrial_20")
t1_traffic <- create_t1("site_traffic")
t1_ind_traf <- create_t1("ind_traf")


# Get table for each site type and save
write_csv(tidy_t1(t1_industry[["stationary"]]), 
          here("results", "tables", "stationary_voc_by_industry.csv"))
write_csv(tidy_t1(t1_industry[["rotating"]]), 
          here("results", "tables", "rotating_voc_by_industry.csv"))

# Traffic goes in supplemental
write_csv(tidy_t1(t1_traffic[["stationary"]]), 
          here("results", "supplemental","tables", "stationary_voc_by_traffic.csv"))
write_csv(tidy_t1(t1_traffic[["rotating"]]), 
          here("results", "supplemental","tables", "rotating_voc_by_traffic.csv"))

# Bivariate goes in supplemental
write_csv(tidy_t1(t1_ind_traf[["stationary"]]), 
          here("results", "supplemental","tables", "stationary_voc_by_traffic_industry.csv"))




## Compounds only included in analysis ----
voc_vars_incl <- setdiff(voc_vars, excluded_compounds)

codebook_included <- codebook %>%
  filter(!variable_name %in% excluded_compounds)

cat_map <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2, category_2no) %>%
  mutate(color = scales::hue_pal()(n())[order(category_2no)])

label_df <- codebook %>%
  left_join(cat_map, by = c("category_2", "category_2no")) %>%
  mutate(
    label = glue::glue("<span style='color:{color}'><b>{voc_name}</b></span>")
  ) %>%
  select(variable_name, label)

name_map_colored <- deframe(label_df %>% select(variable_name, label))


## Seasonality Heatmap ------------------------------------------------------
# Helper: generate symmetric log2 breaks and format them as ratio labels
ratio_breaks_labels <- function(lim){
  max_step <- max(1, ceiling(lim))
  breaks <- seq(-max_step, max_step, by = 1)
  ratios <- 2^breaks
  labels <- ifelse(ratios >= 1,
                   paste0(format(round(ratios, 1), trim = TRUE), "x"),
                   paste0(format(round(ratios, 2), trim = TRUE), "x"))
  list(breaks = breaks, labels = labels)
}


seasonality_dat <- vocs %>%
  mutate(
    month = floor_date(as.Date(end_date), "month"),
    month_label = factor(format(month, "%b '%y"), levels = month_levels)
  ) %>%
  pivot_longer(cols = all_of(voc_vars_incl), names_to = "variable_name", values_to = "value") %>%
  left_join(codebook_included, by = "variable_name") %>%
  group_by(variable_name, category_2, category_2no, sort_in_category, month_label) %>%
  summarize(monthly_med = median(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable_name) %>%
  mutate(
    annual_med = median(monthly_med, na.rm = TRUE),
    log2_ratio = log2(monthly_med / annual_med)
  ) %>%
  ungroup() %>%
  arrange(category_2no, sort_in_category) %>%
  mutate(variable_name = factor(variable_name, levels = rev(unique(variable_name))))

lim_season <- max(abs(seasonality_dat$log2_ratio), na.rm = TRUE)
rb_season <- ratio_breaks_labels(lim_season)

category_dummy <- cat_map %>%
  arrange(category_2no) %>%
  mutate(category_2 = factor(category_2, levels = category_2))

p_season_heatmap <- seasonality_dat %>%
  ggplot(aes(x = month_label, y = variable_name)) +
  geom_tile(aes(fill = log2_ratio), color = "white", linewidth = 0.2) +
  geom_point(
    data = category_dummy,
    aes(x = month_levels[1], y = levels(seasonality_dat$variable_name)[1], color = category_2),
    size = 0, inherit.aes = FALSE
  ) +
  scale_fill_distiller(
    palette = "RdBu", direction = -1,
    name = "Ratio to annual\nmedian (log scale)",
    limits = c(-lim_season, lim_season),
    breaks = rb_season$breaks, labels = rb_season$labels
  ) +
  scale_color_manual(
    values = setNames(category_dummy$color, category_dummy$category_2),
    name = "VOC Category",
    guide = guide_legend(override.aes = list(size = 4, shape = 15))
  ) +
  scale_y_discrete(labels = name_map_colored) +
  labs(
    title = "Seasonal Patterns Across VOCs",
    subtitle = "Median-standardized monthly VOC concentrations",
    x = NULL, y = NULL
  ) +
  paper_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_markdown(lineheight = 0.9),
    legend.position = "right",
    legend.title = element_text(lineheight = 0.2),
    panel.grid = element_blank()
  )

ggsave(
  filename = here("results", "supplemental", "figures", "seasonality_heatmap.png"),
  plot = p_season_heatmap,
  width = 7, height = 8, dpi = 320
)

## By Individual Site ----
### Table ----
voc_site_summary <- vocs %>%
  group_by(site) %>%
  summarize(
    site_type = first(site_type),
    site_id = first(site_id),
    across(
      all_of(voc_vars),
      ~ sprintf("%.1f [%.1f, %.1f]", 
                median(.x, na.rm = TRUE), 
                quantile(.x, 0.25, na.rm = TRUE),
                quantile(.x, 0.75, na.rm = TRUE)),
      .names = "{.col}"
    )
  ) %>%
  pivot_longer(all_of(voc_vars),names_to = "variable_name", values_to = "summary") %>%
  left_join(., codebook, by = "variable_name") %>%
  arrange(site_id, category_2no)


write_excel_csv(voc_site_summary, "results/interim_results/tables/voc_by_site_summary.csv")


## Spatial Heatmap: Year-Round / Summer / Winter -----------------------------

vocs_seasonal_spatial <- vocs %>%
  mutate(
    month_num = month(as.Date(end_date)),
    season_meteo = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ "Other"
    )
  )

build_spatial_dat <- function(df){
  df %>%
    mutate(site_id = as.character(site_id)) %>%
    pivot_longer(cols = all_of(voc_vars_incl), names_to = "variable_name", values_to = "value") %>%
    left_join(codebook_included, by = "variable_name") %>%
    group_by(variable_name, voc_name, category_2, category_2no, sort_in_category,
             site_id, site_type) %>%
    summarize(site_med = median(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(variable_name) %>%
    mutate(
      overall_med = median(site_med, na.rm = TRUE),
      log2_ratio = log2(site_med / overall_med)
    ) %>%
    ungroup() %>%
    arrange(category_2no, sort_in_category) %>%
    mutate(
      voc_name = factor(voc_name, levels = rev(unique(as.character(voc_name)))),
      site_type = factor(site_type, levels = c("stationary", "rotating"),
                         labels = c("Year-Round Sites", "Rotating Sites")),
      site_id = factor(site_id, levels = as.character(sort(as.numeric(unique(site_id)))))
    )
}

spatial_dat_year   <- build_spatial_dat(vocs_seasonal_spatial)
spatial_dat_summer <- build_spatial_dat(filter(vocs_seasonal_spatial, season_meteo == "Summer"))
spatial_dat_winter <- build_spatial_dat(filter(vocs_seasonal_spatial, season_meteo == "Winter"))

# Shared color scale across all three, for visual comparability
lim_spatial <- max(
  abs(spatial_dat_year$log2_ratio), abs(spatial_dat_summer$log2_ratio), abs(spatial_dat_winter$log2_ratio),
  na.rm = TRUE
)
rb <- ratio_breaks_labels(lim_spatial)

make_spatial_heatmap <- function(dat, plot_title, file_suffix){
  
  p <- dat %>%
    ggplot(aes(x = site_id, y = voc_name, fill = log2_ratio)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_distiller(
      palette = "RdBu", direction = -1,
      name = "Ratio to annual\nmedian (log scale)",
      limits = c(-lim_spatial, lim_spatial),
      breaks = rb$breaks, labels = rb$labels
    ) +
    scale_y_discrete(labels = name_map_colored) +
    facet_grid(~ site_type, scales = "free_x", space = "free_x") +
    labs(
      title = plot_title,
      subtitle = "Median-standardized site VOCs",
      x = "Site ID", y = NULL
    ) +
    paper_theme +
    theme(
      axis.text.x = element_text(),
      axis.text.y = element_markdown(lineheight = 0.9),
      legend.position = "right",
      legend.title = element_text(lineheight = 0.2),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  ggsave(
    filename = here("results", "supplemental", "figures",
                    paste0("spatial_heatmap_", file_suffix, ".png")),
    plot = p,
    width = 8, height = 9, dpi = 320
  )
  
  p
}

p_spatial_year   <- make_spatial_heatmap(spatial_dat_year, "Spatial Patterns Across VOCs — Year-Round", "yearround")
p_spatial_summer <- make_spatial_heatmap(spatial_dat_summer, "Spatial Patterns Across VOCs — Summer", "summer")
p_spatial_winter <- make_spatial_heatmap(spatial_dat_winter, "Spatial Patterns Across VOCs — Winter", "winter")

## Summer vs. Winter Comparison (Stationary Sites Only) ---------------------
# Standard meteorological seasons: Winter = Dec-Feb, Summer = Jun-Aug.
# If `season` already exists upstream with this definition, swap it in below
# instead of recomputing `season_meteo`.

vocs_seasonal <- vocs %>%
  mutate(
    month_num = month(as.Date(end_date)),
    season_meteo = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(site_type == "stationary", !is.na(season_meteo))

summer_winter_t1 <- CreateTableOne(
  vars = voc_vars_incl,
  strata = "season_meteo",
  data = vocs_seasonal,
  factorVars = NULL,
  addOverall = FALSE,
  test = TRUE
)

summer_winter_df <- print(
  summer_winter_t1, nonnormal = voc_vars_incl,
  quote = FALSE, noSpaces = TRUE, printToggle = FALSE
) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rownames_to_column(var = "variable_name_messy") %>%
  mutate(variable_name = str_remove(variable_name_messy, " \\(.*\\)$")) %>%
  left_join(codebook_included, by = "variable_name") %>%
  filter(!is.na(category_2)) %>%        # drops the "n" header row
  arrange(category_2no, sort_in_category) %>%
  select(voc_name, category_2, Summer, Winter, p) %>%
  rename(`p-value` = p)

write_excel_csv(
  summer_winter_df,
  here("results", "supplemental", "tables", "stationary_summer_vs_winter.csv")
)

# Correlations ------------------------------------------------------------
# Using Spearman rank correlations, since there is non-normality
correlate <- function(data, corr_vars, season_val = NULL, site_val = NULL, rotating = NULL){
  
  dat <- data
  
  # Filter data based on function inputs
  if (!is.null(season_val)){
    dat <- data %>%
      filter(season == season_val)
  } 
  
  if (is.null(rotating)){
    dat <- data %>%
      filter(site_type == "stationary")
    
    sitetype = "Year-Round Sites Only"
  } else sitetype <- "All Sites"
  
  if (!is.null(site_val)){
    dat <- data %>%
      filter(site_id == site_val)
    
    sitename <- first(dat$site)
  } else sitename <- NULL
  
  dat <- dat %>%
    select(all_of(corr_vars)) %>%
    mutate(across(everything(), as.numeric))
  
  cor <- cor(dat, method="spearman")
  
  corp <- cor_pmat(dat, method = "spearman")
  
  n_tests <- choose(length(corr_vars), 2)
  
  subtitle_text <- list(sitename, sitetype, season_val) %>%
    compact() %>%              
    str_c(collapse = ", ")
  
  
  p <- ggcorrplot(cor, 
                  type = "full",
                  show.legend = TRUE,
                  legend.title = expression(rho),
                  p.mat = corp,
                  sig.level = 0.05/n_tests, # Bonferroni-corrected 
                  insig = "pch",
                  pch = 4,
                  pch.col = "gray",
                  pch.cex = 1,
                  lab = FALSE
  ) + 
    labs(
      title = "Spearman Rank Correlations",
      subtitle = subtitle_text,
      x = NULL,
      y = NULL
    ) + 
    scale_x_discrete(labels = name_map_colored[colnames(cor)]) +
    scale_y_discrete(labels = name_map_colored[rownames(cor)]) +
    paper_theme + 
    theme(
      axis.text.x = element_markdown(angle = 45, hjust = 1, size = 34, lineheight = 0.9, margin = margin(t = 2)),
      axis.text.y = element_markdown(size = 34, lineheight = 0.9, margin = margin(r = 2))
    ) + 
    guides(
      fill = guide_colorbar(
        barheight = unit(4, "lines"),  # adjust colorbar height
        barwidth = unit(0.5, "lines")
      )
    )
  
  
  filedetails = list("heatmap_",season_val, site_val, rotating) %>%
    compact() %>%              
    str_c(collapse = "_")
  
  filename = here("results", "supplemental", "figures", 
                  paste0(filedetails,".png"))
  
  ggsave(filename,
         plot = p,
         width = unit(8, "in"),
         height = unit(8, "in"))
  
}

# Map categories onto colors 
cat_map <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2, category_2no) %>%
  mutate(color = scales::hue_pal()(n())[order(category_2no)])

# Make a label_df from the codebook and category color map
label_df <- codebook %>%
  left_join(cat_map, by = c("category_2", "category_2no")) %>%
  mutate(
    # Create colored label text for markdown rendering in ggcorrplot
    label = glue::glue(
      "<span style='color:{color}'><b>{voc_name}</b></span>"
    )
  ) %>%
  select(variable_name, label)

name_map_colored <- deframe(label_df %>% select(variable_name, label))


corr_vocs <- codebook %>% 
  #Ignore total BTEX and total Xylenes in favor of individual ones.
  filter(!(variable_name %in% c("xylenes","btex"))) %>% 
  # Exclude low-detection or poor reliability compounds
  filter(!(variable_name %in% excluded_compounds)) %>%
  pull(variable_name)


correlate(vocs, corr_vocs, rotating = "incl_rotating")


# Summarize EPA Fenceline Monitoring Data ---------------------------------

## Uses data from EPA fenceline benzene monitoring dashboard
## https://awsedap.epa.gov/public/extensions/Fenceline_Monitoring/Fenceline_Monitoring.html?sheet=MonitoringDashboard
## for Philadelphia Energy Solutions

epa_data <- read_excel(here("data", "clean", "epa_pes_data.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(period_start_date = as.Date(period_start_date)) %>%
  mutate(across(ends_with("_dc"), ~as.numeric(.))) %>%
  # Filter out pre-2019, since annual average is 0
  # There are sampling averages then, but no annual
  filter(year(period_start_date) > 2018) %>%
  filter(annual_avg_dc > 0)

epa_data %>%
  select(period_start_date, annual_avg_dc, sampling_period_dc) %>%
  pivot_longer(annual_avg_dc:sampling_period_dc, names_to = "meas", values_to = "value") %>%
  mutate(meas = recode(meas,
                       "annual_avg_dc" = "Annual Average",
                       "sampling_period_dc" = "2-Week Average")) %>%
  ggplot(aes(x = period_start_date, y = value, color = meas)) +
  geom_vline(xintercept = as.Date("2019-06-21"), color = "grey25", linetype = "dotted") + 
  geom_hline(aes(yintercept = 9, color = "EPA Action Level"), linetype = 2) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Annual Average" = "#ed5b2d",
      "2-Week Average" = "darkgrey",
      "EPA Action Level" = "darkblue"
    ),
    breaks = c("Annual Average", "2-Week Average", "EPA Action Level")
  ) +
  labs(
    x = "Sampling Period Start Date",
    y = expression(Delta*"C ("*mu*"g/"*m^3*")"),
    color = NULL,
    title = "PES Refinery Perimeter Benzene Monitoring",
    subtitle = "2019-2022"
  ) +
  scale_x_date(expand = c(0,0)) + 
  paper_theme + 
  theme(
    legend.position = "bottom"
  ) 

ggsave(
  filename = here("results", "supplemental", "figures","EPA_PES_monitoring.png"),
  height = 8, width = 8, units = "in",
  dpi = 320
  
)


