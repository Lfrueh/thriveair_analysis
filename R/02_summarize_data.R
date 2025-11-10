library(tidyverse)
library(readxl)
library(here)
library(lme4) # Dependency to calculate ICC with psych package
library(psych)
library(ggcorrplot)
library(ggtext)
library(ggpattern)
library(ggnewscale)
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
  arrange(category_2no, sort_in_category)

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

## VOC Data -----
vocs_all <- read_csv("data/clean/dat_mgm3.csv", col_select = -1)

vocs <- vocs_all %>% select(-ends_with('flag')) 

colos <- read_csv("data/clean/colos.csv", col_select = -1) %>%
  select(-ends_with('flag'), -starts_with("xylenes"), -btex) 


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
  select(site, site_type, site_traffic, long, lat) %>%
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


sitemap <- ggmap(basemap) + 
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
                  inherit.aes = FALSE, alpha = 0.6,
                  pattern = "stripe", pattern_spacing = 0.01,
                  pattern_alpha = 0.5) + 
  new_scale_fill() +
  # Land Use
  geom_sf(data = landuse, aes(fill = landuse), lwd = 0, alpha = 0.6,
          inherit.aes = FALSE) + 
  scale_fill_manual(values = landuse_colors,
                    name = "Land Use",
                    guide = guide_legend(order = 1)
  )  + 
  new_scale_fill() + 
  # Monitor site points
  geom_sf(data = sites_sf, inherit.aes = FALSE,
          aes(shape = site_type, fill = site_traffic),
          color = "black", stroke = 0.4, size = 5) + 
  scale_shape_manual(
    values = c("stationary" = 21, "rotating" = 22),
    labels = c("stationary" = "Weekly", "rotating" = "Community"),
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
    height = unit(2, "cm"),
    width = unit(2, "cm"),
    which_north = "true",
    style = north_arrow_orienteering(text_size = 32,
                                     line_width = 1,
                                     text_face = "bold")
  ) +
  coord_zoom(1.15) + 
  labs(
    y = NULL,
    x = NULL,
    title = "Monitoring Sites",
    subtitle = "Land Use and Traffic Density Considerations"
  ) + 
  paper_theme + 
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.box.spacing = unit(0.5, "cm"), 
    legend.spacing = unit(0.5, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 2),  
    legend.key.size = unit(1, "cm")
  )


ggsave(
  filename = here("results", "figures", "sites_landuse_traffic.png"),
  plot = sitemap,
  width = unit(8, "in"),
  height = unit(8, "in")
  
)


# Summary Statistics ------------------------------------------------------
## Overall ----
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
  df <- print(tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
  
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




### Histograms ----
# Function to plot and save histograms
voc_histogram <- function(df, codebook, output_dir = "results/interim_results/figures") {
  
  # Loop over each category
  for (cat_name in names(category_groups)) {
    
    cat_info <- category_groups[[cat_name]]
    voc_vars <- cat_info$variable_name
    
    # Pivot df longer for VOCs in this category
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(voc_vars),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      dplyr::left_join(cat_info, by = "variable_name")
    
    # Plot histograms for each VOC
    p <- df_long %>%
      ggplot(aes(x = value)) +
      geom_histogram(
        bins = 30,
        fill = "#1f77b4",
        color = "white",
        linewidth = 0.3
      ) +
      facet_wrap(~ voc_name, scales = "free") +
      theme_minimal() +
      labs(
        title = paste("Distribution of VOC Concentrations —", cat_name),
        x = expression("Concentration ("~mu*"g/"*m^3*")"),
        y = "Count"
      ) +
      paper_theme 
    
    # Save plot
    ggsave(
      filename = file.path(output_dir, paste0("voccat_", cat_name, "_histograms_summary.png")),
      plot = p,
      width = 8,
      height = 8
    )
  }
}

voc_histogram(vocs, codebook)


## By Individual Site ----
### Table ----
voc_site_summary <- vocs %>%
  group_by(site) %>%
  summarize(
    site_type = first(site_type),
    site_id = first(site_id),
    across(
      all_of(voc_vars),
      ~ sprintf("%.2f [%.2f]", median(.x, na.rm = TRUE), IQR(.x, na.rm = TRUE)),
      .names = "{.col}"
    )
  ) %>%
  pivot_longer(all_of(voc_vars),names_to = "variable_name", values_to = "summary") %>%
  left_join(., codebook, by = "variable_name") %>%
  arrange(site_id, category_2no)


write_excel_csv(voc_site_summary, "results/interim_results/tables/voc_by_site_summary.csv")

### Box Plots ----
# Function to plot and save boxplot distributions
voc_boxplot <- function(df, codebook, output_dir = "results/supplemental/figures") {
  
  # Loop over each category
  for (cat_name in names(category_groups)) {
    
    cat_info <- category_groups[[cat_name]]
    voc_vars <- cat_info$variable_name
    
    # Pivot df longer for VOCs in this category
    df_long <- df %>%
      pivot_longer(
        cols = all_of(voc_vars),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      left_join(cat_info, by = "variable_name")
    
    ncol_facet <- ceiling(length(unique(df_long$voc_name)) / 2)
    strip_size <- if (ncol_facet > 2) 20 else 28
    
    # Plot
    p <- df_long %>%
      mutate(
        site = factor(site, levels = c(
          df %>% filter(site_type == "stationary") %>% pull(site) %>% unique(),
          df %>% filter(site_type == "rotating") %>% pull(site) %>% unique()
        ))
      ) %>%
      ggplot(aes(x = site, y = value, fill = site_type)) +
      geom_boxplot(outlier.color = "black", outlier.size = 0.4, size = 0.4) +
      theme_minimal() +
      labs(
        title = cat_name,
        subtitle = "Concentration Distribution by Site",
        x = "Site",
        y = expression("Concentration ("~mu*"g/"*m^3*")"),
        fill = "Site Type"
      ) +
      scale_fill_manual(
        values = c(
          "rotating" = "#1f77b4",
          "stationary" = "#ff7f0e"
        ),
        labels = c(
          "rotating" = "Community Sites",
          "stationary" = "Weekly Sites"
        )
      ) +
      paper_theme +
      coord_flip() + 
      facet_wrap(~ voc_name, scales = "free_x", nrow = 2) +
      theme(legend.position = "bottom",
            strip.text = element_text(size = strip_size))

    
    # Save plot
    ggsave(
      filename = file.path(output_dir, paste0("voccat_bysite_", cat_name, "_boxplot_summary.png")),
      plot = p,
      width = 8,
      height = 8
    )
  }
}

voc_boxplot(vocs, codebook)


# Time Series Plots -------------------------------------------------------
## Function to make time series plots
voc_ts <- function(df, codebook, output_dir = "results/supplemental/figures") {
  
  # Loop over each category
  for (cat_name in names(category_groups)) {
    
    cat_info <- category_groups[[cat_name]]
    voc_vars <- cat_info$variable_name
    
    # Pivot df longer for VOCs in this category
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(voc_vars),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      dplyr::left_join(cat_info, by = "variable_name")
    
    # Plot histograms for each VOC
    p <- df_long %>%
      ggplot(aes(x = as.Date(end_date), y = value)) +
      geom_point(aes(color = site_type), size = 0.4, alpha = 0.5) +
      geom_smooth(color = "black", linewidth = 0.4) +
      facet_wrap(~ voc_name, scales = "free_y") +
      theme_minimal() +
      labs(
        title = cat_name,
        y = expression("Concentration ("~mu*"g/"*m^3*")"),
        x = "Date",
        color = "Site Type"
      ) +
      scale_color_manual(
        values = c(
          "rotating" = "#1f77b4",
          "stationary" = "#ff7f0e"
        ),
        labels = c(
          "rotating" = "Community Sites",
          "stationary" = "Weekly Sites"
        )
      ) +
      paper_theme +
      guides(
        color = guide_legend(
          override.aes = list(size = 2, alpha = 1)
        )
      ) + 
      theme(
        strip.text = element_text(size = 24),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    # Save plot
    ggsave(
      filename = file.path(output_dir, paste0("voccat_", cat_name, "_ts_summary.png")),
      plot = p,
      width = 8,
      height = 8
    )
  }
}

voc_ts(vocs, codebook)







# Summarize Detection Rates -----------------------------------------------

n_sample <- vocs_all %>% nrow()

flag_summary <- vocs_all %>%
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
  mutate(percentage = round(100 * count / n_sample, 1),
         n_perc     = paste0(count, " (", percentage, "%)")) %>%
  # keep individual VOCs only
  filter(!variable_name %in% c("btex", "xylenes")) %>%
  # join in metadata
  left_join(codebook, by = "variable_name") %>%
  arrange(category_2no, sort_in_category)

# Wide format for easier reading as supplemental table
flag_summary_wide <- flag_summary %>%
  select(voc_name, n_perc, flag_type, category_2) %>%
  pivot_wider(names_from = flag_type, values_from = n_perc)

# Save results
write_csv(flag_summary, "results/interim_results/tables/flag_summary.csv")
write_csv(flag_summary_wide, "results/supplemental/tables/flag_summary_wide.csv")


# Reliability -------------------------------------------------------------

## Calculate reliability ----
# Make dataset that compares value + co-located value for each VOC
colo_wide <- colos %>%
  mutate(id = paste0(site,"_",week)) %>%
  select(id, sample, any_of(voc_vars)) %>%
  mutate(sample = tolower(sample)) %>%
  pivot_wider(names_from = sample, values_from = any_of(voc_vars))

# Helper function: Calculate Root Mean Squared Error (RMSE)
rmse <- function(x, y) sqrt(mean((x - y)^2, na.rm = TRUE))

# Calculate reliability statistics for each pair of values
reliability_results <- map_dfr(intersect(voc_vars, names(colos)), function(v) {
  x <- colo_wide[[paste0(v, "_sample")]]
  y <- colo_wide[[paste0(v, "_duplicate")]]
  
  # RMSE
  rmse_val <- sqrt(mean((x - y)^2, na.rm = TRUE))
  
  # Linear regression Duplicate ~ Sample
  fit <- lm(y ~ x)
  r2_val <- summary(fit)$r.squared
  
  # ICC (two-way random, consistency)
  icc_mat <- cbind(x, y)
  icc_val <- ICC(icc_mat, missing = TRUE)$results
  icc_val <- icc_val$ICC[icc_val$type == "ICC2"]  # ICC2 = two-way random, consistency
  if (icc_val == 0) icc_val <- NA 
  
  tibble(
    variable = v,
    RMSE = round(rmse_val,2),
    R2 = round(r2_val,2),
    ICC = round(icc_val,2)
  )
}) %>% 
  left_join(codebook, by = c("variable" = "variable_name"))

# Save results
write_csv(reliability_results, "results/supplemental/tables/reliability_results.csv")



## Visualize reliability --------
colo_plot <- function(v){
  sample_col <- sym(paste0(v, "_sample"))  # turn string into symbol for tidy eval
  dup_col  <- sym(paste0(v, "_duplicate"))
  
  p <- colo_wide %>%
    ggplot() + 
    geom_point(aes(x = !!sample_col, y = !!dup_col)) + 
    labs(
      title = v,
      x = paste0(v, ", Sample"),
      y = paste0(v, ", Duplicate")
    ) +
    theme_bw()
  
  print(p)
  
}

# 
# for (v in setdiff(voc_vars, "xylenes")){
#   colo_plot(v)
# }



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
    
    sitetype = "Stationary Sites Only"
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
      axis.text.x = element_markdown(angle = 45, hjust = 1, size = 20, lineheight = 0.9, margin = margin(t = 2)),
      axis.text.y = element_markdown(size = 20, lineheight = 0.9, margin = margin(r = 2))
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


ind_vocs <- codebook %>% 
  #Ignore total BTEX and total Xylenes in favor of individual ones.
  filter(!(variable_name %in% c("xylenes","btex"))) %>% pull(variable_name)


correlate(vocs, ind_vocs)
correlate(vocs, ind_vocs, "Summer")
correlate(vocs, ind_vocs, "Winter")
correlate(vocs, ind_vocs, rotating = "incl_rotating")
correlate(vocs, ind_vocs, "Summer", rotating = "incl_rotating")
correlate(vocs, ind_vocs, "Winter", rotating = "incl_rotating")



