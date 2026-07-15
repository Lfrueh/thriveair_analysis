library(tidyverse)
library(readxl)
library(patchwork)
library(ggtext)
library(here)
library(scales)
library(openxlsx)

source("R/00_plot_theme.R")  

# Purpose of this code: 
# Visualize PMF results stored in interim_results.
# PMF was performed using EPA PMF 5.0 as described in the manuscript.

# OUTPUT MAP: which code block produces which publication figure/table
# --------------------------------------------------------------------
# Figure 3  (main text)     -> fact_contrib_allsites_all_pmf.png,
#                              species_contrib_allsites_all_pmf.png
# Figure 4  (main text)     -> sites_factors_allsites.png
# Figure S6 (supplemental)  -> sites_factors_timeseries.png
# Figure S8 (supplemental)  -> fact_contrib_stationary_all_pmf.png,
#                              species_contrib_stationary_all_pmf.png
# Figure S9 (supplemental)  -> fact_contrib_allsites_summer_pmf.png,
#                              species_contrib_allsites_summer_pmf.png
# Figure S10 (supplemental) -> fact_contrib_allsites_winter_pmf.png,
#                              species_contrib_allsites_winter_pmf.png
# Figure S11 (supplemental) -> species_contrib_seasonal_comparison.png
# --------------------------------------------------------------------

# Codebook ----------------------------------------------------------------
codebook <- read_excel('data/codebook.xlsx')
voc_vars <- codebook %>% 
  #Ignore total BTEX and total Xylenes in favor of individual ones.
  filter(!(variable_name %in% c("xylenes", "btex"))) %>% pull(variable_name)

category_levels <- codebook %>%
  arrange(category_2no) %>%
  distinct(category_2) %>%
  pull(category_2)

site_info <- read_csv("data/site_info.csv")



# Plot PMF Results -------------------------------

get_pmf_factors <- function(site_type = c("allsites","stationary"),
                            season = c("all", "summer", "winter")){
  
  season <- match.arg(season)
  
  
  if (site_type == "stationary"){
    factor_file <- "stationary_pmf_bs_fpeak.xlsx"
  } else {
    if (season == "all"){
      factor_file <- "allsites_pmf_bs_fpeak.xlsx"
    } else if (season == "summer"){
      factor_file <- "summer_pmf_bs.xlsx"
    } else if (season == "winter"){
      factor_file <-"winter_pmf_bs.xlsx"
    }
    
  }
  
  
  
  # Get factor information
  readsheet <- function(sheetname){
    read_excel(file.path("results", "interim_results","pmf_results", factor_file),
               sheet = sheetname) %>%
      janitor::clean_names() %>%
      mutate(across(-species, ~as.numeric(.x))) %>%
      rename(variable_name = species)
  }
  
  # Pull only sheets that match "Factor N" pattern, however many exist
  sheets <- excel_sheets(file.path("results", "interim_results", "pmf_results", factor_file)) %>%
    str_subset("^Factor [0-9]+$")
  
  pmf_fac <- map(sheets, ~readsheet(.x))
  names(pmf_fac) <- sheets
  
  
  pmf_fac_results <- bind_rows(pmf_fac, .id = "factor") %>%
    left_join(., codebook, by = "variable_name") %>%
    mutate(total_mass = sum(bs_median_conc)) %>%
    group_by(factor) %>%
    # Calculate % mass explained by each factor
    mutate(
      total_mass_fac = paste0(" (",round(100*sum(bs_median_conc) / total_mass,0),"%)")
    ) %>%
    # Normalize concentration contributions for plotting
    mutate(denominator = sum(bs_median_conc),
           bs_median_conc = 100*bs_median_conc/denominator,
           bs_95th_conc = 100*bs_95th_conc/denominator,
           bs_5th_conc = 100*bs_5th_conc/denominator) %>%
    ungroup() %>%
    # Renormalize % of species total so factors sum to 100% per species
    # (bootstrap medians taken independently per factor don't preserve mass
    # balance across factors, since median is non-additive)
    group_by(variable_name) %>%
    mutate(
      pct_denom = sum(bs_median_pct, na.rm = TRUE),
      bs_median_pct = 100 * bs_median_pct / pct_denom,
      bs_5th_pct = 100 * bs_5th_pct / pct_denom,
      bs_95th_pct = 100 * bs_95th_pct / pct_denom
    ) %>%
    ungroup() %>%
    select(-pct_denom) %>%
    mutate(factor_text = paste0(factor, total_mass_fac))
  
  return(pmf_fac_results)
  
  
}

make_species_contrib_plot <- function(data, col_pal, plot_title = NULL){
  
  data %>%
    ggplot() + 
    aes(x = voc_name, y = bs_median_pct, fill = Factor) + 
    geom_bar(position = position_stack(reverse = TRUE), stat = "identity") + 
    coord_flip() + 
    theme_minimal() +
    scale_fill_manual(values = col_pal) + 
    labs(
      title = plot_title,
      x = "VOC", y = "Contribution (%)",
      fill = "Factor"
    ) +
    paper_theme
}

# `output_subdir` lets callers route main-text vs. supplemental figures into
# the correct results folder (see call sites below and the output map above).
plot_pmf_factors <- function(data, site_type = "allsites", season = c("all", "summer", "winter"),
                             print = FALSE, save = TRUE, output_subdir = "figures"){
  
  season <- match.arg(season)
  if (site_type == "stationary"){
    subtitle_text = "Year-round sites only"
    
  } else {
    subtitle_text = "All sites"
  }
  
  if (season == "all"){
    col_pal <- factor_colors
  } else if(season == "summer"){
    col_pal <- factor_colors_summer
  } else if (season == "winter"){
    col_pal <- factor_colors_winter
  }
  
  # Long merged-category labels (e.g. "Mixed Industry & Background") need
  # wrapping + a smaller strip size when season != "all"
  strip_wrap <- if (season == "all") 40 else 30
  strip_size <- if (season == "all") 40 else 34
  
  
  file_name <- paste0(site_type,"_",season,"_pmf.png")
  
  data$category_2 <- factor(data$category_2,levels = category_levels)
  
  data <- data %>%
    arrange(category_2, voc_name) %>%
    mutate(voc_name = factor(voc_name, levels = unique(voc_name))) # Set the order for voc_name
  
  # Plot 1: VOC Contribution to Factors
  plot1 <- data %>%
    ggplot() +
    aes(x = voc_name, y = bs_median_conc, fill = category_2) + 
    geom_col() +
    geom_errorbar(
      aes(ymin = bs_5th_conc, ymax = bs_95th_conc),
      width = 0.5,      
      color = "gray50",  
      linewidth = 0.3  
    ) +
    facet_wrap(~factor_text, scales = "free_x", ncol = 2,
               labeller = labeller(factor_text = function(x) str_wrap(x, width = strip_wrap))) + 
    coord_flip() + 
    labs(
      title = "Relative VOC Contributions to Each Factor",
      subtitle = subtitle_text,
      y = "Normalized Concentration (%)",
      x = "VOC",
      fill = "VOC Group"
    ) +
    paper_theme + 
    theme(
      axis.text.x = element_text(angle = 30),
      legend.key.height = unit(0.3, "in"),
      legend.key.width = unit(0.3, "in"),
      strip.text = element_text(size = strip_size, lineheight = 0.3)
    )  
  
  if (save){
    ggsave(
      file.path("results", output_subdir, paste0("fact_contrib_", file_name)),
      plot = plot1,
      width = unit(9, "in"),
      height = unit(11, "in"),
      dpi = 320
    )
  }
  
  
  # Plot 2: Factor Contributions to Total VOC Mass
  
  plot2 <- make_species_contrib_plot(data,col_pal)
  
  if(save) {
    ggsave(
      file.path("results", output_subdir, paste0("species_contrib_", file_name)),
      plot = plot2,
      width = unit(8, "in"),
      height = unit(8, "in"),
      dpi = 320
    ) 
  }
  
  
  
  
  if(print){
    print(plot1)
    print(plot2)
  }
  
}

# Get results & Add interpretation

# Define once, keyed by interpretation label (not position) -- this is the fix
factor_colors <- c(
  "Vehicle Exhaust"      = "#1F3A8A",  # dark blue
  "Gasoline Evaporation" = "#F59E0B",  # bright orange
  "Industrial Solvents"  = "turquoise",
  "Background Pollution" = "#B91C1C",  # deep red
  "Mixed Industry"       = "#16A34A"   # bright green
)

factor_levels <- names(factor_colors)  # single source of truth for ordering too

# Factor-number-to-label mappings, one per PMF model run -----------------
# IMPORTANT: PMF factor numbers are arbitrary and assigned independently by
# each model run (allsites/stationary/summer/winter are 4 separate PMF runs),
# so "Factor 1" can mean something different in each. These mappings encode
# the manual interpretation of each run's factors and must be re-verified
# any time PMF is re-run, since factor numbering can shift between runs even
# on the same input data.
# Defined once here (previously this mapping was retyped identically in three
# separate places for the allsites/all-season results alone, which risked the
# copies drifting out of sync).
factor_label_allsites <- c(
  "Factor 1" = "Industrial Solvents",
  "Factor 2" = "Vehicle Exhaust",
  "Factor 3" = "Background Pollution",
  "Factor 4" = "Gasoline Evaporation",
  "Factor 5" = "Mixed Industry"
)

factor_label_stationary <- c(
  "Factor 1" = "Background Pollution",
  "Factor 2" = "Mixed Industry",
  "Factor 3" = "Vehicle Exhaust",
  "Factor 4" = "Industrial Solvents",
  "Factor 5" = "Gasoline Evaporation"
)

factor_label_summer <- c(
  "Factor 1" = "Vehicle Exhaust",
  "Factor 2" = "Gasoline Evaporation",
  "Factor 3" = "Industrial Solvents",
  "Factor 4" = "Mixed Industry & Background"
)

factor_label_winter <- c(
  "Factor 1" = "Industrial Solvents",
  "Factor 2" = "Mixed Industry & Background",
  "Factor 3" = "Gasoline Evaporation",
  "Factor 4" = "Vehicle Exhaust"
)

## All sites
pmf_factors <- get_pmf_factors("allsites") %>%
  mutate(
    Factor = factor_label_allsites[factor],
    Factor = factor(Factor, levels = factor_levels),
    factor_text = paste(Factor, total_mass_fac)
  ) %>%
  arrange(desc(total_mass_fac)) %>%
  mutate(factor_text = factor(factor_text, levels = unique(factor_text)))

# Figure 3 (main text)
plot_pmf_factors(pmf_factors, "allsites")

## Stationary Sites 
pmf_factors_stationary <- get_pmf_factors("stationary") %>%
  mutate(
    Factor = factor_label_stationary[factor],
    Factor = factor(Factor, levels = factor_levels),
    factor_text = paste(Factor, total_mass_fac)
  ) %>%
  arrange(desc(total_mass_fac)) %>%
  mutate(factor_text = factor(factor_text, levels = unique(factor_text)))


# Figure S8 (supplemental)
plot_pmf_factors(pmf_factors_stationary, "stationary", output_subdir = file.path("supplemental", "figures"))


## Seasonal ----

## Summer
# Summer-specific palette: reuse the harmonized colors for the three factors
# that stayed separate, add one new color for the merged category
factor_colors_summer <- c(
  factor_colors[c("Vehicle Exhaust", "Gasoline Evaporation", "Industrial Solvents")],
  "Mixed Industry & Background" = "#7C3AED"   # new color, distinct from the other 5
)

factor_levels_summer <- names(factor_colors_summer)

pmf_factors_summer <- get_pmf_factors("allsites", season = "summer") %>%
  mutate(
    Factor = factor_label_summer[factor],
    Factor = factor(Factor, levels = factor_levels_summer),   # use the summer-specific levels
    factor_text = paste(Factor, total_mass_fac)
  ) %>%
  arrange(desc(total_mass_fac)) %>%
  mutate(factor_text = factor(factor_text, levels = unique(factor_text)))

# Figure S9 (supplemental)
plot_pmf_factors(pmf_factors_summer, "allsites", season = "summer", output_subdir = file.path("supplemental", "figures"))

factor_colors_winter <- factor_colors_summer
## Winter
pmf_factors_winter <- get_pmf_factors("allsites", season = "winter") %>%
  mutate(
    Factor = factor_label_winter[factor],
    Factor = factor(Factor, levels = factor_levels_summer),   # use the summer-specific levels
    factor_text = paste(Factor, total_mass_fac)
  ) %>%
  arrange(desc(total_mass_fac)) %>%
  mutate(factor_text = factor(factor_text, levels = unique(factor_text)))

# Figure S10 (supplemental)
plot_pmf_factors(pmf_factors_winter, "allsites", season = "winter", output_subdir = file.path("supplemental", "figures"))


## Supplemental: species contribution, year-round vs. summer vs. winter ----

# Force identical VOC ordering across all three panels so rows align
# vertically when summer/winter sit side by side

p_species_year <- make_species_contrib_plot(
  pmf_factors, factor_colors, plot_title = "Year-Round"
)

p_species_summer <- make_species_contrib_plot(
  pmf_factors_summer, factor_colors_summer, plot_title = "Summer"
)

p_species_winter <- make_species_contrib_plot(
  pmf_factors_winter, factor_colors_winter, plot_title = "Winter"
) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Summer + winter share one collected legend (their palettes/levels match
# exactly -- factor_colors_winter is the same 4-category scale as
# factor_colors_summer), placed at the bottom of that row
bottom_row <- (p_species_summer + p_species_winter) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Year-round keeps its own separate 5-category legend, since its factor
# set doesn't match summer/winter's merged categories
species_contrib_comparison <- p_species_year / bottom_row +
  plot_layout(heights = c(1, 1.2)) 

# Figure S11 (supplemental)
ggsave(
  filename = here("results", "supplemental", "figures", "species_contrib_seasonal_comparison.png"),
  plot = species_contrib_comparison,
  width = 11, height = 16, dpi = 320
)
# Main PMF Analysis: Contributions to Sites ------

pmf_contribs <- read_excel("results/interim_results/pmf_results/allsites_contributions_bs_fpeak.xlsx") %>%
  filter(!is.na(site_id2))



site_contribs <- pmf_contribs %>%
  group_by(site_id2) %>%
  summarize(
    total_mass = sum(factor_1) + sum(factor_2) + sum(factor_3) + sum(factor_4) + sum(factor_5),
    factor_1 = 100*sum(factor_1)/total_mass,
    factor_2 = 100*sum(factor_2)/total_mass,
    factor_3 = 100*sum(factor_3)/total_mass,
    factor_4 = 100*sum(factor_4)/total_mass,
    factor_5 = 100*sum(factor_5)/total_mass
  ) %>%
  ungroup() %>%
  pivot_longer(
    factor_1:factor_5, names_to = "factor", values_to = "Contribution"
  ) %>%
  mutate(
    # NOTE: keys here are "factor_1".."factor_5" (underscore), while
    # factor_label_allsites above is keyed by "Factor 1".."Factor 5" (space,
    # capital F) as read from the PMF Excel sheet names. Reformat the key
    # rather than duplicating the mapping a second time.
    Factor = factor_label_allsites[str_replace(factor, "factor_", "Factor ")],
    Factor = factor(Factor, levels = factor_levels),
    site_id = as.numeric(str_remove_all(site_id2, "site_")),
    # Replace negative contributions with zero (since these are medians
    # from bootstrapped samples, sometimes a negative can happen)
    Contribution = case_when(Contribution < 0 ~ 0, TRUE ~ Contribution)
  ) %>%
  left_join(., site_info, by = "site_id") %>%
  mutate(
    industrial_traffic = interaction(paste0(site_traffic," Traffic"),industrial_20, sep = " "),
    site_label = if_else(
      site_type == "stationary",
      paste0("**", site_id, "**"),  # Markdown bold
      as.character(site_id)
    )
  )



site_contrib_plot <- site_contribs %>%
  ggplot() + 
  aes(x = site_label, y = Contribution, fill = Factor) + 
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") + 
  coord_flip() + 
  facet_wrap(~industrial_traffic, scales = "free_y", space = "free_y") +
  theme_minimal() +
  # Reuse the named factor_colors palette (keyed by label) instead of a
  # positional hex vector, so colors can't silently drift out of sync with
  # `Factor`'s level order the way an unnamed vector would allow.
  scale_fill_manual(values = factor_colors) + 
  scale_y_continuous(expand = c(0,0)) +
  labs(x =  "Site",y = "Contribution (%)") +
  paper_theme + 
  guides(fill = guide_legend(nrow = 2)) + 
  theme(
    legend.position = "bottom",
    axis.text.y = element_markdown(),
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.spacing = unit(0.5, "lines"),
    plot.margin = margin(l = 10, t = 10, r = 10, unit = "pt") 
  )


# Figure 4 (main text)
ggsave(
  file.path("results", "figures", "sites_factors_allsites.png"),
  plot = site_contrib_plot,
  width = unit(8, "in"),
  height = unit(8, "in"),
  dpi = 320
)

site_ts <- pmf_contribs %>%
  pivot_longer(
    factor_1:factor_5, names_to = "factor", values_to = "cont"
  ) %>%
  mutate(
    Factor = factor_label_allsites[str_replace(factor, "factor_", "Factor ")],
    Factor = factor(Factor, levels = factor_levels),
    site_id = as.numeric(str_remove_all(site_id2, "site_")),
    # NOTE: assumes end_date is a fixed-width "MM/DD/YY" string (positions
    # 1-2 = month, 4-5 = day, 7-8 = year). This will silently mis-parse if
    # the upstream date format ever changes (e.g. single-digit months/days
    # without zero-padding, or a different separator/column width).
    date = lubridate::make_date(year = paste0("20",str_sub(end_date,7,8)),
                                month = str_sub(end_date,1,2),
                                day = str_sub(end_date,4,5))
  ) %>%
  left_join(., site_info, by = "site_id") %>%
  group_by(site_id, end_date) %>%
  mutate(norm_cont = 100*cont/sum(cont)) %>%
  ungroup() %>%
  mutate(
    industrial_traffic = interaction(paste0(site_traffic," Traffic"),industrial_20, sep = " "),
    site_long = paste0(site, "\n", industrial_traffic)
  ) %>%
  mutate(    site_long = factor(
    site_long,
    levels = site_long %>%
      tibble(site_long = ., industrial_traffic = industrial_traffic) %>%
      distinct() %>%
      arrange(industrial_traffic, site_long) %>%
      pull(site_long)
  ))

site_ts_plot <- site_ts %>%
  filter(site_id < 10) %>%
  ggplot() + 
  geom_line(aes(x = date, y = norm_cont, color = Factor)) + 
  # Reuse the named factor_colors palette (see note above on site_contrib_plot)
  scale_color_manual(values = factor_colors) + 
  facet_wrap(~site_long) + 
  labs(
    x = "Date",
    y = "Relative Contribution (%)"
  ) + 
  paper_theme + 
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1,
      vjust = 1
    ),
    strip.text = element_text(lineheight=0.3, size = 30)
  ) + 
  scale_x_date(date_labels = "%b %y")


# Figure S6 (supplemental)
ggsave(
  file.path("results", "supplemental", "figures", "sites_factors_timeseries.png"),
  plot = site_ts_plot,
  width = unit(8, "in"),
  height = unit(8, "in"),
  dpi = 320
)
