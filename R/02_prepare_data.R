library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(stringi)



# This code takes raw GC-MS output and uses diffusion rates to get concentrations.
# Uncertainty and detection flags are also estimated for each measurement.
# Finally, data are cleaned and combined with contextual information for analysis.

# Get Data ----------------------------------------------------------------
codebook <- read_excel(here("data", "codebook.xlsx")) %>%
  filter(!variable_name %in% c("btex", "xylenes"))

voc_vars <- codebook %>%
  pull(variable_name)

unit_vars <- paste0(voc_vars, "_unit")

site_info <- read_csv("data/site_info.csv") %>%
  mutate(site = stri_enc_toutf8(site))

# Prep Concentrations Data ------------------------------------------------

## GC/MS Data ----

raw_tenax <- read_excel(here("data", "raw", "gcms_data.xlsx")) %>%
  janitor::clean_names() %>%
  rename(file_name = x2) %>%
  # Get rid of empty columns that were brought in with excel reading
  select(-x1) 


clean_compound_names <- function(data){
  old_names <- names(data)
  
  new_names <- ifelse(
    # If no info in the second row, then just keep old name
    is.na(data[1, ]),
    old_names,
    ifelse(
      # If "amount" is in the second row, then take off variable suffix
      str_detect(data[1, ], regex("Amount", ignore_case = TRUE)),
      str_replace(old_names, "_[0-9]+$", ""),
      ifelse(
        # If "unit" is in the second row, then add _unit as suffix
        str_detect(data[1, ], regex("Unit", ignore_case = TRUE)),
        str_replace(old_names, "_[0-9]+$", "_unit"),
        old_names
      )
    )
  )
  
  names(data) <- new_names
  
  return(data)
  
}


# Keep only compounds of interest for this project
raw_tenax <- raw_tenax %>%
  clean_compound_names(.) %>%
  select(file_name, all_of(voc_vars), all_of(unit_vars))

# Check that all of the units are in nL (otherwise would have to convert)
unit_tbl <- raw_tenax %>% summarize(across(all_of(unit_vars), ~ toString(unique(.)))) %>%
  pivot_longer(everything(), names_to = "compound", values_to = "unit")

# Because all units are the same, we can drop them from the dataset
raw_tenax <- raw_tenax %>%
  select(-all_of(unit_vars))


# Figure out where the start of data is
start_line <- which(grepl("^start of data", raw_tenax[[1]], ignore.case = TRUE))
# remove rows 1 through that line (assuming you want to start *after* it)
if (length(start_line) > 0) {
  tenax <- raw_tenax[-(1:start_line), ]
}

# Make identifier columns
tenax <- tenax %>%
  rowwise() %>%
  mutate(
    tube_number = word(file_name, 1, sep = " "),
    analysis_batch_date = mdy(word(file_name, 2, sep = " ")),
    sample_end_date = mdy(word(file_name, 3, sep = " ")),
    across(all_of(voc_vars), ~as.numeric(.x))
  ) %>%
  relocate(tube_number, analysis_batch_date, sample_end_date) %>%
  ungroup() 


## Sample information ----
samples <- read_csv(here("data", "raw", "sampling_sheet.csv"),
                    col_types = cols(
                      `START TIME` = col_character(),
                      `END TIME` = col_character()
                    )) %>%
  janitor::clean_names() %>%
  rename(file_name_sampling = file_name_tube_number_today_date_end_date_location_fb_lab_blank_dup_etc,
         site_id = location_number) %>%
  filter(tolower(sample_type) %in% c("sample", "duplicate"),
         is.na(use_sample)) %>%
    rowwise() %>%
    mutate(
      tube_number = word(file_name_sampling, 1, sep = " "),
      analysis_batch_date = mdy(word(file_name_sampling, 2, sep = " ")),
      sample_end_date = mdy(word(file_name_sampling, 3, sep = " ")),
      sample_type = tolower(sample_type),
      start_date = mdy(start_date),
      end_date = mdy(end_date),
      start_time = parse_date_time(start_time, "%I:%M:%S %p"),
      end_time = parse_date_time(end_time, "%I:%M:%S %p"),
      # Combine: add the time component to the actual date
      start_datetime = start_date + 
        hours(hour(start_time)) + 
        minutes(minute(start_time)) + 
        seconds(second(start_time)),
      end_datetime = end_date + 
        hours(hour(end_time)) + 
        minutes(minute(end_time)) + 
        seconds(second(end_time)),
      time_difference = as.numeric(difftime(end_datetime, start_datetime, units = "mins")),
      sample_type = str_squish(tolower(sample_type)),
      # Create a unique week ID
      week = paste0(week(end_date),year(end_date)),
      #Create a variable that is # of weeks of sampling
      sample_length = week(end_date)-week(start_date)
    ) %>%
    relocate(tube_number, analysis_batch_date, sample_end_date) %>%
    filter(end_date > mdy("06/30/2023"),
           !is.na(site_id),
           site_id != "0",
           sample_length == 1) %>% # Filter out non-standard samples
    ungroup() %>%
    select(tube_number, file_name_sampling, analysis_batch_date, sample_end_date, 
           site_id, sample_type, start_date, end_date, time_difference,
           multiplier, week, sample_length)


## Combine sample info with measurements ----
vocs_raw <- left_join(samples, tenax, by = c("tube_number", "analysis_batch_date",
                                             "sample_end_date")) %>%
  relocate(file_name, .after = "file_name_sampling") %>%
  mutate(across(all_of(voc_vars), ~as.numeric(.x)))


## Diffusion rates, LODs, ULODs ----
utr_lods <- read_excel(here("data", "raw", "utr_blanks.xlsx"),
                              sheet = "Extended Drexel UTR sheet") %>%
  janitor::clean_names() %>%
  clean_compound_names(.) %>%
  select(utr_sheet, all_of(voc_vars)) %>%
  filter(utr_sheet %in% c("UTR 1 wk", "LOD raw", "MW")) %>%
  pivot_longer(cols = voc_vars, names_to = "variable") %>%
  pivot_wider(names_from = utr_sheet, values_from = value) %>%
  janitor::clean_names()


batch_dates <- vocs_raw %>% select(analysis_batch_date)  %>% distinct()

ulods <- read_excel(here("data", "raw", "utr_blanks.xlsx"),
                    sheet = "ULODs") %>%
  janitor::clean_names() %>%
  clean_compound_names(.) %>%
  select(earliest_batch_date, all_of(voc_vars)) %>%
  filter(!is.na(earliest_batch_date)) %>%
  mutate(earliest_batch_date = 
           mdy(earliest_batch_date)) %>%
  left_join(
    batch_dates,
    join_by(earliest_batch_date <= analysis_batch_date)
  ) %>%
  relocate(analysis_batch_date) %>%
  group_by(analysis_batch_date) %>%
  slice_max(earliest_batch_date, n = 1) %>%
  select(-earliest_batch_date) %>%
  pivot_longer(
    cols = voc_vars, 
    names_to = "variable",
    values_to = "ulod"
  ) %>% 
  ungroup()

blanks <- read_excel(here("data", "raw", "utr_blanks.xlsx"),
                     sheet = "blanks") %>%
  janitor::clean_names() %>%
  clean_compound_names(.) %>%
  select(earliest_batch_date, all_of(voc_vars)) %>%
  filter(!is.na(earliest_batch_date)) %>%
  mutate(earliest_batch_date = 
           mdy(earliest_batch_date)) %>%
  left_join(
    batch_dates,
    join_by(earliest_batch_date <= analysis_batch_date)
  ) %>%
  relocate(analysis_batch_date) %>%
  group_by(analysis_batch_date) %>%
  slice_max(earliest_batch_date, n = 1) %>%
  select(-earliest_batch_date) %>%
  pivot_longer(
    cols = voc_vars, 
    names_to = "variable",
    values_to = "blank"
  ) %>%
  mutate(blank = as.numeric(blank)) %>% 
  ungroup()



# Now we have an LOD, ULOD, and UTR for each compound and analysis date.
utr_lod_ulod_blank <- left_join(ulods, utr_lods, by = "variable") %>%
  left_join(., blanks, by = c("variable", "analysis_batch_date")) %>%
  mutate(across(c("utr_1_wk", "ulod", "lod_raw", "mw"), ~ as.numeric(.x)))


## Estimate uncertainties -----
# We will add in quadrature:
# Analytic error (estimate this at 10% since we don't have calibration curve uncertainty)
# Field sampling error (from duplicate field samples)
# Diffusion rate error (from repeat UTR experiments)

### Coefficient of variation for duplicate samples ---
dupes <- vocs_raw %>%
  group_by(site_id, week) %>%
  filter(n() > 1) %>%
  select(site_id, week, sample_type, any_of(voc_vars)) %>%
  pivot_wider(names_from = sample_type, values_from = all_of(voc_vars)) %>%
  ungroup()

# Calculate relative standard deviation from duplicate measurements
calc_cv <- function(voc, c1, c2){
  lod <- utr_lod_ulod_blank %>% 
    filter(variable == voc) %>%
    pull(lod_raw) %>%
    unique()
  
  keep <- (c1 > lod & c2 > lod)

  rd <- abs(c1[keep] - c2[keep]) / ((c1[keep] + c2[keep]) / 2)
  sqrt(0.5) * sqrt(mean(rd^2))
  
}

# Coefficient of variation for paired samples
dupe_cvs <- tibble(voc = voc_vars) %>%
  mutate(
    dupe_cv = map_dbl(
      voc,
      ~ calc_cv(
        voc = .x,
        c1  = dupes[[paste0(.x, "_sample")]],
        c2  = dupes[[paste0(.x, "_duplicate")]]
      )
    )
  )

### Diffusion rate uncertainties -----
utr_unc <- read_csv(here("data", "raw", "utr_7day_uncertainties.csv")) %>%
  # For compounds with insufficient uptake rate calibration data (n<3), 
  # we will leave the SE as blank.
  mutate(utr_se = case_when(
    n_trials < 3 ~ NA_real_,
    TRUE ~ utr_se
  )) %>%
  select(variable_name, utr_se)

# Note that some of these are blank, so we'll just replace with a formula
uncertainties <- left_join(dupe_cvs, utr_unc, by = c("voc"="variable_name"))

## Combine all the compound info ----
compound_info <- left_join(utr_lod_ulod_blank, uncertainties, by = c("variable"="voc")) %>%
  # For compounds with insufficient uptake rate calibration data (n<3), 
  # uptake rate uncertainties were estimated as 30% based on the 
  # median of relative uncertainty of compounds with complete calibration data.
  mutate(utr_se = case_when(
    is.na(utr_se) ~ 0.30*utr_1_wk,
    TRUE ~ utr_se)
  ) %>%
  # Calculate error fraction using relative SE from duplicate measurements
  # and relative SE of the diffusion rate
  # Add 10% relative uncertainty for missing analytic uncertainty info
  mutate(
    prop_unc = sqrt((utr_se/utr_1_wk)^2 + dupe_cv^2 + 0.1^2)
  )

## Generate Concentrations, flags, and uncertainties ----

process_voc_data <- function(batch_date, unit = "ppb"){
  
  df <- vocs_raw %>%
    filter(analysis_batch_date == batch_date)

  
  for (voc in voc_vars){
    
    # Get parameters for that batch and VOC
    params <- compound_info %>%
      filter(variable == voc,
             analysis_batch_date == batch_date) %>%
      select(-variable, -analysis_batch_date) %>%
      as.list()
    
    
    # Create dynamic column names to add a flag column
    flag_col <- paste0(voc, "_flag")
    unc_col <- paste0(voc, "_unc")

    df <- df %>%
      mutate(
        # Conversion factor to ppb using (1000/(UTR*time))
        conversion_factor = 1000 / (params$utr_1_wk * time_difference),
        # Blank-correct values: (raw amount * multiplier - blank)
        blank_corrected_value = (.data[[voc]]*multiplier) - params$blank,
        # Measurement flags
        !!flag_col := case_when(
          # Above upper limit, then flag ULOD
          .data[[voc]] > params$ulod ~ "ULOD",
          
          # Non-detects: raw value = 0 or blank-corrected value is negative
          (.data[[voc]] == 0) | (blank_corrected_value < 0) ~ "ND",
          
          # LOD: Below LOD but non-zero
          (.data[[voc]] > 0) & (.data[[voc]] < params$lod_raw) ~ "LOD",
          
          # All others: regular (including when blank-corrected == 0)
          TRUE ~ "REG"
        ),
        # Calculate uncertainty in nL (raw units)
        !!unc_col := case_when(
          # Uncertainty calculation is based on EPA technical documentation
          # Eq. 5-2 in:
          # https://www.epa.gov/sites/default/files/2015-02/documents/pmf_5.0_user_guide.pdf
          .data[[flag_col]] %in% c("ULOD", "LOD", "REG") ~ 
           sqrt((blank_corrected_value * params$prop_unc)^2 + params$lod_raw^2),
          # For non-detect values, we use equation 5-1
          .data[[flag_col]] == "ND" ~ (5/6) * params$lod_raw
        ),
        # Conver to ppb.
     !!voc := case_when(
       # Replace NDs with LOD/2, convert to ppb
       .data[[flag_col]] == "ND" ~ round((params$lod_raw/2 * conversion_factor), 4),
       # Replace >ULOD measurements with the ULOD, blank-correct and convert to ppb
       .data[[flag_col]] == "ULOD" ~ round(((params$ulod*multiplier) - params$blank)*conversion_factor,4),
       # Convert REG values to PPB
       TRUE ~ round(blank_corrected_value * conversion_factor, 4)
     ),
     # Convert uncertainty to ppb
     !!unc_col := round(.data[[unc_col]] * conversion_factor,4)
      ) %>%
      relocate(!!flag_col, .after = !!voc) %>%
      relocate(!!unc_col, .after = !!flag_col)
    
    # Convert to micrograms per meter cubed if specified in function arguments
    if (unit == "mgm3"){
      df <- df %>%
        mutate(
          !!voc := round(.data[[voc]] * params$mw / 24.45, 3),
          !!unc_col := round(.data[[unc_col]]*params$mw / 24.45, 3)
        )
      
    }

  }
  
  

  
  return(df)
  
  
  
}

clean_data <- function(df){
  
  dat <- left_join(site_info %>% mutate(site_id = as.character(site_id)) %>%
                     filter(site_id != "17"),
                  df, by = "site_id") %>%
    #Make sums for xylenes and BTEX
    mutate(
      xylenes = m_p_xylene_2 + o_xylene,
      xylenes_flag = case_when(
        m_p_xylene_2_flag == "ULOD" | o_xylene_flag == "ULOD" ~ "ULOD",
        TRUE ~ "REG"
      ),
      btex = benzene + toluene + ethylbenzene + m_p_xylene_2 + o_xylene,
      btex_flag = case_when(
        benzene_flag == "ULOD" | toluene_flag == "ULOD" & ethylbenzene_flag == "ULOD" | 
          m_p_xylene_2_flag == "ULOD" | o_xylene_flag == "ULOD" ~ "ULOD",
        TRUE ~ "REG"
      )
    ) %>%
    mutate(
      season = case_when(between(as.Date(start_date), 
                                 as.Date("2023-12-06"), as.Date("2024-02-14")) ~ "Winter",
                         between(as.Date(start_date), 
                                 as.Date("2023-08-09"), as.Date("2023-10-25")) ~ "Summer")
    )
  
  return(dat)
  
}


voc_ppb <- map_dfr(batch_dates$analysis_batch_date,
                   ~process_voc_data(.x)) 

voc_ppb <- clean_data(voc_ppb)


voc_mgm3 <- map_dfr(batch_dates$analysis_batch_date,
                    ~process_voc_data(.x, unit = "mgm3"))

voc_mgm3 <- clean_data(voc_mgm3)


# Save clean data -----
write_excel_csv(voc_ppb %>% filter(sample_type == "sample"), "data/clean/dat_ppb.csv")
write_excel_csv(voc_mgm3 %>% filter(sample_type == "sample"), "data/clean/dat_mgm3.csv")

# Create and save colo dataset -----
colo <- voc_ppb %>%
  group_by(site_id, week) %>%
  # Keep only co-located samples
  filter(n() > 1)

write_excel_csv(colo, "data/clean/colos.csv")


# Save data for PMF -----
dat_forpmf <- voc_ppb %>%
  # Replace zero values with 0.0001 to avoid PMF errors. 
  mutate(across(voc_vars, ~replace(.,.==0,0.0001))) %>%
  filter(sample_type == "sample") %>%
  mutate(site_id2 = paste0("site_", site_id)) %>%
  # Arrange by site and then date per EPA instructions
  arrange(site_id2, end_date)

conc_forpmf_all <- dat_forpmf %>%
  select(site_id2, end_date,  all_of(voc_vars)) 

conc_forpmf_stationary <- dat_forpmf %>%
  filter(site_type == "stationary") %>%
  select(site_id2, end_date, all_of(voc_vars)) 


unc_forpmf_all <- dat_forpmf %>%
  select(site_id2, end_date, all_of(paste0(voc_vars,"_unc"))) %>%
  rename_with(~str_replace(., "_unc", ""), ends_with("_unc"))


unc_forpmf_stationary <- dat_forpmf %>%
  filter(site_type == "stationary") %>%
  select(site_id2, end_date, all_of(paste0(voc_vars,"_unc"))) %>%
  rename_with(~str_replace(., "_unc", ""), ends_with("_unc"))  
  

write.xlsx(
  list(
    concentrations = conc_forpmf_all,
    uncertainties = unc_forpmf_all
  ),
  here("data", "clean", "allsite_pmf.xlsx")
)

write.xlsx(
  list(
    concentrations = conc_forpmf_stationary,
    uncertainties = unc_forpmf_stationary
  ),
  here("data", "clean", "stationary_pmf.xlsx")
)










