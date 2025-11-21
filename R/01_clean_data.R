library(readxl)
library(tidyverse)



# Codebook ----------------------------------------------------------------
codebook <- read_excel('data/codebook.xlsx')
voc_vars <- codebook %>% 
  filter(variable_name != "btex") %>% pull(variable_name)


# Merge Site Information --------------------------------------------------
# Site names and coordinates
sitenames <- read_excel("data/raw/sitenames.xlsx") %>% 
  rename("site" = "name_new") %>%
  select(-name_long, -name_old)


## Point sources -----------------------------------------------------------

pointsources <- read_excel("data/raw/pointsources.xlsx")


## Land Use Assignments ---------------------------------------------------
# Land use was downloaded from OpenDataPhilly. 
# In ArcGIS Pro: Each site was assigned a 100m buffer, 
# and the area (sqm) of each landuse category (1-digit) was summarized.
# The result is saved as landuse_100mbuffer.csv

polys <- read_csv("data/raw/landuse_100mbuffer.csv")
landuse_key <- read_excel("data/raw/landuse_key.xlsx")

polys <- polys %>%
  filter(c_dig1desc != 5) %>%
  left_join(., landuse_key, by = "c_dig1desc") %>%
  group_by(site_id) %>%
  mutate(
    tot_area = sum(area),
    area_pt = 100*area / tot_area
  ) %>%
  ungroup() %>%
  select(-poly_count)

# Assign majority land-use category (area >50% of 100m buffer)
# If none achieve 50%, then assign "Mixed Use"
# Then make simpler category of "Industry vs. Non-Industry" based on if Industry > 20% of buffer

landuse_assignments <- left_join(polys, sitenames, by = "site_id") %>%
  select(-area, -c_dig1desc) %>%
  mutate(landuse = case_when(
    # Re-define water as open space
    landuse == "Water" ~ "Open Space",
    TRUE ~ landuse
  )) %>%
  group_by(site_id, landuse) %>%
  mutate(area_pt = sum(area_pt)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(site_id) %>%
  # Make industry vs. not variable
  mutate(
    # Flag industrial if industrial >25%
    ind = if_else(landuse == "Industrial" & area_pt > 20, 1, 0),
    # Simplified category
    industrial_20 = if_else(sum(ind) > 0, "Industrial", "Non-Industrial")
  ) %>%
  # Make specific land use categories
  slice_max(., order_by = area_pt) %>%
  mutate(
    land_use = if_else(area_pt > 50, landuse, "Mixed Use")
  ) %>%
  select(-ind, -landuse, -area_pt, -tot_area) %>%
  ungroup()

write_csv(landuse_assignments, "data/raw/landuse_assignments.csv")

## Traffic Assignments ----------------------------------------------------
# Traffic was assigned using weighted road density using the method
# outlined in Rose, 2009. For each 30m grid cell, road type segments
# were weighted by length of road segment with each road traffic hierarchy.
# Each site was categorized as low vs. high traffic.

traffic_assignments <- read_excel("data/raw/site_traffic.xlsx")

# Join together and export site info
site_info <- left_join(landuse_assignments, traffic_assignments, by = "site_id") %>%
  left_join(., pointsources, by = "site")


# Create bivariate category
site_info <- site_info %>%
  mutate(
    ind_traf = case_when(
      site_traffic == "Low" & industrial_20 == "Non-Industrial" ~ "LowT/NoI",
      site_traffic == "High" & industrial_20 == "Non-Industrial" ~ "HighT/NoI",
      site_traffic == "Low" & industrial_20 == "Industrial" ~ "LowT/I",
      site_traffic == "High" & industrial_20 == "Industrial" ~ "HighT/I",
      
    )
  )

write_csv(site_info, "data/site_info.csv")



# Clean VOC Data -----------------------------------------------------------

## Second batch of VOCs, to be joined on File ID
extra_vocs <- read_excel("data/raw/extravocs_0825.xlsx") %>%
  janitor::clean_names() %>%
  rename_with(~ str_replace(., "_1$", "_flag")) %>%
  select(file_id, butane_2_methyl:benzene_1_4_diethyl_flag)

## Original batch of VOCs
rawdata <- read_excel("data/raw/rawdata.xlsx") %>%
  # Will use clean coordinates from site_info instead
  select(-coordinates) %>%
  janitor::clean_names() %>%
  select(file_id, q_edit_method, date_return_shipped_to_id, site_id:time_difference) %>%
  mutate(q_edit_method = str_remove(q_edit_method, " ")) %>%
  left_join(., extra_vocs, by = "file_id") %>%
  select(-starts_with("user"), -comments, -time_difference, -tracking) %>%
  # Join in site info
  left_join(., site_info, by="site_id") %>%
  separate(coordinates, into = c("lat", "long"), sep = ",", remove = FALSE) %>%
#Move stuff around
  relocate(site) %>%
  relocate(benzene, benzene_flag, toluene, toluene_flag, etbenz, etbenz_flag,
           mpxylene,	mpxylene_flag,	oxylene,	oxylene_flag, .after = end_date) %>%
#Make sums for xylenes and BTEX
  mutate(
    xylenes = mpxylene + oxylene,
    xylenes_flag = case_when(
      mpxylene_flag == "ULOD" | oxylene_flag == "ULOD" ~ "ULOD",
      TRUE ~ "REG"
    ),
    btex = benzene + toluene + etbenz + mpxylene + oxylene,
    btex_flag = case_when(
      benzene_flag == "ULOD" | toluene_flag == "ULOD" & etbenz_flag == "ULOD" |mpxylene_flag == "ULOD" | oxylene_flag == "ULOD" ~ "ULOD",
      TRUE ~ "REG"
    )
    ) %>%
  relocate(xylenes, xylenes_flag, .after = etbenz_flag) %>%
  relocate(btex, btex_flag, .after = xylenes_flag) %>%
  select(-location, -coordinates) %>%
  mutate(
    season = case_when(between(as.Date(start_date), 
                               as.Date("2023-12-06"), as.Date("2024-02-14")) ~ "Winter",
                       between(as.Date(start_date), 
                               as.Date("2023-08-09"), as.Date("2023-10-25")) ~ "Summer")
  )


cleandata <- rawdata %>%
  filter(!is.na(site_id) & !is.na(site)) %>% # filter out a few non-standard samples
  # Create a week number
  # Note that some samples were up for 2 weeks--exclude these since LOD is determined for a one-week sample (N = 16)
  mutate(
    #Create a unique week ID
    week = paste0(week(end_date),year(end_date)),
    #Create a variable that is # of weeks of sampling
    sample_length = week(end_date)-week(start_date)
  ) %>%
  #Keep only one-week samples
  filter(sample_length == 1) %>%
  #Calculate total weeks sampled to weight in PCA
  group_by(site_id) %>%
  mutate(
    tot_weeks = n_distinct(week)
  ) %>%
  ungroup() %>%
  # Filter out the June 2023 date since we only sampled once that month
  filter(as.Date(end_date) > as.Date("2023-07-01"))

cleandata_colo <- cleandata %>%
  group_by(site, week) %>%
  # Keep only co-located samples
  filter(n() > 1)

cleandata_sample <- cleandata %>% 
  filter(str_detect(tolower(sample), "sample")) %>%
  select(-sample) %>%
  mutate(across(all_of(voc_vars), as.numeric))

# Reshape to long, convert, reshape back
cleandata_mgm3 <- cleandata_sample %>%
  pivot_longer(cols = all_of(voc_vars), 
               names_to = "variable_name", 
               values_to = "value") %>%
  left_join(codebook, by = "variable_name") %>%
  mutate(value = ifelse(!is.na(value),
                        round(value * mw / 24.45, 3),
                        value)) %>%
  select(-mw) %>%
  pivot_wider(id_cols = setdiff(names(cleandata_sample), voc_vars),
              names_from = variable_name, values_from = value)
    

#Write clean dataset
write.csv(cleandata_sample, "data/clean/dat_ppb.csv")

write.csv(cleandata_mgm3, "data/clean/dat_mgm3.csv")

#Write colo_dataset
write.csv(cleandata_colo, "data/clean/colos.csv")


# Coordinates for Basemap -------------------------------------------------
coords <- cleandata_sample %>% filter(!is.na(lat)) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  st_coordinates()

write_rds(coords, "data/raw/coords.rds")







