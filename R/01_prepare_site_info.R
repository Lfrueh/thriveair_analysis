library(readxl)
library(tidyverse)
library(sf)



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
  ) %>%
  # Keep coordinates
  separate(coordinates, into = c("lat", "long"), sep = ",", remove = FALSE)

write_csv(site_info, "data/site_info.csv")


# Coordinates for Basemap -------------------------------------------------
coords <- site_info %>% filter(!is.na(lat)) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  st_coordinates()

write_rds(coords, "data/raw/coords.rds")

