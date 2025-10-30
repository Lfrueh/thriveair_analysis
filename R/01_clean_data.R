library(readxl)
library(tidyverse)


#Codebook ----
codebook <- read_excel('data/codebook.xlsx')
voc_vars <- codebook %>% 
  filter(variable_name != "btex") %>% pull(variable_name)


sitenames <- read_excel("data/sitenames.xlsx") %>% 
  rename("site" = "name_new") %>%
  select(-name_long, -name_old)

extra_vocs <- read_excel("data/raw/extravocs_0825.xlsx") %>%
  janitor::clean_names() %>%
  rename_with(~ str_replace(., "_1$", "_flag")) %>%
  select(file_id, butane_2_methyl:benzene_1_4_diethyl_flag)

rawdata <- read_excel("data/raw/rawdata.xlsx") %>%
  select(-coordinates) %>%
  janitor::clean_names() %>%
  select(file_id, q_edit_method, date_return_shipped_to_id, site_id:time_difference) %>%
  mutate(q_edit_method = str_remove(q_edit_method, " ")) %>%
  left_join(., extra_vocs, by = "file_id") %>%
  select(-starts_with("user"), -comments, -time_difference, -tracking) %>%
  left_join(., sitenames, by="site_id") %>%
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
  ungroup() 

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

# Save coordinates to create basemap
coords <- cleandata_sample %>% filter(!is.na(lat)) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  st_coordinates()

write_rds(coords, "data/clean/coords.rds")

