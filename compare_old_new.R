dat_new <- read_csv(here("data","clean","dat_ppb.csv")) %>%
  mutate(siteweek = paste0(site,week)) %>%
  select(-ends_with("_unc"))

dat_old <- read_csv(here("data","dat_ppb_old.csv")) %>%
  mutate(siteweek = paste0(site,week)) %>%
  rename(
    ethylbenzene = etbenz,
    m_p_xylene_2 = mpxylene,
    o_xylene = oxylene,
    trichloromonofluoromethane = trichlorofluoromethane,
    tetrachloroethylene = tce,
    benzene_1_ethyl_2_methyl = benz1ethyl2methyl,
    benzene_1_ethyl_3_methyl = benz1ethyl3methyl,
    benzene_1_ethyl_4_methyl = benz1ethyl4methyl,
    n_hexane = hexane,
    benzene_1_2_dichloro = benz12chloro,
    hexane_2_methyl = hexane2methyl,
    heptane_3_methyl = heptane3methyl,
    benzene_propyl = propylbenzene
  )


vars <- voc_vars
tol  <- 0.05

# join + rename
test <- left_join(dat_new, dat_old, by = "siteweek") %>%
  rename_with(~ sub("\\.x$", "_new", .x), ends_with(".x")) %>%
  rename_with(~ sub("\\.y$", "_old", .x), ends_with(".y"))

# keep only VOCs that actually exist in both datasets
vars <- intersect(
  voc_vars,
  sub("_new$", "", names(test)[grepl("_new$", names(test))])
)

# create discrepancy flags
for (v in vars) {
  new_col <- paste0(v, "_new")
  old_col <- paste0(v, "_old")
  dis_col <- paste0(v, "_dis")
  new_flag_col <- paste0(v, "_flag_new")
  old_flag_col <- paste0(v, "_flag_old")
  
  test[[dis_col]] <-
    ifelse(
      abs(test[[new_col]] - test[[old_col]]) > tol,
      "CHECK",
      NA_character_
    )
}

# ---- FORCE COLUMN ORDER ----

# VOC blocks in exact order: new → old → dis
voc_block <- unlist(lapply(vars, function(v) {
  c(
    paste0(v, "_new"),
    paste0(v, "_old"),
    paste0(v, "_dis"),
    paste0(v, "_flag_new"),
    paste0(v, "_flag_old")
  )
}))

# drop any that don't exist
voc_block <- voc_block[voc_block %in% names(test)]

# final column order
test <- test %>%
  select(
    siteweek,
    voc_block
  )


test_withinfo <- dat_new %>% 
  select(siteweek,site_id:sample_length) %>%
  left_join(., test, by = "siteweek")

write_excel_csv(test_withinfo, "compare_data.csv")