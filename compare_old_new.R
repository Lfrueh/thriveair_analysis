dat_new <- read_csv(here("data","clean","dat_ppb.csv")) %>%
  mutate(siteweek = paste0(site,week)) %>%
  select(-ends_with("_unc"))
dat_old <- read_csv(here("data","dat_ppb_old.csv")) %>%
  mutate(siteweek = paste0(site,week)) %>%
  rename(
    ethylbenzene = etbenz,
    ethylbenzene_flag = etbenz_flag,
    m_p_xylene_2 = mpxylene,
    m_p_xylene_2_flag = mpxylene_flag,
    o_xylene = oxylene,
    o_xylene_flag = oxylene_flag,
    trichloromonofluoromethane = trichlorofluoromethane,
    trichloromonofluoromethane_flag = trichlorofluoromethane_flag,
    tetrachloroethylene = tce,
    tetrachloroethylene_flag = tce_flag,
    benzene_1_ethyl_2_methyl = benz1ethyl2methyl,
    benzene_1_ethyl_2_methyl_flag = benz1ethyl2methyl_flag,
    benzene_1_ethyl_3_methyl = benz1ethyl3methyl,
    benzene_1_ethyl_3_methyl_flag = benz1ethyl3methyl_flag,
    benzene_1_ethyl_4_methyl = benz1ethyl4methyl,
    benzene_1_ethyl_4_methyl_flag = benz1ethyl4methyl_flag,
    n_hexane = hexane,
    n_hexane_flag = hexane_flag,
    benzene_1_2_dichloro = benz12chloro,
    benzene_1_2_dichloro_flag = benz12chloro_flag,
    hexane_2_methyl = hexane2methyl,
    hexane_2_methyl_flag = hexane2methyl_flag,
    heptane_3_methyl = heptane3methyl,
    heptane_3_methyl_flag = heptane3methyl_flag,
    benzene_propyl = propylbenzene,
    benzene_propyl_flag = propylbenzene_flag
  )
vars <- voc_vars
tol  <- 0.10


test <- left_join(dat_new, dat_old, by = "siteweek") %>%
  rename_with(~ sub("\\.x$", "_new", .x), ends_with(".x")) %>%
  rename_with(~ sub("\\.y$", "_old", .x), ends_with(".y"))

# create discrepancy flags and ratios
for (v in vars) {
  new_col <- paste0(v, "_new")
  old_col <- paste0(v, "_old")
  dis_col <- paste0(v, "_dis")
  ratio_col <- paste0(v, "_ratio")
  new_flag_col <- paste0(v, "_flag_new")
  old_flag_col <- paste0(v, "_flag_old")
  
  # Calculate ratio: old/new, rounded to 2 decimal places
  test[[ratio_col]] <- round(test[[old_col]] / test[[new_col]], 2)
  
  # Create discrepancy flag
  test[[dis_col]] <-
    ifelse(
      abs(test[[new_col]] - test[[old_col]]) > tol,
      "CHECK",
      NA_character_
    )
}

# ---- FORCE COLUMN ORDER ----
# VOC blocks in exact order: old conc → new conc → old flag → new flag → ratio → dis
voc_block <- unlist(lapply(vars, function(v) {
  c(
    paste0(v, "_old"),           # old concentration first
    paste0(v, "_new"),           # new concentration second
    paste0(v, "_flag_old"),      # old flag third
    paste0(v, "_flag_new"),      # new flag fourth
    paste0(v, "_ratio"),         # ratio fifth
    paste0(v, "_dis")            # discrepancy last
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

onlychecks <- test_withinfo %>%
  filter(if_any(ends_with("_dis"), ~ grepl("CHECK", .)))

write_excel_csv(onlychecks, "compare_data_checksonly.csv")

# Get all _dis column names
dis_cols <- names(test_withinfo) %>% str_subset("_dis$")

# Find which ones have NO "CHECK" in any row
no_check_cols <- dis_cols[sapply(dis_cols, function(col) {
  !any(grepl("CHECK", test_withinfo[[col]]))
})]

# Print them
print(no_check_cols)



# Sort and remove _dis columns
onlychecks_formatted <- onlychecks %>%
  arrange(analysis_batch_date, site_id) %>%
  select(-ends_with("_dis"))

# Create workbook
wb <- createWorkbook()
addWorksheet(wb, "Checks")

# Write data
writeData(wb, "Checks", onlychecks_formatted)

# Get all _ratio column positions
ratio_cols <- names(onlychecks_formatted) %>% str_subset("_ratio$")
ratio_col_indices <- which(names(onlychecks_formatted) %in% ratio_cols)

# Define styles
style_dark_blue <- createStyle(bgFill = "#4472C4", fontColour = "#FFFFFF")
style_light_blue <- createStyle(bgFill = "#B4C7E7")
style_green <- createStyle(bgFill = "#C6EFCE")
style_light_red <- createStyle(bgFill = "#FFC7CE")

# Apply conditional formatting to each _ratio column
for (col_idx in ratio_col_indices) {
  col_letter <- int2col(col_idx)
  data_rows <- 2:(nrow(onlychecks_formatted) + 1)  # +1 for header row
  
  # 0.92-0.94 = dark blue
  conditionalFormatting(
    wb, "Checks",
    cols = col_idx,
    rows = data_rows,
    rule = "AND($<<col>><<row>>>=0.92, $<<col>><<row>><=0.94)",
    style = style_dark_blue
  )
  
  # 0.95-0.99 = light blue
  conditionalFormatting(
    wb, "Checks",
    cols = col_idx,
    rows = data_rows,
    rule = "AND($<<col>><<row>>>=0.95, $<<col>><<row>><=0.99)",
    style = style_light_blue
  )
  
  # 1 = green
  conditionalFormatting(
    wb, "Checks",
    cols = col_idx,
    rows = data_rows,
    rule = "$<<col>><<row>>=1",
    style = style_green
  )
  
  # > 1 = light red
  conditionalFormatting(
    wb, "Checks",
    cols = col_idx,
    rows = data_rows,
    rule = "$<<col>><<row>>>1",
    style = style_light_red
  )
}

# Auto-size columns for better readability
setColWidths(wb, "Checks", cols = 1:ncol(onlychecks_formatted), widths = "auto")

# Freeze first row
freezePane(wb, "Checks", firstRow = TRUE)

# Save workbook
saveWorkbook(wb, "compare_data_checksonly.xlsx", overwrite = TRUE)