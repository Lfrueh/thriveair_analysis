library(readxl)
library(tidyverse)
library(here)

litreview_raw <- read_excel(here("data", "raw", "btex_litsummary.xlsx"))


# Delete before publication since this isn't part of the analysis

litreview <- litreview_raw %>%
  janitor::clean_names() %>%
  mutate(across(benzene:o_xylene, ~round(.x, 2))) %>%
  mutate(total_xylenes = m_p_xylenes + o_xylene,
         btex_ratio = paste0(
           round(benzene/ethylbenzene,1), " : ",
           round(toluene/ethylbenzene,1), " : ",
           round(ethylbenzene/ethylbenzene,1), " : ",
           round(total_xylenes/ethylbenzene,1)
         ),
         t_b_ratio = round(toluene/benzene,1),
         m_e_ratio = round(m_p_xylenes/ethylbenzene,1),
  )