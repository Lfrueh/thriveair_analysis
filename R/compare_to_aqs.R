library(tidyverse)
library(readxl)
library(patchwork)

#This is not even helpful...since the methods don't coincide at all. 
# just scrap it?? or....
# can't even average, since it's one 24-hour sample every 6 days. 
# Probably just going to take a peek at it and then delete

source("R/00_plot_theme.R")  

#Codebook ----
codebook <- read_excel('data/codebook.xlsx')
voc_vars <- codebook %>% 
  filter(variable_name != "btex") %>% pull(variable_name)

#Get data -----
vocs <- read_csv("data/clean/dat.csv", col_select = -1) %>% select(-ends_with('flag')) 

# Only have AQS data from 2023, as of 9/25/25
aqs23 <- read_csv("data/raw/ritner_aqs_2023.csv")

# BTEX -----
btex <- vocs %>%
  filter(site_id == 8, end_date < as.Date("2023-12-31")) %>%
  select(end_date, benzene, toluene, etbenz, oxylene, mpxylene) %>%
  pivot_longer(benzene:mpxylene, names_to = "variable_name", values_to = "concentration") %>%
  left_join(., codebook, by = "variable_name")

btex_plot <- btex %>%
  ggplot(aes(x = as.Date(end_date), y = concentration, color = voc_name)) + 
  geom_line()

btex_plot

btex_aqs <- aqs23 %>%
  filter(parameter_name %in% c("Benzene", "Toluene", "Ethylbenzene", "o-Xylene", "m/p Xylene"),
         date_local <= as.Date("2023-12-20")) 

btex_aqs_plot <- btex_aqs %>%
  ggplot(aes(x = as.Date(date_local), y = arithmetic_mean, color = parameter_name)) + 
  geom_point()

btex_aqs_plot


# make datasets comparable
df1 <- aqs23 %>%
  filter(
    # parameter_name %in% c("Benzene", "Toluene", "Ethylbenzene", "o-Xylene", "m/p Xylene",
    #                            "Chloroform", "1,3,5-Trimethylbenzene"),
         date_local <= as.Date("2023-12-20"),
         date_local >= as.Date("2023-06-20")) %>%
  group_by(date_local, parameter_name) %>%
  summarize(mean_val = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = parameter_name, values_from = mean_val) %>%
  janitor::clean_names() %>%
  mutate(tb_ratio = toluene / benzene,
         oxe_ratio = o_xylene / ethylbenzene,
         pxe_ratio = m_p_xylene / ethylbenzene,
         etbenz = ethylbenzene,
         oxylene = o_xylene,
         mpxylene = m_p_xylene,
         trichloromethane = chloroform,
         mesitylene = x1_3_5_trimethylbenzene,
         hexane = n_hexane,
         heptane = n_heptane,
         source = "aqs",
         date = date_local) 

df2 <- vocs %>%
  filter(site_id == 8, end_date < as.Date("2023-12-31")) %>%
  mutate(tb_ratio = toluene / benzene,
         oxe_ratio = oxylene / etbenz,
         pxe_ratio = mpxylene / etbenz,
         source = "site8",
         date = end_date)

# bind rows
combined <- bind_rows(df1, df2)

# plot
ggplot(combined, aes(x = date, y = tb_ratio, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = oxe_ratio, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = pxe_ratio, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = trichloromethane, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = trichlorofluoromethane, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = mesitylene, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = pxe_ratio, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = heptane, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = hexane, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = mesitylene, color = source)) +
  geom_line(linewidth = 2) 

ggplot(combined, aes(x = date, y = mesitylene, color = source)) +
  geom_line(linewidth = 2) 


#X/E relationship is unexpected

ebplot <- ggplot(combined) +
  geom_line(aes(x = date, y = etbenz, color = source), linewidth = 1.5) 

mpxplot <- ggplot(combined) + 
  geom_line(aes(x = date, y = mpxylene, color = source), linewidth = 1.5, linetype = 2) 

oxplot <- ggplot(combined) + 
  geom_line(aes(x = date, y = oxylene, color = source), linewidth = 1.5, linetype = 3)

ebplot / mpxplot / oxplot
 
benzplot <- ggplot(combined) +
  geom_line(aes(x = date, y = benzene, color = source, linetype = source), linewidth = 1.5) 

tolplot <- ggplot(combined) +
  geom_line(aes(x = date, y = toluene, color = source, linetype = source), linewidth = 1.5) 
  

tolplot / benzplot