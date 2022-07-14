library(readxl)
library(tidyverse)
library(janitor)

#Load custom functions
source("R/convert_unit_functions.R")

taillardat_ts <- read_excel("~/Desktop/taillardat/Taillardat et al (JGRB 2022) Figure4c_Figure8_CO2CH4timeseries.xlsx")  %>% 
  clean_names() %>% 
  rename(co2_umol_l_other = co2_umol_l,
         ch4_umol_l_other = ch4_umol_l)

names(taillardat_ts)

taillardat_ts <- taillardat_ts %>%
  mutate(henrys_co2_ours = getKh(temp_deg_c + 273.15, "CO2"),
         henrys_ch4_ours = getKh(temp_deg_c + 273.15, "CH4"),
         co2_umol_l_ours =  henrys_co2_ours*co2_ppm/pressure_atm,
         ch4_umol_l_ours =  henrys_ch4_ours*ch4_ppm/pressure_atm)

ggplot(taillardat_ts)+
  geom_line(aes(temp_deg_c, (henrys_constant_ch4_mol_dm_3_atm_1 - henrys_ch4_ours )/henrys_ch4_ours*100))

ggplot(taillardat_ts)+
  geom_line(aes(temp_deg_c, (henrys_constant_co2_mol_dm_3_atm_1 - henrys_co2_ours )/henrys_co2_ours*100))


ggplot(taillardat_ts)+
  geom_line(aes(number, co2_umol_l_ours), alpha= .5)+
  geom_line(aes(number, co2_umol_l_other), color= "red", alpha=.5)

ggplot(taillardat_ts)+
  geom_line(aes(number, ch4_umol_l_ours), alpha= .5)+
  geom_line(aes(number, ch4_umol_l_other), color= "red", alpha=.5)


taillardat_ts %>% 
  mutate(ch4_umol_l_other_div = henrys_constant_ch4_mol_dm_3_atm_1*ch4_ppm/pressure_atm ) %>% 
ggplot()+
  geom_line(aes(temp_deg_c, ch4_umol_l_ours - ch4_umol_l_other), alpha= .5)+
  geom_line(aes(temp_deg_c, ch4_umol_l_ours - ch4_umol_l_other_div), color= "red", alpha= .5)
