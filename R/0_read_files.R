library(tidyverse)
library(readxl)
library(here)
library(patchwork)
library(corrr)

#Load custom functions
source("R/convert_unit_functions.R")

#Load unit conversion table
unit_convert_table <- readRDS("R/unit_convert_table.rds")

#load GIS data
gis <- read_csv(file.path(path_to_dropbox, "db_processingR", 
                          "methdb_explore", "data", 
                          "methdb_gis.csv")) %>% 
  rename(Site_Nid =site_nid) 

#load methdb papers table
papers <- read_excel(file.path(path_to_dropbox, MethDB_filename),
                    sheet = "Papers",  guess_max = 250)  %>% 
  drop_na(Publication_Nid) %>%
  select_if(~sum(!is.na(.)) > 0)

#rename columns
names(papers) <- gsub(" ", "", names(papers))


#load methdb site table
sites <- read_excel(file.path(path_to_dropbox, MethDB_filename),
                    sheet = "Sites", guess_max = 3500)  %>% 
  mutate(Site_Nid = as.character(Site_Nid)) %>%
  distinct(Site_Nid, .keep_all = TRUE)

#rename columns
names(sites) <- gsub(" ", "", names(sites))
names(sites) <- gsub("\\(m\\)", "_m", names(sites))

#load methdb concentration table and bind with elevation
concentrations <- read_excel(file.path(path_to_dropbox, MethDB_filename), 
                             sheet = "Concentrations", guess_max = 14000) %>% 
  mutate(Site_Nid = as.character(Site_Nid)) %>%
  left_join(select(sites, Site_Nid, Elevation_m))

#rename columns
names(concentrations) <- gsub(" ", "", names(concentrations))
  
#load methdb fluxes
fluxes <- read_excel(file.path(path_to_dropbox, MethDB_filename), 
                     sheet = "Fluxes", guess_max = 4000)

names(fluxes) <- gsub(" ", "", names(fluxes))


#Convert concentration and fluxes to uM and mmol m-2 d-1

concentrations_converted <- convert_conc_units(concentrations, unit_convert_table)
fluxes_converted <- convert_flux_units(fluxes, unit_convert_table)

save(concentrations_converted, fluxes_converted, 
     sites, papers, gis,
     file = file.path(path_to_dropbox, "db_processingR", 
                      "MethDB_tables_converted.rda"))

load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))

#Quick plots of distributions
ggplot(concentrations_converted) +
  geom_histogram(aes(x = CH4mean)) +
  scale_x_log10()

ggplot(concentrations_converted) +
  geom_histogram(aes(x = CH4min)) +
  scale_x_log10()

ggplot(concentrations_converted) +
  geom_histogram(aes(x = CH4max)) +
  scale_x_log10()

ggplot(concentrations_converted) +
  geom_histogram(aes(x = CH4median)) +
  scale_x_log10()

ggplot(fluxes_converted) +
  geom_histogram(aes(x = DiffusiveCH4FluxMean)) +
  scale_x_log10()

ggplot(fluxes_converted) +
  geom_histogram(aes(x = BubbleCH4FluxMean)) +
  scale_x_log10()

ggplot(fluxes_converted) +
  geom_histogram(aes(x = TotalCH4FluxMean)) +
  scale_x_log10()

ggplot(fluxes_converted) +
  geom_histogram(aes(x = CO2FluxMean)) +
  scale_x_log10()

ggplot(fluxes_converted) +
  geom_histogram(aes(x = N2OFluxMean)) +
  scale_x_log10()


# End
# below here was code from Gerard. 
# Perhaps move these to a separate script and start with the load function above


# join the concentration with the gis datasets
# concs.gis <-  left_join(concentrations, gis, by="Site_Nid")
# 
# #quick check that the unit conversion didn't fuck things up significantly, but needs to be double checked
# ggplot(concs.gis)+
#   geom_boxplot(aes(x=CH4unit, y=CH4_umol_l))+
#   scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))
# 
# 
# # some quick plots on broad controls
# ggplot(gis)+
#   geom_bar(aes(x=land_cover_main))+
#   scale_x_discrete(labels = function(land_cover_main) str_wrap(land_cover_main, width = 5))+
#   labs(x="")+
#   theme_classic()
# 
# ggsave(file="outputs/plots/land cover.png", width = 14)
# 
# ggplot(concs.gis)+
#   geom_boxplot(aes(x=land_cover_main, y=CH4_umol_l))+
#   scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
#   scale_x_discrete(labels = function(land_cover_main) str_wrap(land_cover_main, width = 5))+
#   labs(x="")+
#   theme_classic()
# 
# ggsave(file="outputs/plots/land cover ch4.png", width = 14)
# 
# ggplot(concs.gis)+
#   geom_boxplot(aes(x=climate_zones, y=CH4_umol_l))+
#   scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
#   scale_x_discrete(labels = function(climate_zones) str_wrap(climate_zones, width = 5))+
#   labs(x="")+
#   theme_classic()
# 
# ggsave(file="outputs/plots/climate zones ch4.png", width = 14)
# 
# ###  plot on the effect of slope
# p1 <- ggplot(concs.gis)+
#   geom_point(aes(x=slope, y=CH4_umol_l))+
#   scale_y_continuous(limits=c(0,1000))+
#   theme_classic()
# 
# p2 <-  ggplot(concs.gis)+
#   geom_point(aes(x=slope, y=CH4_umol_l))+
#   scale_y_continuous(limits=c(0,100))+
#   scale_x_continuous(limits=c(0,0.1))+
#   theme_classic()
# 
# p1 + inset_element(p2, left=0.3, bottom = 0.2, top=0.9, right = 0.9) 
# 
# ggsave(file="outputs/plots/slope ch4.png")
# 
# 
# #### exploring some stuff
# ggplot(concs.gis)+
#   geom_point(aes(x=soil_erosion_up_kghectareyear, y=CH4_umol_l))+
#   scale_y_continuous(limits=c(0,100))+
#   theme_classic()
# 
# 
# concs.gis %>% 
# ggplot()+
#   geom_point(aes(x=population_density_up_peoplekm2, y= CH4_umol_l))+
#   scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
#   theme_classic()
# 
# ggsave(file="outputs/plots/pop density ch4.png")
# 
# 
# concs.gis %>% 
#   ggplot()+
#   geom_point(aes(x=gw_table_cm, y= CH4_umol_l))+
#   scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
#   theme_classic()
# 
# ggsave(file="outputs/plots/gw table ch4.png")
# 
# 
# 
# 
# colnames(concs.gis)
# 
# #ggplot(concs.gis)+
# #  geom_point(aes(`Catchment size km2`, catch_area_km))
# 
# 
# #### Example of within site variability= temperature
# concs.gis  %>% 
#   add_count(Site_Nid) %>% 
#   filter(n> 30) %>% 
#   drop_na(CH4_umol_l, `Water Temp actual`) %>% 
#   ggplot()+
#   geom_point(aes(`Water Temp actual`,CH4_umol_l))+
#   facet_wrap(~Site_Nid, scales = "free")+
#   theme_classic()
# 
# ggsave(file="outputs/plots/temp ch4.png", width = 15, height = 15)
#   
# 
# ##### SOme stats? another day...
# 
# corr.ch4.everything <- concs.gis  %>% 
#   select_if(is.numeric) %>% 
#   select(CH4_umol_l, q_avg_m3s:human_footprint_up_09) %>% 
#   filter(CH4_umol_l > 0.01) %>% 
#   mutate(CH4_umol_l = log(CH4_umol_l)) %>% 
#   correlate() %>% focus(CH4_umol_l)