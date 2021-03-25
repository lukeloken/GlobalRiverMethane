library(tidyverse)



load(file.path(path_to_dropbox, "db_processingR", 
                "MethDB_tables_converted.rda"))

#Quick plots of distributions
ggplot(conc_df) +
  geom_histogram(aes(x = CH4mean)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = CO2mean)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = N2Omean)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = CH4min)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = CH4max)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = CH4median)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = DiffusiveCH4FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = BubbleCH4FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = TotalCH4FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = CO2FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = N2OFluxMean)) +
  scale_x_log10()

source("R/convert_unit_functions.R")

#calculate %sat for all values
conc_df <- conc_df %>% 
  mutate(temp.any = ifelse(is.na(WaterTempactual) == T, WaterTempest, WaterTempactual),
           pressure = estimate_pressure(Elevation_m),
           kh = getKh(temp.any+ 273.15, "CH4"),
           atm.eq = 1.8*kh/pressure,
         ch4.sat = CH4mean/atm.eq*100
         )

ggplot(conc_df)+
  geom_histogram(aes(x=ch4.sat))+
  scale_x_log10()+
  geom_vline(xintercept =100)

conc_df %>% filter(ch4.sat < 100) %>% 
  select(Site_Nid, CH4mean, ch4.sat) %>% 
arrange(ch4.sat) %>% 
  print(n=100)

# join the concentration with the gis datasets
concs_gis <-  left_join(conc_df, gis_df, by="Site_Nid")

#quick check that the unit conversion didn't fuck things up significantly, but needs to be double checked
ggplot(concs_gis)+
  geom_boxplot(aes(x=orig_N2Ounit, y=N2Omean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))

conc_df %>% 
  filter(orig_N2Ounit == "mgN/L") %>% 
  select(Publication_Nid, Site_Nid, SiteName, N2Omean, CH4mean) %>% 
  print(n=50)

# some quick plots on broad controls
ggplot(gis_df)+
  geom_bar(aes(x=land_cover_main))+
  scale_x_discrete(labels = function(land_cover_main) str_wrap(land_cover_main, width = 5))+
  labs(x="")+
  theme_classic()

#ggsave(file="outputs/plots/land cover.png", width = 14)

ggplot(concs_gis)+
  geom_boxplot(aes(x=land_cover_main, y=CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  scale_x_discrete(labels = function(land_cover_main) str_wrap(land_cover_main, width = 5))+
  labs(x="")+
  theme_classic()

#ggsave(file="outputs/plots/land cover ch4.png", width = 14)

ggplot(concs_gis)+
  geom_boxplot(aes(x=climate_zones, y=CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  scale_x_discrete(labels = function(climate_zones) str_wrap(climate_zones, width = 5))+
  labs(x="")+
  theme_classic()

#ggsave(file="outputs/plots/climate zones ch4.png", width = 14)

###  plot on the effect of slope
p1 <- ggplot(concs_gis)+
  geom_point(aes(x=slope, y=CH4mean))+
  scale_y_continuous(limits=c(0,1000))+
  theme_classic()

p2 <-  ggplot(concs_gis)+
  geom_point(aes(x=slope, y=CH4mean))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_continuous(limits=c(0,0.1))+
  theme_classic()

p1 + inset_element(p2, left=0.3, bottom = 0.2, top=0.9, right = 0.9)

#ggsave(file="outputs/plots/slope ch4.png")


#### exploring some stuff
ggplot(concs_gis)+
  geom_point(aes(x=soil_erosion_up_kghectareyear, y=CH4mean))+
  scale_y_continuous(limits=c(0,100))+
  theme_classic()


concs_gis %>%
ggplot()+
  geom_point(aes(x=population_density_up_peoplekm2, y= CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  theme_classic()

#ggsave(file="outputs/plots/pop density ch4.png")


concs_gis %>%
  ggplot()+
  geom_point(aes(x=gw_table_cm, y= CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  theme_classic()

#ggsave(file="outputs/plots/gw table ch4.png"))


#### Example of within site variability= temperature
conc_df  %>%
  add_count(Site_Nid) %>%
  filter(n> 30) %>%
  drop_na(CH4mean, WaterTempactual) %>%
  ggplot()+
  geom_point(aes(WaterTempactual, CH4mean))+
  facet_wrap(~Site_Nid, scales = "free")+
  theme_classic()


#### Example of within site variability= discharge
conc_df  %>%
  add_count(Site_Nid) %>%
  filter(n> 30) %>%
  drop_na(CH4mean, Q) %>%
  ggplot()+
  geom_point(aes(Q, CH4mean))+
  facet_wrap(~Site_Nid, scales = "free")+
  theme_classic()

#ggsave(file="outputs/plots/temp ch4.png", width = 15, height = 15)


conc_df %>% 
  ggplot()+
  geom_point(aes(CH4mean, N2Omean), alpha=.5)+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()

conc_df %>% 
  ggplot()+
  geom_point(aes(CH4mean, CO2mean), alpha=.5)+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()




##### SOme stats? another day...
library(corrr)
corr.ch4.everything <- concs_gis  %>%
  select_if(is.numeric) %>%
  select(CH4mean, q_avg_m3s:human_footprint_up_09) %>%
  filter(CH4mean > 0.01) %>%
  mutate(CH4mean = log(CH4mean)) %>%
  correlate() %>% focus(CH4mean)


