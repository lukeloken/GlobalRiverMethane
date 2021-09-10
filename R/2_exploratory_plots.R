library(tidyverse)
library(patchwork)
library(here)
library(lubridate)

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


# Exploration in space with the gis data ----
# join the concentration with the gis datasets
concs_gis <-  left_join(conc_df, gis_df %>% mutate(Site_Nid=as.character(Site_Nid)), by="Site_Nid")

# join the concentration with the site dataset, sort by latitude for later plots
conc_sites <- left_join(conc_df, sites_df, by="Site_Nid") %>% 
  mutate(site_lat = paste("Site:",Site_Nid,"\nLat: " , round(Latitude,1), sep=""))

conc_sites$site_lat <- reorder(conc_sites$site_lat, desc(conc_sites$Latitude))


# some quick plots on broad controls
ggplot(gis_df)+
  geom_bar(aes(x=land_cover_main))+
  scale_x_discrete(labels = function(land_cover_main) str_wrap(land_cover_main, width = 5))+
  labs(x="")+
  theme_classic()



ggplot(concs_gis)+
  geom_boxplot(aes(x=land_cover_main, y=CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  scale_x_discrete(labels = function(land_cover_main) str_wrap(land_cover_main, width = 5))+
  labs(x="")+
  theme_classic()


ggplot(concs_gis)+
  geom_boxplot(aes(x=climate_zones, y=CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  scale_x_discrete(labels = function(climate_zones) str_wrap(climate_zones, width = 5))+
  labs(x="")+
  theme_classic()


###  plot on the effect of slope
p1 <- ggplot(concs_gis)+
  geom_point(aes(x=slope_m_m, y=CH4mean))+
  scale_y_continuous(limits=c(0,1000))+
  theme_classic()

p2 <-  ggplot(concs_gis)+
  geom_point(aes(x=slope_m_m, y=CH4mean))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_continuous(limits=c(0,0.1))+
  theme_classic()

p1 + inset_element(p2, left=0.3, bottom = 0.2, top=0.9, right = 0.9)

ggsave(file="man/figures/slope ch4.png")


concs_gis %>%
ggplot()+
  geom_point(aes(x=population_density_up_peoplekm2, y= CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  theme_classic()


concs_gis %>%
  group_by(Site_Nid) %>% 
  summarise( across(c(CH4mean, gw_table_cm), mean)) %>% 
  ggplot()+
  geom_point(aes(x=gw_table_cm, y= CH4mean))+
  scale_y_log10(breaks=c(.01,.1,1, 10, 100, 1000, 10000),labels=c(.01,.1,1, 10, 100, 1000, 10000))+
  theme_classic()



# Seasonal and temporal plots
#### Example of within site variability = co2 vs ch4
conc_df %>% 
  filter(CH4mean > 0, CO2mean > 1) %>%
  ggplot(aes(CO2mean, CH4mean))+
  geom_point(alpha=.1, size=2)+
  geom_point(data= conc_sites %>%
                drop_na(Q,CH4mean, WaterTemp_actual) %>%
                add_count(Site_Nid) %>%
                filter(n> 30, CH4mean > 0),
              aes(CO2mean, CH4mean, color= Site_Nid), alpha=.2, size=2)+
  geom_smooth(data= conc_sites  %>%
               drop_na(Q,CH4mean, WaterTemp_actual) %>%
               add_count(Site_Nid) %>%
               filter(n> 30, CH4mean > 0),
             aes(CO2mean, CH4mean, color= Site_Nid), se=FALSE)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_discrete(guide = "none")+
  theme_bw()

#### Example of within site variability = q vs ch4
conc_df %>% 
  filter(Q > 0, CH4mean > 0) %>%
  ggplot(aes(Q, CH4mean))+
  geom_point(alpha=.05, size=2)+
  geom_point(data= conc_sites  %>%
                drop_na(CH4mean, Q) %>%
                add_count(Site_Nid) %>%
                filter(n> 30, CH4mean > 0),
              aes(Q, CH4mean, color= Site_Nid), alpha=.2, size=2)+
  geom_smooth(data= conc_sites  %>%
                drop_na(CH4mean, Q) %>%
                add_count(Site_Nid) %>%
                filter(n> 30, CH4mean > 0),
              aes(Q, CH4mean, color= Site_Nid), se=FALSE)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_discrete(guide = "none")+
  theme_bw()


conc_df %>% 
  filter(WaterTemp_actual > 0, CH4mean > 0) %>%
  ggplot(aes(WaterTemp_actual, CH4mean))+
 # geom_point(alpha=.05, size=2)+
  geom_point(data= conc_sites  %>%
               drop_na(CH4mean, WaterTemp_actual) %>%
               add_count(Site_Nid) %>%
               filter(n> 30, CH4mean > 0, WaterTemp_actual > 0),
             aes(WaterTemp_actual, CH4mean, color= Site_Nid), alpha=.2, size=2)+
  geom_smooth(data= conc_sites  %>%
                drop_na(CH4mean, WaterTemp_actual) %>%
                add_count(Site_Nid) %>%
                filter(n> 30, CH4mean > 0, WaterTemp_actual > 0),
              aes(WaterTemp_actual, CH4mean, color= Site_Nid), se=FALSE)+
  scale_y_log10()+
  scale_color_discrete(guide = "none")+
  theme_bw()



temp_plot <- conc_sites  %>%
  drop_na(Q,CH4mean, WaterTemp_actual) %>%
  add_count(Site_Nid) %>%
  filter(n> 20, CH4mean > 0) %>%
  ggplot()+
  geom_point(aes(WaterTemp_actual, CH4mean, color=month(Date_start)))+
  scale_color_viridis_c()+
  facet_wrap(~site_lat, scales = "free")+
  theme_classic()



temp_plot %>% 
  ggsave( file="man/figures/temp ch4.png", width = 15, height = 15)

#### Example of within site variability= discharge
disch_plot <- conc_sites  %>%
  drop_na(CH4mean, Q) %>%
  add_count(Site_Nid) %>%
  filter(n> 30, CH4mean > 0) %>%
  ggplot()+
  geom_point(aes(Q, CH4mean, color=month(Date_start)))+
  scale_color_viridis_c()+
  facet_wrap(~site_lat, scales = "free")+
  theme_classic()

disch_plot %>% 
  ggsave(file="man/figures/q ch4.png", width = 15, height = 15)




## relationships between gases ----
co2_ch4 <- conc_df %>% 
  filter(CH4mean > 0, CO2mean > 1) %>%
  drop_na( WaterTemp_actual) %>%
  ggplot(aes(CO2mean, CH4mean, color=WaterTemp_actual))+
  geom_point(alpha=.6, size=2)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_viridis_c()

co2_n2o <- conc_df %>% 
  filter(N2Omean > 0, CO2mean > 0) %>%
  drop_na( WaterTemp_actual) %>%
  ggplot(aes(CO2mean, N2Omean, color=WaterTemp_actual))+
  geom_point(alpha=.6, size=2)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_viridis_c(guide = "none")

ch4_n2o <- conc_df %>% 
  filter(CH4mean > 0, N2Omean > 0) %>%
  drop_na( WaterTemp_actual) %>%
  ggplot(aes(CH4mean, N2Omean, color=WaterTemp_actual))+
  geom_point(alpha=.6, size=2)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_viridis_c(guide = "none")

o2_ch4 <- conc_df %>% 
  filter(CH4mean > 0, DO_percentsat > 0) %>%
  drop_na( WaterTemp_actual) %>%
  ggplot(aes(DO_percentsat, CH4mean, color=WaterTemp_actual))+
  geom_point(alpha=.6, size=2)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_viridis_c()

o2_n2o <- conc_df %>% 
  filter(N2Omean > 0, DO_percentsat > 0) %>%
  drop_na( WaterTemp_actual) %>%
  ggplot(aes(DO_percentsat, N2Omean, color=WaterTemp_actual))+
  geom_point(alpha=.6, size=2)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_viridis_c(guide = "none")

o2_co2 <- conc_df %>% 
  filter(DO_percentsat > 0, CO2mean > 0) %>%
  drop_na( WaterTemp_actual) %>%
  ggplot(aes(DO_percentsat, CO2mean, color=WaterTemp_actual))+
  geom_point(alpha=.6, size=2)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_viridis_c(guide = "none")



gases_all_o2 <- (co2_ch4 + co2_n2o + ch4_n2o + o2_ch4 + o2_n2o + o2_co2) +  
  plot_layout(guides = 'collect') & theme_minimal()

gases_all_o2 %>% 
  ggsave(file="man/figures/ghg_all_o2.png", width = 12, height = 7)


conc_df %>% 
  filter(CH4mean > 0.01, CO2mean > 1) %>%
  drop_na( WaterTemp_actual) %>%
  ggplot(aes(Q, CH4mean/CO2mean, color=WaterTemp_actual))+
  geom_point(alpha=.6, size=2)+
  scale_y_log10()+
  scale_x_log10()+
  scale_color_viridis_c()

### Some correlations----
library(corrr)
corr.ch4.everything <- concs_gis  %>%
  select_if(is.numeric) %>%
  select(CH4mean, q_avg_m3s:human_footprint_up_09) %>%
  filter(CH4mean > 0.01) %>%
  mutate(CH4mean = log(CH4mean)) %>%
  correlate() %>% focus(CH4mean)

corr.ch4.everything %>% arrange(CH4mean)
corr.ch4.everything %>% arrange(desc(CH4mean))

### Summary of time series ----
long_ts <-  conc_df  %>%
  drop_na(CH4mean) %>%
  group_by(Site_Nid) %>% 
  filter(n() >= 20) %>%
  summarise(year_start = min(year(Date_start)),
            year_end = max(year(Date_start)),
            n_CH4=n(),
            n_CO2 = sum(CO2mean > 0),
            n_N2O = sum(N2Omean > 0),
            n_Q = sum(Q > 0),
            n_temp = sum(WaterTemp_actual > -10, na.rm = TRUE) + sum(WaterTemp_est > -10, na.rm = TRUE),
            n_jan = sum(month(Date_start) == 1),
            n_feb = sum(month(Date_start) == 2),
            n_mar = sum(month(Date_start) == 3),
            n_apr = sum(month(Date_start) == 4),
            n_may = sum(month(Date_start) == 5),
            n_jun = sum(month(Date_start) == 6),
            n_jul = sum(month(Date_start) == 7),
            n_aug = sum(month(Date_start) == 8),
            n_sep = sum(month(Date_start) == 9),
            n_oct = sum(month(Date_start) == 10),
            n_nov = sum(month(Date_start) == 11),
            n_dec = sum(month(Date_start) == 12)
            )

write_csv(long_ts, "man/summary_timeseries_GRIME.csv")



# global map per month ----
library(sf)
library(ggthemes)

map_sc <- map_data('world')

sites_months <- conc_sites %>% group_by(Site_Nid) %>% 
  summarise(Latitude =first(Latitude),
            Longitude = first(Longitude),
            n_CH4=n(),
            n_CO2 = sum(CO2mean > 0),
            n_N2O = sum(N2Omean > 0),
            n_Q = sum(Q > 0),
            n_temp = sum(WaterTemp_actual > -10, na.rm = TRUE) + sum(WaterTemp_est > -10, na.rm = TRUE),
            jan = sum(month(Date_start) == 1),
            feb = sum(month(Date_start) == 2),
            mar = sum(month(Date_start) == 3),
            apr = sum(month(Date_start) == 4),
            may = sum(month(Date_start) == 5),
            jun = sum(month(Date_start) == 6),
            jul = sum(month(Date_start) == 7),
            aug = sum(month(Date_start) == 8),
            sep = sum(month(Date_start) == 9),
            oct = sum(month(Date_start) == 10),
            nov = sum(month(Date_start) == 11),
            dec = sum(month(Date_start) == 12)
  ) %>% 
  pivot_longer(jan:dec, names_to = "month", values_to = "n_month") %>% 
  mutate(n_month = if_else(n_month == 0, NA_integer_, n_month)) %>% 
  drop_na(n_month)


sites_months$month <- factor(sites_months$month, levels= c("jan","feb","mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

dat_text <- sites_months %>% 
  group_by(month) %>% 
  summarise(n_sites = n_distinct(Latitude), 
    label = paste("n_obs = ", n(), ", n_sites =", n_sites))
  

sites_months_map <- ggplot()+
  geom_map(data=map_sc, map=map_sc,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="gray90", colour="gray40", size=0.5)+
  geom_point(data=sites_months, 
             aes(x=Longitude, y=Latitude, color= n_month, alpha = 0.5), 
             size=2, alpha=.5)+
  scale_y_continuous(limits=c(-52,80))+
  geom_text( data = dat_text,
    mapping = aes(x = -Inf, y = -Inf, label = label), hjust   = -.1, vjust   = -.8)+
  scale_colour_viridis_b(option = "D", trans= "log", breaks = c(1,2, 5, 10, 50, 100), na.value = NA)+
  coord_sf(crs = 4087)+
  labs(color= "")+
  theme_map()+
  facet_wrap(~month)+
  theme(title = element_text(size=16))

ggsave("man/figures/map_sites_months.png", plot = sites_months_map , width = 15)


