

library(tidyverse)
library(maps)
library(ggthemes)
library(patchwork)
library(ggExtra)
library(plotbiomes)
library(rnaturalearth)
library(sf)
library(lwgeom)

#Load custom ggplot functions
source("R/ggplot2_utils.R")

#for sf processing
sf::sf_use_s2(FALSE)

# load formatted and converted tables into your R environment
load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))

world_gis <- read_rds(file.path(path_to_dropbox, "db_processingR", "methdb_explore", "data", 
              "basin_atlas_L12.rds"))  %>% 
  st_transform(4326)

flux_df <- flux_df %>% 
  mutate(Site_Nid =as.character(Site_Nid))



#summarise how mauny observations of concs and flux per site
sites_conc_flux <-sites_df %>% 
                select(Site_Nid, Latitude, Longitude) %>% 
  left_join(conc_df %>% 
              drop_na(orig_CH4unit) %>% 
              group_by(Site_Nid) %>% 
              summarise(n_conc= n()), by= "Site_Nid" ) %>% 
  left_join(flux_df %>% 
              group_by(Site_Nid) %>% 
              summarise(n_flux= n()), by= "Site_Nid"  ) %>% 
  mutate(
    type = case_when(
    is.na(n_conc) == FALSE & is.na(n_flux) == FALSE ~ "both",
    is.na(n_conc) == TRUE & is.na(n_flux) == FALSE ~ "only flux",
    is.na(n_conc) == FALSE & is.na(n_flux) == TRUE ~ "only concentrations",
    is.na(n_conc) == TRUE & is.na(n_flux) == TRUE ~ "none"),
    type = fct_relevel(type, "both", "only flux", "only concentrations", "none" )
  ) %>%
  st_as_sf( coords = c("Longitude", "Latitude"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

# quick look
sites_conc_flux %>% 
  group_by(type) %>% 
  tally()

sites_conc_flux %>% 
  filter(type == "none") %>% 
 # select(Site_Nid) %>% 
  print(n=50)


#figure of MAT vs MAP with global distribution
  mat_map <- 
  ggplot()+
  #geom_hex(data= world_gis, bins=50,  aes(air_temp_sub_avg_celsius/10, precip_sub_mm))+
  geom_polygon(data = Whittaker_biomes, 
               aes(x = temp_c, y = precp_cm*10, fill = biome), 
               colour = "gray98", size   = 1) +
    scale_fill_manual(name   = "Whittaker biomes",
                      breaks = names(Ricklefs_colors),
                      labels = names(Ricklefs_colors),
                      values = Ricklefs_colors)+
    geom_point(data=gis_df, aes(air_temp_sub_avg_celsius/10, precip_sub_mm), alpha=.3, color="black")+
#  scale_fill_viridis_c()+
  theme_classic()+
  labs(x="Mean average temperature (C)", y= "Mean average precipitation (mm)")+
  theme(legend.position = c(0.3, 0.85), legend.text=element_text(size=10), 
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())+
  guides(fill = guide_legend(title ="", ncol=1, keywidth=0.2,
                             keyheight=0.1,
                             default.unit="inch"))



#global map
map_sc <- ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

rivers50 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
                        category = 'physical', returnclass = "sf" )

lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = "sf")

map_world <- 
  ggplot()+
  geom_sf(data=map_sc, fill="gray50", colour=NA, size=0.5)+
  geom_sf(data = rivers50 %>% filter(scalerank < 8), color= "steelblue4")+
  geom_sf(data=lakes %>% filter(scalerank < 1), fill="aliceblue", color=NA)+
  geom_sf(data=sites_conc_flux %>% filter(type != "none"), 
             aes( color = type), 
             size=2, alpha=.5)+
  scale_color_manual(values = c("olivedrab3",  "steelblue3", "darkgoldenrod2"), name="")+
  coord_sf(crs = 4087, xlim=c(-18026400, 21026400), ylim=c(-7062156, 10602156))+
  labs(color= "")+
  theme_map()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        #panel.background=element_rect(fill="gray50"),
        legend.position=c(.12,.15),
        legend.text = element_text(size=13),
        legend.justification = "center" )+
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1) ) )



#function to run at custom accuracies, from plyr package
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy} 

sites_lat <- sites_conc_flux %>% 
  st_transform(4326) %>% 
  filter(type != "none") %>% 
  mutate(lat = st_coordinates(.)[, "Y"] %>% round(0),
         type ="observations") %>% 
  group_by(lat) %>% 
  summarise(n=n(),
            type=first(type)) %>% 
  dplyr::select(type, lat, n) %>% 
  st_drop_geometry()

sites_lon <- sites_conc_flux %>% 
  st_transform(4326) %>% 
  filter(type != "none") %>% 
  mutate(lon = st_coordinates(.)[, "X"] %>% round(0),
         type ="observations") %>% 
  group_by(lon = round_any(lon,1)) %>% 
  summarise(n=n(),
            type=first(type)) %>% 
  dplyr::select(type, lon, n) %>% 
  st_drop_geometry()


world_gis_good <- st_is_valid(world_gis, reason = FALSE)

lat_rivers <- 
  world_gis[world_gis_good,] %>% 
  mutate(start_point = st_startpoint(st_cast(., "MULTIPOINT")),
         lat = st_coordinates(start_point)[,2] %>%  round(0) ) %>%
  group_by(lat) %>% 
  summarise(riv_area = sum(river_area_sub_ha)) %>% 
  dplyr::select( lat,  riv_area) %>% 
  st_drop_geometry()

lon_rivers <- world_gis[world_gis_good,] %>% 
  mutate(start_point = st_startpoint(st_cast(., "MULTIPOINT")),
         lon = st_coordinates(start_point)[,1] %>%  round(0) ) %>% 
  st_drop_geometry() %>%
  group_by(lon= round_any(lon,1)) %>% 
  summarise(riv_area = sum(river_area_sub_ha)) %>% 
  dplyr::select( lon,  riv_area)



together_lat <- lat_rivers %>% 
  mutate(type="river area",
         n=riv_area/max(riv_area)) %>% 
  dplyr::select(type, lat, n) %>% 
  bind_rows(sites_lat %>% 
              mutate(n=n/max(n)) %>% 
              dplyr::select(type, lat, n)) 



together_lon <- lon_rivers %>% 
  mutate(type="river area",
         n=riv_area/max(riv_area)) %>% 
  dplyr::select(type, lon, n) %>% 
  bind_rows(sites_lon %>% 
              mutate(n=n/max(n)) %>% 
              dplyr::select(type,lon,  n)) 

n_lats <- ggplot(together_lat, aes( x= lat, y=n, fill=type))+
  geom_col(alpha=.5, position = "identity", width = 1)+
  scale_fill_manual(  values = c("gray15",  "cadetblue3"))+
  theme_classic()+
  coord_flip()+
  scale_x_continuous(expand=c(0,0), breaks = c(-50, 0, 50), name="", limits = c(-50, 85))+
  scale_y_continuous( expand=c(0,0), name="")+
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line  = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", color="transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color="transparent"))
  
n_lons <- ggplot(together_lon, aes( x= lon, y=n, fill=type))+
  geom_col(alpha=.5, position = "identity", width = 1)+
  scale_fill_manual(  values = c("gray15",  "cadetblue3"))+
  theme_classic()+
  scale_x_continuous(expand=c(0,0), breaks = c(-120, -60, 0, 60, 120), name="", limits=c(-170, 180))+
  scale_y_continuous( expand=c(0,0), name="")+
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line  = element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "transparent", color="transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color="transparent"))



map_world + 
  inset_element(n_lats, .93, .045, 1.01, .915 ) +
  inset_element(n_lons, 0.012, .84, .94, 1.02)

#ggarrange(map_world, n_lats, widths = c(8, 1), ncol = 2, align = "h")

ggsave("man/figures/map_hist.png", scale=1, dpi = 400)

hists_n <- 
  sites_conc_flux %>% 
  st_drop_geometry() %>% 
  pivot_longer(n_conc:n_flux, names_to = "type_value",
               values_to = "n") %>%
  mutate(category = case_when(
    n <= 1 ~ "n = 1",
    n %in% 2:10 ~ "2 - 10",
    n %in% 11:100 ~ "11 - 100",
    n > 100 ~ "> 100")) %>%
  drop_na(category) %>% 
  group_by(category, type) %>% 
  summarise(n= n()) %>% 
  ungroup() %>% 
  mutate( category = fct_relevel(category, "n = 1","2 - 10","11 - 100", "> 100" ),
          type =  fct_relevel(type, "both", "only concentrations", "only flux")) %>% 
  ggplot()+
  geom_col(aes(x = category, y=n, fill= type),  color= NA, position = "dodge")+
  scale_fill_manual(values = c("#3b4994", "#b364ac", "#5ac8c8"), name="")+
  theme_classic()+
  labs( x= "number of observations", y= "number of sites")+
  theme(legend.position=c(0.34, 1.4),
        legend.text = element_text(size=14))


# Plot for the time series of observations. 
# Fuck me, what was supposed to be the easiest needed a lot of work.
# geom_area wants a value for each date, continuous date, so I need to make a file with dates for each continent
data_sum_year_continent <- conc_df %>% 
  left_join(sites_df, by = "Site_Nid") %>%
  mutate(Continent = case_when(
    Continent == "Greenland" ~ "North America",
    Continent == "Central America" ~ "North America",
    !Continent %in% c("Greenland", "Central America") ~ Continent
  )) %>% 
  select(Date_start, Continent) %>% 
  bind_rows(
    flux_df %>% 
      left_join(sites_df, by = "Site_Nid") %>%
      mutate(Continent = case_when(
        Continent == "Greenland" ~ "North America",
        Continent == "Central America" ~ "North America",
        !Continent %in% c("Greenland", "Central America") ~ Continent
      )) %>%
      select(Date_start, Continent) 
  ) %>% 
  group_by(date = as.Date(Date_start), 
           Continent = Continent ) %>% 
  summarise(obs = n()) %>% 
  ungroup(date, Continent) 

data_plot_obs <- tibble(
  date = rep(seq.Date( from = min(data_sum_year_continent$date, na.rm = TRUE), 
                  to = max(data_sum_year_continent$date, na.rm = TRUE),
                  by = "day"),
            each = length(unique(data_sum_year_continent$Continent)) 
  )) %>% 
  mutate(Continent = rep(unique(data_sum_year_continent$Continent), length.out = length(date)) ) %>% 
  left_join(data_sum_year_continent, by= c("date", "Continent")) %>% 
  mutate(obs = replace_na(obs, 0)) %>% 
  group_by(Continent) %>% 
  arrange(date) %>% 
  mutate(cum.obs = cumsum(obs)) %>% 
  ungroup(date, Continent) %>% 
  drop_na(Continent) %>% 
  mutate( Continent =  fct_relevel(Continent, "Oceania", "Africa", "Asia", "Europe", "North America", "South America") )

# and finally the plot for the time series
plot_ts_concs <- ggplot(data_plot_obs)+
  geom_area(aes(x=date, y=cum.obs, fill=Continent), position = "stack")+
#  geom_vline(xintercept = as.Date("2015-01-01"), linetype= 2)+
  scale_fill_manual(values= c("#b88c3f", #colors from http://medialab.github.io/iwanthue/
                              "#ba5fb2",
                              "#75ab3d",
                              "#6a7fce",
                              "#cc5658",
                              "#54a77b"))+
  scale_x_date(breaks = seq.Date(as.Date("1970-01-01"), as.Date("2020-01-01"), by = "10 year"), 
               date_labels = "%Y" )+
  theme_classic()+
  labs(y="Cumulative observations")+
  theme(legend.position = c(0.2, 0.8))+
  guides(fill = guide_legend( ncol=1, keywidth=0.2,
                             keyheight=0.1,
                             default.unit="inch"))


#put them together in a panel
together <- grid.arrange(world_densities, arrangeGrob(hists_n, plot_ts_concs, mat_map, ncol= 3), 
                         nrow = 2, heights = c(2,1))

ggsave(together,  filename =  file.path(path_to_dropbox, "Figures", "map_sites.png"),
       width = 14, height = 9, dpi = 400)




### some randoms stuff to check the countries

a <- sites_df %>% select(Site_Nid, Country) %>% 
  left_join(gis_df %>%  select(Site_Nid, countries_sub), 
            by="Site_Nid") %>% 
  filter(map2_lgl(countries_sub, Country,  str_detect))

a2 <- sites_df %>% select(Site_Nid, Country) %>% 
  anti_join( a , by ="Site_Nid") %>% 
  left_join(gis_df %>%  select(Site_Nid, countries_sub), 
            by="Site_Nid") 

  write_csv(a2,  file.path(path_to_dropbox, "sites_with_different_country.csv"))
  
  # and random stuff to check the coauthors
coauthors <- sites_df %>% 
  group_by(Publication_Nid) %>% 
  summarise(n_sites=n()) %>% 
  left_join(conc_df %>% 
  group_by(Publication_Nid) %>% 
  summarise(n_concs=n()), by = "Publication_Nid") %>% 
  left_join(flux_df %>% 
              group_by(Publication_Nid) %>% 
              summarise(n_flux = n()), by = "Publication_Nid") %>% 
  left_join(papers_df, by= "Publication_Nid") %>% 
  group_by(Authorlastname) %>% 
  summarise(n_pubs= n(),
            n_sites =sum(n_sites),
            n_concs = sum(n_concs),
            n_flux = sum(n_flux))

write_csv(coauthors, file.path(path_to_dropbox, "co_authors_summary.csv"))



          