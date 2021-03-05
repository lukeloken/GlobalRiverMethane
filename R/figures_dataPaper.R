

library(tidyverse)
library(maps)
library(ggthemes)
library(gridExtra)
library(ggExtra)
library(patchwork)

#Load custom ggplot functions
source("R/ggplot2_utils.R")

path_to_dropbox <-  "C:/Users/gero0008/Dropbox/SCIENCE/PostDoc/MethDB2.0"

# load formatted and converted tables into your R environment
load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))

world_gis <- read_rds(file.path(path_to_dropbox, "db_processingR", "methdb_explore", "data", 
               "basin_atlas_L12.rds"))

flux_df <- flux_df %>% 
  mutate(Site_Nid =as.character(Site_Nid))

conc_df <- conc_df %>% 
  rename(SampleDatestart = `SampleDate(start)`)

#summarise how mauny observations of concs and flux per site
sites_conc_flux <-sites_df %>% 
                select(Site_Nid, Latitude, Longitude) %>% 
  left_join(conc_df %>% 
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
  ) 

# quick look
sites_conc_flux %>% 
  group_by(type) %>% 
  tally()


  
library(plotbiomes)
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
  theme(legend.position = c(0.2, 0.75))



#global map
map_sc <- map_data('world')

map_world <- 
  ggplot()+
  geom_map(data=map_sc, map=map_sc,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="gray90", colour="gray40", size=0.5)+
  geom_point(data=sites_conc_flux, 
             aes(x=Longitude, y=Latitude, color = type), 
             size=2, alpha=.5)+
  scale_y_continuous(limits=c(-52,80))+
  scale_color_manual(values = c("#3b4994", "#5ac8c8", "#b364ac","gold"))+
  coord_sf(crs = 4087)+
  labs(color= "")+
  theme_map()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        legend.text = element_text(size=13),
        legend.justification = "center" )


#add the density plots on the map
world_densities <- ggMarginal(map_world, type="density", size=15, fill="#add6f0", color= NA)

#histograms of observations
# hists_n <- 
# sites_conc_flux %>% 
#   pivot_longer(n_conc:n_flux, names_to = "type_value",
#                                  values_to = "n") %>% 
#   ggplot()+
#   geom_histogram(aes(x=n, fill= type),  color= NA)+
#   scale_fill_manual(values = c("#3b4994", "#5ac8c8", "#b364ac"), name="")+
#   scale_x_log10(breaks=c(1,5, 10, 50, 100))+
#   theme_classic()+
#   theme(legend.position=c(0.8, 0.8))

hists_n <- 
  sites_conc_flux %>% 
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
  theme(legend.position=c(0.8, 0.8))


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
  group_by(date = SampleDatestart, 
           Continent = Continent ) %>% 
  summarise(obs = n()) %>% 
  ungroup(date, Continent)

data_plot_obs <- tibble(
  date = rep(seq.Date( from = min(data_sum_year_continent$date), 
                  to = max(data_sum_year_continent$date),
                  by = "day"),
            each = length(unique(data_sum_year_continent$Continent)) 
  )) %>% 
  mutate(Continent = rep(unique(data_sum_year_continent$Continent), length.out = length(date)) ) %>% 
  left_join(data_sum_year_continent, by= c("date", "Continent")) %>% 
  mutate(obs = replace_na(obs, 0)) %>% 
  group_by(Continent) %>% 
  arrange(date) %>% 
  mutate(cum.obs = cumsum(obs)) %>% 
  ungroup(date, Continent) 

# and finally the plot for the time series
plot_ts_concs <- ggplot(data_plot_obs)+
  geom_area(aes(x=date, y=cum.obs, fill=Continent), position = "stack")+
  geom_vline(xintercept = as.Date("2015-01-01"), linetype= 2)+
  theme_classic()+
  labs(y="Cumulative observations")+
  theme(legend.position = c(0.2, 0.6))


#put them together in a panel
together <- grid.arrange(world_densities, arrangeGrob(hists_n, plot_ts_concs, mat_map, ncol= 3), 
                         nrow = 2, heights = c(2,1))

ggsave(together,  filename =  file.path(path_to_dropbox, "data ms files", "figures", "map_sites.png"),
       width = 14, height = 9)




##### just to look at the world basin atlas, to have an idea of how big the catchments are... for reference, one "reach" is the whole krycklan
library(leaflet)

world_gis %>% 
  filter(countries_sub == "Sweden") %>% 
  st_transform( 4326) %>% 
  leaflet() %>% 
    addProviderTiles("Esri.WorldImagery") %>% 
    addPolygons() %>% 
    addMeasure() 


a <- sites_df %>% select(Site_Nid, Country) %>% 
  left_join(gis_df %>%  select(Site_Nid, countries_sub), 
            by="Site_Nid") %>% 
  filter(map2_lgl(countries_sub, Country,  str_detect))

a2 <- sites_df %>% select(Site_Nid, Country) %>% 
  anti_join( a , by ="Site_Nid") %>% 
  left_join(gis_df %>%  select(Site_Nid, countries_sub), 
            by="Site_Nid") 

  write_csv(a2,  file.path(path_to_dropbox, "sites_with_different_country.csv"))
  
  
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
          