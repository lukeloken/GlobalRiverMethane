
# Load packages -----

library(tidyverse)
library(broom)
library(ggthemes)
library(patchwork)
library(rnaturalearth)
library(sf)
library(lwgeom)
library(readxl)


# read /download files ----
path_to_dropbox <-  "/Users/gdro0001/Dropbox/SCIENCE/PostDoc/MethDB2.0" #gerards pc
# load formatted and converted tables of GRiMeDB into your R environment
load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))


# Download the hydroatlas, from here: https://figshare.com/articles/dataset/HydroATLAS_version_1_0/9890531
# We can do it automatically 
zipped_atlas <- "original_raw_data/BASINatlas/BasinAtlasshp.zip"
unzipped_atlas <- "original_raw_data/BASINatlas/BasinATLAS_v10"

if(file.exists(zipped_atlas) == TRUE){
  print("files already downloaded") 
} else {
download.file("https://figshare.com/ndownloader/files/20087237", zipped_atlas, mode="wb")

unzip(zipped_atlas, exdir = unzipped_atlas)

}

## Read and wrange the BASINAtlas
#read the Basinatlas, level 12 is the highest resolution but for this analysis we use that one
# transformation to planar is required, since sf library assumes planar projection. First we also set s2 to false
sf_use_s2(FALSE)

basin_atlas <- st_read( "original_raw_data/BASINatlas/BasinATLAS_v10/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.shp") %>% 
  st_transform( 2163)  

#Due to the shitty constrain on name lengths in the variables of shapefiles, are nearly impossible to understand. 

##### A bit tedious, but change labels for discrete variables with readable names
labels.clz <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "clz_cl")
key_clz <- as.character(labels.clz$GEnZ_Name)
names(key_clz) <- labels.clz$GEnZ_ID

labels.cls <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "cls_cl")
key_cls <- as.character(labels.cls$GEnZ_Name)
names(key_cls) <- labels.cls$GEnZ_ID

labels.glc <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "glc_cl")
key_glc <- as.character(labels.glc$GLC_Name)
names(key_glc) <- labels.glc$GLC_ID

labels.pnv <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "pnv_cl")
key_pnv <- as.character(labels.pnv$PNV_Name)
names(key_pnv) <- labels.pnv$PNV_ID

labels.wet <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "wet_cl")
key_wet <- as.character(labels.wet$GLWD_Name)
names(key_wet) <- labels.wet$GLWD_ID

labels.tbi <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "tbi_cl")
key_tbi <- as.character(labels.tbi$Biome_Name)
names(key_tbi) <- labels.tbi$Biome_ID

labels.tec <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "tec_cl")
key_tec <- as.character(labels.tec$Ecoregion_Name)
names(key_tec) <- labels.tec$Eco_ID

labels.fmh <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "fmh_cl")
key_fmh <- as.character(labels.fmh$MHT_Name)
names(key_fmh) <- labels.fmh$MHT_ID

labels.fec <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "fec_cl")
key_fec <- as.character(labels.fec$MHT_Name)
names(key_fec) <- labels.fec$MHT_ID

labels.lit <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "lit_cl")
key_lit <- as.character(labels.lit$Class_Name)
names(key_lit) <- labels.lit$GLiM_ID

labels.gad <- read_excel(path="original_raw_data/BASINatlas/HydroATLAS_v10_Legends.xlsx", sheet = "gad_id")
key_gad <- as.character(labels.gad$Country_Name)
names(key_gad) <- labels.gad$Country_ID

#this is a file I made with better names for each variable in BASINAtlas
new.names.atlas <- read_csv("original_raw_data/pretty_names.csv")

basin_atlas <- basin_atlas %>%
  # drop_na(clz_cl_smj) %>% 
  mutate(clz_cl_smj = recode(clz_cl_smj, !!!key_clz, .default =NA_character_),
         cls_cl_smj = recode(cls_cl_smj, !!!key_cls, .default =NA_character_),
         glc_cl_smj = recode(glc_cl_smj, !!!key_glc, .default =NA_character_),
         pnv_cl_smj = recode(pnv_cl_smj, !!!key_pnv, .default =NA_character_),
         wet_cl_smj = recode(wet_cl_smj, !!!key_wet, .default =NA_character_),
         tbi_cl_smj = recode(tbi_cl_smj, !!!key_tbi, .default =NA_character_),
         tec_cl_smj = recode(tec_cl_smj, !!!key_tec, .default =NA_character_),
         fmh_cl_smj = recode(fmh_cl_smj, !!!key_fmh, .default =NA_character_),
         lit_cl_smj = recode(lit_cl_smj, !!!key_lit, .default =NA_character_),
         gad_id_smj = recode(gad_id_smj, !!!key_gad, .default =NA_character_)
  ) %>% 
  rename_at(vars(everything()), 
            ~ str_replace_all(., 
                              setNames(new.names.atlas$new_labels, new.names.atlas$codes_hydro_atlas))
  )


#and now we get the sites from GRiMeDB, turn it into a sf, and joint it with basin_atlas 
gis_df <- sites_df %>% 
  select(Site_Nid, Latitude, Longitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),  crs = 4326) %>% 
  st_transform( 2163) %>% 
  st_join(basin_atlas, join = st_within)
  
# Do a world map of the sampled sites ----

#we first download some base layers, such as land shapefiles, rivers and lakes
map_sc <- ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

rivers50 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
                        category = 'physical', returnclass = "sf" )

lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = "sf")

#For plotting we will summarise how many observations of concs and flux per site
sites_conc_flux <- sites_df %>% 
  select(Site_Nid, Latitude, Longitude) %>% 
  left_join(conc_df %>% 
              drop_na(orig_CH4unit) %>% 
              group_by(Site_Nid) %>% 
              summarise(n_conc= n()), by= "Site_Nid" ) %>% 
  left_join(flux_df %>% 
              group_by(Site_Nid) %>% 
              summarise(n_flux= n()), by= "Site_Nid"  ) %>% 
  mutate(
    n_conc = replace_na(n_conc, 0),
    n_flux = replace_na(n_flux, 0),
    type = case_when(
      n_conc > 0 & n_flux > 0 ~ "both",
      n_conc < 1 & n_flux > 0 ~ "only flux",
      n_conc > 0 & n_flux < 1 ~ "only concentrations",
      n_conc < 1 & n_flux < 1 ~ "none"),
    type = fct_relevel(type, "both", "only flux", "only concentrations", "none" )
  ) %>%
  st_as_sf( coords = c("Longitude", "Latitude"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

# quick look
sites_conc_flux %>% 
  group_by(type) %>% 
  tally()

# Now we do a world map with ggplot. Adding the background layers as well as the sampled sites
map_world <- 
  ggplot()+
  geom_sf(data=map_sc, fill="oldlace", colour="gray90", size=0.5)+
  geom_sf(data = rivers50 %>% filter(scalerank < 8), color= "dodgerblue3", size= .3)+
  geom_sf(data=lakes %>% filter(scalerank < 1), fill="aliceblue", color=NA)+
  geom_sf(data=sites_conc_flux %>% filter(type != "none"), 
          aes( color = type), 
          size = 1, alpha = .7)+
  scale_color_manual(values = c("#936639", "#a4ac86", "#ff9f1c"), name="")+
  coord_sf(crs = 4087, xlim=c(-18026400, 21026400), ylim=c(-7062156, 10602156))+
  labs(color= "")+
  theme_void()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.position = c(.12,.15),
        legend.text = element_text(size=13, color="black"),
        legend.justification = "center" )+
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1) ) )


## Extra: Add some histograms of water area and sampled area. We need to do both things by latitude and longitude ----
#function to run at custom accuracies, stolen from plyr package
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy} 

# Here we summuarise the number of sites by each latitudinal degree, and longitude
sites_lat <- sites_conc_flux %>% 
  st_transform(4326) %>% 
  filter(type != "none") %>% 
  mutate(lat = st_coordinates(.)[, "Y"] %>% round(0),
         type ="observations") %>% 
  st_drop_geometry() %>% 
  group_by(lat) %>% 
  summarise(n=n(),
            type=first(type)) %>% 
  dplyr::select(type, lat, n) 


sites_lon <- sites_conc_flux %>% 
  st_transform(4326) %>% 
  filter(type != "none") %>% 
  mutate(lon = st_coordinates(.)[, "X"] %>% round(0),
         type ="observations") %>% 
  st_drop_geometry() %>% 
  group_by(lon = round_any(lon,1)) %>% 
  summarise(n=n(),
            type=first(type)) %>% 
  dplyr::select(type, lon, n) 


# we need to find some geometries that are weird in the basinAtlas. Those are giving errors when doing calculations later on
basin_atlas_good <- st_is_valid(basin_atlas, reason = FALSE)

#check how many sites we filtered and the total
length(basin_atlas_good) - sum(basin_atlas_good, na.rm = TRUE)
length(basin_atlas$hybas_id)

# And now we do the same aggregation by lat/lon bands, using river area. 
#We remove first a fraction of sites with conflicted geometries
lat_rivers <- 
  basin_atlas %>% #[basin_atlas_good,] %>% 
  st_transform(4326) %>% 
  mutate(start_point = st_startpoint(st_cast(., "MULTIPOINT")),
         lat = st_coordinates(start_point)[,2] %>%  round(0) ) %>%
  st_drop_geometry() %>% 
  group_by(lat) %>% 
  summarise(riv_area = sum(river_area_sub_ha)) %>% 
  dplyr::select(lat,  riv_area) 


lon_rivers <- basin_atlas %>% #[basin_atlas_good,] %>% 
  st_transform(4326) %>% 
  mutate(start_point = st_startpoint(st_cast(., "MULTIPOINT")),
         lon = st_coordinates(start_point)[,1] %>%  round(0) ) %>% 
  st_drop_geometry() %>%
  group_by(lon= round_any(lon,1)) %>% 
  summarise(riv_area = sum(river_area_sub_ha)) %>% 
  dplyr::select(lon, riv_area)

#Make a dataframe for the latitude n of observations and river area, and both scaled from 0 to 1
together_lat <- lat_rivers %>% 
  mutate(type="river area",
         n=riv_area/max(riv_area)) %>% 
  dplyr::select(type, lat, n) %>% 
  bind_rows(sites_lat %>% 
              mutate(n=n/max(n)) %>% 
              dplyr::select(type, lat, n)) 

# Same for longitudes
together_lon <- lon_rivers %>% 
  mutate(type="river area",
         n=riv_area/max(riv_area)) %>% 
  dplyr::select(type, lon, n) %>% 
  bind_rows(sites_lon %>% 
              mutate(n=n/max(n)) %>% 
              dplyr::select(type,lon,  n)) 

#Do a ggplot of columns for the density of observations and river area, first latitude and then longitude
n_lats <- ggplot(together_lat, aes( x= lat, y=n, fill=type) )+
  geom_col(alpha=.5, position = "identity", width = 1)+
  scale_fill_manual(  values = c("gray15",  "dodgerblue3"))+
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
  scale_fill_manual(  values = c("gray15",  "dodgerblue3"))+
  theme_classic()+
  scale_x_continuous(expand=c(0,0), breaks = c(-120, -60, 0, 60, 120), name="", limits=c(-170, 180))+
  scale_y_continuous( expand=c(0,0), name="")+
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", color="transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color="transparent"))

# Now put them together with the world map, using patchwork. 
# The alignment with the real map latitude/longitude is not perfect because the world is not flat. 
# And it took a while to align the plots so don't touch it
map_world + 
  inset_element(n_lats, .93, .045, 1.01, .915 ) +
  inset_element(n_lons, 0.012, .84, .94, 1.02)


#ggsave("man/figures/map_hist.png", scale = 1, dpi = 400)


##### Assessment of how representative samples are of the whole world 
  #select only the month we need and make new columns for where we have observations or not
  df_together <- basin_atlas %>%
    st_drop_geometry() %>% 
    mutate(type = ifelse(hybas_id %in% gis_df$hybas_id, "sampled", "world")) %>%
    dplyr::select(hybas_id, type, q_avg_m3s:hdi_sub, -ends_with(c("s01", "s02", "s03", "s04", "s05", "s06", 
                                                                  "s07", "s08", "s09", "s10", "s11", "s12")),
                  -contains(c("up", "max", "min", "gdp")), -human_footprint_sub_93, -lake_area_catch_per) %>% 
    mutate(across(everything(), ~replace(., . ==  -999 , 0)),
           moisture_index_sub_avg = moisture_index_sub_avg+ 100,
           elevation_sub_average_m = elevation_sub_average_m + 415,
           air_temp_sub_avg_celsius = air_temp_sub_avg_celsius + 262) 

  names(df_together) %>% sort()
  
  df_together %>% 
    select(-c(hybas_id, type)) %>% 
    select_if(is.numeric) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value" ) %>% 
    ggplot(aes(value))+
    geom_histogram(bins = 80) +
    theme_classic()+
    facet_wrap(~variable, scales='free')
  
  vars_to_log <- df_together %>% 
    select_if(is.numeric) %>%
    select(contains(
    c("aridity_index_sub", "cover_sub", "glacier_sub_per", "gradient_stream_sub_dmkm", "human_population_sub", 
      "irrigated_area_sub_per", "karst_sub_per", "night_lights_sub_dgn", "vegetation", "protected_area", "q_avg_m3s",
      "reservoir_vol_millionm3", "river_area_sub_ha", "river_regulation_per", "river_volume_sub_thousandm3", 
      "road_density_sub_meterskm2", "soil_erosion_sub_kghectareyear", "soil_org_carbon_sub_tonneshectare", "urban_extent_sub_per"
      ))) %>%
    colnames(.)
  
  df_together_log <- df_together %>% 
    mutate(across(all_of(vars_to_log), ~log(.x + 1))) 
 
df_together %>% 
  summarise(across(where(is.numeric), min))
  

    
  
    # We do a PCA of the global dataset
  pca_all <- df_together_log %>% 
    select_if(is.numeric) %>% #we keep numeric variables, and remove the monthly ones.
    scale() %>% 
    prcomp()
  
  
  #26 axis to get to 90% of variation
  summary(pca_all)
  
  #List of the PCs
  ev_pca <- pca_all %>%
    tidy(matrix = "eigenvalues")
  
  #recover the original database, but only the columns we care, hybas_id and whether locations are sampled or not.
  pca_df <- pca_all %>%
    augment(df_together) %>%
    dplyr::select(hybas_id, type, starts_with(".fitted"))
  
  #Save the PCs that we want, the ones that give us 90% of the variation
  selected_pcas <- ev_pca %>% 
    filter(cumulative < 0.9) %>% 
    mutate(PC_rep = PC) 
  
  #make a vector of all possible combinations
  combinations <- selected_pcas %>% 
    expand(crossing(PC, PC_rep)) %>% 
    filter(PC != PC_rep) %>% 
    filter(!duplicated(paste0(pmax(PC, PC_rep), pmin(PC, PC_rep))))
  
  
  #prepare an empty df to save results
  dat_repr <- tibble(hybas_id = df_together$hybas_id,
                    obs = 0)
  
  
  #in case you want to save a plot of each step set this to true
  plot_the_space = FALSE
  
  plot_list <- list()
  
  #now we do a loop to run it for all combinations of PC axis, (800)
  for(i in 1:length(combinations$PC)){
    
    PC_to_select <- c(paste0(".fittedPC", combinations$PC[i]), paste0(".fittedPC", combinations$PC_rep[i]) )
    
    pca_df_selected <- pca_df %>% 
      dplyr::select(hybas_id, type, all_of(PC_to_select)) 
    
    colnames(pca_df_selected) <- c("hybas_id", "type", "PC_x", "PC_y")
    
    #make a hull of the sampled locations, to characterize the space represented in the samples
    hull <- pca_df_selected %>% 
      filter(type == "sampled") %>%
      slice(chull(PC_x, PC_y))
    
    # mark which sites are inside or outside the hull, in this given plane
    points_in_hull <- sp::point.in.polygon(pca_df_selected$PC_x, pca_df_selected$PC_y, hull$PC_x, hull$PC_y )
    points_in_hull <- if_else(points_in_hull >= 1, 1, 0)
    
    #make a plot of the space, optional parameter
    if(plot_the_space == TRUE){
      
   plot <- 
     ggplot()+
        geom_hex(data = pca_df_selected %>% filter(type == "world"), 
                 aes(PC_x, PC_y))+
        geom_polygon(data = hull, aes(PC_x, PC_y), alpha = 0.4, color= "red3")+
        geom_point(data = pca_df_selected %>% filter(type == "sampled"), 
                   aes(PC_x, PC_y), alpha=.5, color= "red3")+
        scale_fill_viridis_c()+
        theme_classic()+
        labs(x = paste(PC_to_select[1]), y = paste(PC_to_select[2]),
             title= paste(PC_to_select[1], "x", PC_to_select[2]))
   
   plot_list[[i]] <- plot   
   #ggsave(paste0( "man/figures/PCAs_examples/", PC_to_select[1], "x", PC_to_select[2], ".png") )
    }
    
    #update the file
    dat_repr <- dat_repr %>%
      mutate(obs = obs + points_in_hull)
    
    print(paste("done ",i ,"of", length(combinations$PC) ))
    
  }

plot_list[[1]]  

## Plot all PCs against ieach other
#library(cowplot)
#allplots <- do.call(plot_grid, c(plot_list, 
#                     align = "h",
#                     axis = 'tb'))
#
#ggsave("man/figures/allin1.pdf" ,allplots, width = 100, height = 100, limitsize = FALSE)
 
   
dat_repr_gis <- dat_repr %>% 
  mutate(representativeness = obs/length(combinations$PC)) %>% 
  dplyr::select(-obs) %>% 
  left_join(basin_atlas %>% select(hybas_id, geometry), by ="hybas_id" ) %>% 
  st_sf() %>%
  st_transform(4326) #st_transform("+proj=eqearth +wktext") 


ggplot(dat_repr_gis, aes(representativeness))+
  geom_density()

 
#download world map
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform(4326) #st_transform("+proj=eqearth +wktext") 

#make a hex grid for the world and keep only terrestrial masses
grid <- st_make_grid(
  world,
  n = c(250, 250), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>% 
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid) 

#join the data to the grid
repr_hexes <- dat_repr_gis %>% 
  st_join(grid, join = st_intersects)

#now aggregate all the  data for each hex, do mean value
repr_hexes_avg <- repr_hexes %>% 
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(repr = mean(representativeness, na.rm = TRUE)) %>% 
  right_join(grid, by="index") %>%
  st_sf() 

 
map_rep <- ggplot() +
  geom_sf(data = repr_hexes_avg, color = NA, aes(fill = repr))+
  geom_sf(data = world, color= "gray90", fill = NA)+
  geom_sf(data = rivers50 %>% 
            filter(scalerank < 8), color= "dodgerblue3", size = .3)+
  geom_sf(data = lakes %>% 
            filter(scalerank < 1), fill="aliceblue", color=NA)+
  scale_fill_distiller(type = "seq", direction = -1, palette = "Oranges", na.value = "gray", #RdGy
     name = "Representativeness")+
  coord_sf(crs = 4087, xlim=c(-18026400, 21026400), ylim=c(-7062156, 10602156))+
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(.56, .11),
        legend.direction = "horizontal",
        legend.key.height  = unit(.3, 'cm'),
        legend.key.width =  unit(.9, 'cm'),
        legend.title = element_text(size = 14, face = "bold"))

map_sites <- map_world + 
  inset_element(n_lats, .93, .045, 1.01, .915 ) +
  inset_element(n_lons, 0.012, .84, .94, 1.02)


maps_both <- map_sites +
  map_rep + 
  plot_layout(ncol=1, tag_level = 'keep')  +
  plot_annotation(tag_levels = list(c('a', '', '', 'b'))) & 
  theme(plot.tag.position = c(0, .92), plot.tag = element_text(size=17, face= "bold"))

ggsave("man/figures/maps_both_xs.png", maps_both, dpi = 500, scale = 1.3)  


# new figure for emily with Q, and other stuff
library(scales)

theme_grime <- function(){
  list(
    scale_color_manual(values = c("#ff9f1c",  "#936639", "#a4ac86"), drop = FALSE), 
    scale_fill_manual(values = c("#ff9f1c", "#936639", "#a4ac86"), drop = FALSE)
  ) 
}

conc_sites <- conc_df %>% 
  left_join(sites_df, by = "Site_Nid")


meth_lat <- conc_sites %>% 
  filter(Q > 0, CH4mean > 0) %>% 
  ggplot(aes(Latitude, CH4mean))+
  geom_point(color = "#ff9f1c", alpha = .3)+
  scale_y_log10(limits=c(0.0001, 100), 
                label=trans_format("log10",math_format(10^.x)))+
  theme_classic()+
  labs(x= "Latitude", y = expression(CH[4]~(umol~L^-1)))


conc_sites %>% 
  filter(Q > 0, CH4mean > 0) %>% 
  ggplot(aes(Q, CH4mean))+
  geom_point(color = "#ff9f1c", alpha = .5)+
  scale_x_log10(label=trans_format("log10",math_format(10^.x)),
                breaks = c(0.0001, 0.01, 1, 100, 10000, 1000000))+
  scale_y_log10(label=trans_format("log10",math_format(10^.x)),
                limits=c(0.0001, 100))+
  theme_classic()+
  labs(x= expression(Discharge~(m^3~s^-1)), y = expression(CH[4]~(umol~L^-1)))


mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(57)

meth_q <- 
conc_sites %>% 
  filter(Q > 0, CH4mean > 0 ) %>% 
  ggplot()+
  geom_point(aes(Q, CH4mean ), size = 2, alpha = .1)+
  geom_point(data = conc_sites %>% 
               filter(Q > 0, CH4mean > 0 ) %>%
               add_count(Site_Nid) %>%
               filter(n > 30),
             aes(Q, CH4mean , color= Site_Nid), size = 2, alpha = .3)+
  geom_smooth(data = conc_sites %>% 
                filter(Q > 0, CH4mean > 0 ) %>%
                add_count(Site_Nid) %>%
                filter(n > 30),
              aes(Q, CH4mean , color= Site_Nid, group= Site_Nid), se=FALSE, method = "lm")+
  scale_color_manual(values = mycolors)+
  scale_x_log10(label=trans_format("log10",math_format(10^.x)),
               breaks = c(0.0001, 0.01, 1, 100, 10000, 1000000))+
  scale_y_log10(label=trans_format("log10",math_format(10^.x)),
                limits=c(0.0001, 100))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x= expression(Discharge~(m^3~s^-1)), y = expression(CH[4]~(umol~L^-1)))


meth_order <- conc_sites %>% 
  filter(Strahler_order> 0, CH4mean > 0) %>% 
  ggplot(aes(Strahler_order, CH4mean, group = Strahler_order))+
  geom_jitter(color = "#ff9f1c", alpha = .3)+
  geom_boxplot(outlier.shape = NA, fill = NA)+
  scale_y_log10(limits=c(0.0001, 100),
                label=trans_format("log10",math_format(10^.x)))+
  scale_x_continuous(breaks = 1:8)+
  theme_classic()+
  labs(x= "Strahler order", y = "")

meth_catchment <- conc_sites %>% 
  filter(CH4mean > 0, Catchment_size_km2 > 0.01) %>% 
  ggplot(aes(Catchment_size_km2, CH4mean))+
  geom_point(color = "#ff9f1c", alpha = .3)+
  scale_y_log10(limits=c(0.0001, 100), 
                label=trans_format("log10",math_format(10^.x)))+
  scale_x_log10(label=trans_format("log10",math_format(10^.x)))+
  theme_classic()+
  labs(x= expression(Catchment~area~(km^2)), y="")

meth_lat + meth_order + meth_q + meth_catchment  +
  plot_layout(ncol = 2)

ggsave("man/figures/river_size_conc.png", scale= .9)

# same figure for fluxes  
flux_sites <- flux_df %>% 
  left_join(conc_df, by = c("Site_Nid", "Date_start")) %>% 
  left_join(sites_df, by = "Site_Nid")


meth_flux_lat <- flux_sites %>% 
  filter(Q > 0, Diffusive_CH4_Flux_Mean > 0) %>% 
  ggplot(aes(Latitude, Diffusive_CH4_Flux_Mean))+
  geom_point(color = "#a4ac86", alpha = .3)+
  scale_y_log10(limits=c(0.0001, 1000), 
                label=trans_format("log10",math_format(10^.x)))+
  theme_classic()+
  labs(x= "Latitude", y = expression(CH[4]~emissions~(umol~m^-2~d^-1)))


flux_sites %>% 
  filter(Q > 0, Diffusive_CH4_Flux_Mean > 0) %>% 
  ggplot(aes(Q, Diffusive_CH4_Flux_Mean))+
  geom_point(color = "#a4ac86", alpha = .5)+
  scale_x_log10(label=trans_format("log10",math_format(10^.x)),
                breaks = c(0.0001, 0.01, 1, 100, 10000, 1000000))+
  scale_y_log10(label=trans_format("log10",math_format(10^.x)),
                limits=c(0.0001, 100))+
  theme_classic()+
  labs(x= expression(Discharge~(m^3~s^-1)), y = expression(CH[4]~emissions~(umol~m^-2~d^-1)))


mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(15)

meth_flux_q <- 
  flux_sites %>% 
  filter(Q > 0, Diffusive_CH4_Flux_Mean > 0 ) %>% 
  ggplot()+
  geom_point(aes(Q, Diffusive_CH4_Flux_Mean ), size = 2, alpha = .1)+
  geom_point(data = flux_sites %>% 
               filter(Q > 0, Diffusive_CH4_Flux_Mean > 0 ) %>%
               add_count(Site_Nid) %>%
               filter(n > 30),
             aes(Q, Diffusive_CH4_Flux_Mean , color= Site_Nid), size = 2, alpha = .3)+
  geom_smooth(data = flux_sites %>% 
                filter(Q > 0, Diffusive_CH4_Flux_Mean > 0 ) %>%
                add_count(Site_Nid) %>%
                filter(n > 30),
              aes(Q, Diffusive_CH4_Flux_Mean , color= Site_Nid, group= Site_Nid), se=FALSE, method = "lm")+
  scale_color_manual(values = mycolors)+
  scale_x_log10(label=trans_format("log10",math_format(10^.x)),
                breaks = c(0.0001, 0.01, 1, 100, 10000, 1000000))+
  scale_y_log10(label=trans_format("log10",math_format(10^.x)),
                limits=c(0.0001, 1000))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x= expression(Discharge~(m^3~s^-1)), y = expression(CH[4]~emissions~(umol~m^-2~d^-1)))


meth_flux_order <- flux_sites %>% 
  filter(Strahler_order> 0, Diffusive_CH4_Flux_Mean > 0) %>% 
  ggplot(aes(Strahler_order, Diffusive_CH4_Flux_Mean, group = Strahler_order))+
  geom_jitter(color = "#a4ac86", alpha = .3)+
  geom_boxplot(outlier.shape = NA, fill = NA)+
  scale_y_log10(limits=c(0.0001, 1000),
                label=trans_format("log10",math_format(10^.x)))+
  scale_x_continuous(breaks = 1:8)+
  theme_classic()+
  labs(x= "Strahler order", y = "")

meth_flux_catchment <- flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0, Catchment_size_km2 > 0.01) %>% 
  ggplot(aes(Catchment_size_km2, Diffusive_CH4_Flux_Mean))+
  geom_point(color = "#a4ac86", alpha = .3)+
  scale_y_log10(limits=c(0.0001, 1000), 
                label=trans_format("log10",math_format(10^.x)))+
  scale_x_log10(label=trans_format("log10",math_format(10^.x)))+
  theme_classic()+
  labs(x= expression(Catchment~area~(km^2)), y="")

meth_flux_lat + meth_flux_order + meth_flux_q + meth_flux_catchment  +
  plot_layout(ncol = 2)         


ggsave("man/figures/river_size_flux.png", scale= .9)


