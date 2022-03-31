
# Load packages -----

library(tidyverse)
library(broom)
library(ggthemes)
library(patchwork)
#library(plotbiomes) #this is needed for the plot of biomes
library(rnaturalearth)
library(sf)
library(lwgeom)
library(readxl)


# read /download files ----
path_to_dropbox <-  "C:/Users/gero0008/Dropbox/SCIENCE/PostDoc/MethDB2.0" #gerards pc
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

basin_atlas <- st_read( "original_raw_data/BASINatlas/BasinATLAS_v10/BasinATLAS_v10_shp/BasinATLAS_v10_lev10.shp") %>% 
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
new.names.atlas <- read_csv("original_raw_data/BASINatlas/pretty_names.csv")

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
  geom_sf(data=map_sc, fill="gray20", colour=NA, size=0.5)+
  geom_sf(data = rivers50 %>% filter(scalerank < 8), color= "steelblue4")+
  geom_sf(data=lakes %>% filter(scalerank < 1), fill="aliceblue", color=NA)+
  geom_sf(data=sites_conc_flux %>% filter(type != "none"), 
          aes( color = type), 
          size=2, alpha=.7)+
  scale_color_manual(values = c("#936639", "#a4ac86", "#ff9f1c"), name="")+
  coord_sf(crs = 4087, xlim=c(-18026400, 21026400), ylim=c(-7062156, 10602156))+
  labs(color= "")+
  theme_map()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.background=element_rect(fill="white"),
        legend.position=c(.12,.15),
        legend.text = element_text(size=13),
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
length(basin_atlas_good) - sum(basin_atlas_good)
length(basin_atlas$hybas_id)

# And now we do the same aggregation by lat/lon bands, using river area. 
#We remove first a fraction of sites with conflicted geometries
lat_rivers <- 
  basin_atlas[basin_atlas_good,] %>% 
  st_transform(4326) %>% 
  mutate(start_point = st_startpoint(st_cast(., "MULTIPOINT")),
         lat = st_coordinates(start_point)[,2] %>%  round(0) ) %>%
  st_drop_geometry() %>% 
  group_by(lat) %>% 
  summarise(riv_area = sum(river_area_sub_ha)) %>% 
  dplyr::select(lat,  riv_area) 


lon_rivers <- basin_atlas[basin_atlas_good,] %>% 
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
  scale_fill_manual(  values = c("gray15",  "steelblue4"))+
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
  scale_fill_manual(  values = c("gray15",  "steelblue4"))+
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

# Now put them together with the world map, using patchwork. 
# The alignment with the real map latitude/longitude is not perfect because the world is not flat. 
# And it took a while to align the plots so don't touch it
map_world + 
  inset_element(n_lats, .93, .045, 1.01, .915 ) +
  inset_element(n_lons, 0.012, .84, .94, 1.02)


ggsave("man/figures/map_hist.png", scale=1, dpi = 400)



# figure of MAT vs MAP with global distribution ----
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


##### Assessment of how representative samples are of the whole world 
  #select only the month we need and make new columns for where we have observations or not
  df_together <- basin_atlas %>%
    mutate(type = ifelse(hybas_id %in% gis_df$hybas_id, "sampled", "world")) %>% 
    st_drop_geometry()
  
  
  # We do a PCA of the global dataset
  pca_all <- df_together %>% 
    select_if(is.numeric) %>% #we keep numeric variables, and remove the monthly ones.
    dplyr::select(q_avg_m3s:hdi_sub, -ends_with(c("s01", "s02", "s03", "s04", "s05", "s06", 
                                       "s07", "s08", "s09", "s10", "s11", "s12")),
                  -contains(c("up", "max", "min", "gdp")), -human_footprint_sub_93) %>% 
    mutate(across(everything(), ~replace(., . ==  -999 , 0))) %>% 
    scale() %>% 
    prcomp()
  
  
  #30 axis to get to 90% of variation
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
    filter(PC != PC_rep)
  
  #prepare an empty df to save results
  dat_repr <- tibble(hybas_id = df_together$hybas_id,
                    obs = 0)
  
  
  #in case you want to save a plot of each step set this to true
  plot_the_space = FALSE
  
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
      
      ggplot()+
        geom_hex(data = pca_df_selected %>% filter(type == "world"), 
                 aes(PC_x, PC_y))+
        geom_polygon(data = hull, aes(PC_x, PC_y), alpha = 0.4, , color= "red3")+
        geom_point(data = pca_df_selected %>% filter(type == "sampled"), 
                   aes(PC_x, PC_y), alpha=.5, color= "red3")+
        scale_fill_viridis_c()+
        theme_classic()+
        labs(x = paste(PC_to_select[1]), y = paste(PC_to_select[2]),
             title= paste(PC_to_select[1], "x", PC_to_select[2]))
      ggsave(paste0( "man/figures/PCAs_examples/", PC_to_select[1], "x", PC_to_select[2], ".png") )
    }
    
    #update the file
    dat_repr <- dat_repr %>%
      mutate(obs = obs + points_in_hull)
    
    print(paste("done ",i ,"of", length(combinations$PC) ))
    
  }
  
dat_repr_gis <- dat_repr %>% 
  mutate(representativeness = obs/length(combinations$PC)) %>% 
  dplyr::select(-obs) %>% 
  left_join(basin_atlas %>% select(hybas_id, geometry), by ="hybas_id" ) %>% 
  st_sf() %>%
  st_transform("+proj=eqearth +wktext") 


ggplot(dat_repr, aes(representativeness))+
  geom_density()

 
#download world map
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

#make a hex grid for the world and keep only terrestrial masses
grid <- st_make_grid(
  world,
  n = c(250, 250), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>% 
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid) 



#join the meth data to the grid
repr_hexes <- st_join(dat_repr_gis, grid, join = st_intersects)

#now aggregate all the meth data for each hex, do means and sum for area.
repr_hexes_avg <- repr_hexes %>% 
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(repr = mean(representativeness, na.rm = TRUE)) %>% 
  right_join(grid, by="index") %>%
  st_sf() 

 
ggplot() +
  geom_sf(data = repr_hexes_avg, color = NA, aes(fill = repr))+
  geom_sf(data = rivers50 %>% filter(scalerank < 8), color= "steelblue4", size = .3)+
  geom_sf(data=lakes %>% filter(scalerank < 1), fill="aliceblue", color=NA)+
  scale_fill_distiller(type = "seq", direction = -1, palette = "Oranges", na.value = "gray",
     name = "Representativeness")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height  = unit(.4, 'cm'),
        legend.key.width =  unit(.8, 'cm'))

ggsave("man/figures/map_repr.png", dpi=500)  
  
              
              
              
              

