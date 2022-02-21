library(tidyverse)
library(readxl)
library(openxlsx)
library(here)
library(patchwork)

#Load custom functions
source("R/convert_unit_functions.R")

#Load unit conversion table
unit_convert_table <- readRDS("R/unit_convert_table.rds")

#load GIS data
gis_df <- read_csv(file.path(path_to_dropbox, "db_processingR", 
                           "methdb_explore", "data", 
                           "methdb_gis.csv"))  %>%
  mutate(Site_Nid = as.character(Site_Nid))


gis_df2 <- gis_df %>%
   group_by(Site_Nid) %>%
   summarize(across(everything(), ~.x[1]))
 
dub_sites <- which(table(gis_df$Site_Nid)>1)
filter(gis_df, Site_Nid %in% names(dub_sites))

which(table(gis_df2$Site_Nid)>1)

setdiff(unique(gis_df2$Site_Nid), unique(gis_df$Site_Nid))
setdiff(unique(gis_df$Site_Nid), unique(gis_df2$Site_Nid))

dim(gis_df)
dim(gis_df2)

#load methdb papers table
papers_df <- read_excel(file.path(path_to_dropbox, MethDB_filename),
                    sheet = "Papers",  guess_max = 250) 

last_row = which(papers_df$Title == "DELETED/REJECTED") - 1

papers_df <- papers_df %>%
  slice(1:last_row) %>%
  drop_na(Publication_Nid) %>%
  select_if(~sum(!is.na(.)) > 0)


#rename columns
names(papers_df) <- gsub(" ", "", names(papers_df))


#load methdb site table
sites_df <- read_excel(file.path(path_to_dropbox, MethDB_filename),
                    sheet = "MethDB_2_sites", guess_max = 3500)  %>% 
  # rename(Elevation_m = Elevation_m_reported) %>%
  mutate(Site_Nid = as.character(Site_Nid)) %>%
  left_join(gis_df %>% 
              select(Site_Nid, lat_new = lat, lon_new = lon, 
                     elevation_m_new = z_m_combined, 
                     subcatch_area_km, catch_area_km, slope_m_m) %>%
              distinct(),
            by = "Site_Nid")
# 
# dup_sites <- table(sites_df$Site_Nid)
# dup_sites <- names(dup_sites[which(dup_sites>1)])
# 
# site_df_duplicated <- filter(sites_df, Site_Nid %in% dup_sites) %>%
#   arrange(Site_Nid, Publication_Nid)
# 
# write.csv(site_df_duplicated, file = file.path(path_to_dropbox, 
#                                                "db_processingR", 
#                                                "MethDB_sites_duplicated.csv"), 
#           row.names = FALSE)
          
#rename columns
names(sites_df) <- gsub(" ", "", names(sites_df))
names(sites_df) <- gsub("\\(m\\)", "_m", names(sites_df))

#load methdb concentration table and bind with elevation
concentrations <- read_excel(file.path(path_to_dropbox, MethDB_filename),
                             sheet = "MethDB_2_conc", guess_max = 20000) %>% 
  mutate(Site_Nid = as.character(Site_Nid),
         across(contains("Date_"), ~as.Date(.x))) %>%
  left_join(select(sites_df, Site_Nid, Elevation_m)) %>%
  left_join(select(gis_df, Site_Nid, elevation_m_new = z_m_combined)) %>% 
  filter(!is.na(Publication_Nid) & !is.na(Site_Nid))


#rename columns
names(concentrations) <- gsub(" ", "", names(concentrations))
names(concentrations) <- gsub("\\(", "", names(concentrations))
names(concentrations) <- gsub("\\)", "", names(concentrations))
names(concentrations) <- gsub("%", "percent", names(concentrations))
names(concentrations) <- gsub("/", "", names(concentrations))
names(concentrations) <- gsub("Flux\\?", "FluxYesNo", names(concentrations))
# names(concentrations) <- gsub("WaterTemp_", "WaterTemp", names(concentrations))


dim(data.frame(concentrations[which(is.na(concentrations$Date_start )),]))
unique(concentrations$Publication_Nid[which(is.na(concentrations$Date_start ))])

dim(data.frame(concentrations[which(is.na(concentrations$Date_end )),]))
data.frame(concentrations[which(is.na(concentrations$Site_Nid)),])

summary(select(concentrations, contains("Date")))


#There are two types of dates in this dataset. some are excel format, others are m/d/y. 
# concentrations <- concentrations %>%
#   mutate(across(c("SampleDatestart", "SampleDateend"), 
#                 convertToDate, .names = "{col}_v1"))
# 
# concentrations$SampleDatestart_v1[which(is.na(concentrations$SampleDatestart_v1))] <- 
#   as.Date(concentrations$SampleDatestart[which(is.na(concentrations$SampleDatestart_v1))], format = "%m/%d/%Y")
# concentrations$SampleDateend_v1[which(is.na(concentrations$SampleDateend_v1))] <- 
#   as.Date(concentrations$SampleDateend[which(is.na(concentrations$SampleDateend_v1))], format = "%m/%d/%Y")
# 
# concentrations <- concentrations %>%
#   select(-SampleDatestart, -SampleDateend) %>%
#   rename(SampleDatestart = SampleDatestart_v1,
#          SampleDateend = SampleDateend_v1) 




#load methdb fluxes
fluxes <- read_excel(file.path(path_to_dropbox, MethDB_filename), 
                     sheet = "MethDB_2_flux", guess_max = 4000) %>%
  mutate(Site_Nid = as.character(Site_Nid))

#Change names of dates, seasons, and counts
names(fluxes)[grepl("Date", names(fluxes))] <- c("Date_start", "Date_end")
names(fluxes)[grepl("Season", names(fluxes))] <- c("Season")
                                                   
names(fluxes)[grepl("Count", names(fluxes))] <- paste0("SampleCount_", c("Diffusive", "Bubble")) 

names(fluxes) <- gsub("\\..*","",names(fluxes))

#rename columns
names(fluxes) <- gsub(" ", "", names(fluxes))



fluxes <- fluxes %>%
  select(-which(names(fluxes) == "")) %>%
  mutate(across(contains("Date"), ~as.Date(.x))) %>%
  filter(!is.na(Site_Nid))

data.frame(fluxes[which(is.na(fluxes$Date_start )),])
data.frame(fluxes[which(is.na(fluxes$Date_end )),])$Publication_Nid
data.frame(fluxes[which(is.na(fluxes$Site_Nid)),])


summary(select(fluxes, contains("Date")))

concs_without_mean_ch4 <- concentrations %>% 
  mutate(CH4mean_numeric= as.numeric(CH4mean)) %>% 
  filter(is.na(CH4mean_numeric) == TRUE) %>% 
  select(Publication_Nid, Site_Nid, CH4min:CH4unit) 


#write.csv(concs_without_mean_ch4, file = file.path(path_to_dropbox,
#                                              "MethDB_concs_without_ch4_mean.csv"),
#        row.names = FALSE)

#concs_without_mean_ch4 %>% 
#  count(CH4unit) %>% 
#  print(n=50)

#Are all conc and flux sites in the sites table?
setdiff(concentrations$Site_Nid, sites_df$Site_Nid)
setdiff(fluxes$Site_Nid, sites_df$Site_Nid)

#Vice versa. But know that some sites should not be in the conc/flux datasets
# setdiff(sites_df$Site_Nid, concentrations$Site_Nid)
# setdiff(sites_df$Site_Nid, fluxes$Site_Nid)


#Convert concentration and fluxes to uM and mmol m-2 d-1

conc_df <- convert_conc_units(concentrations, unit_convert_table)
flux_df <- convert_flux_units(fluxes, unit_convert_table)


save(conc_df, flux_df, 
     sites_df, papers_df, gis_df,
     file = file.path(path_to_dropbox, "db_processingR", 
                      "MethDB_tables_converted.rda"))

#Observations missing siteID
conc_missing_SiteNID <- filter(conc_df, Site_Nid %in% setdiff(conc_df$Site_Nid, sites_df$Site_Nid)) 
flux_missing_SiteNID <- filter(flux_df, Site_Nid %in% setdiff(flux_df$Site_Nid, sites_df$Site_Nid)) 

write.csv(conc_missing_SiteNID, file.path(path_to_dropbox, "db_processingR",
                                         "Conc_missing_SiteNid.csv"), row.names = FALSE)

write.csv(flux_missing_SiteNID, file.path(path_to_dropbox, "db_processingR",
                                          "Flux_missing_SiteNid.csv"), row.names = FALSE)


# load(file.path(path_to_dropbox, "db_processingR", 
#                "MethDB_tables_converted.rda"))



#output files for Yale and database
papers_out <- papers_df

sites_out <- sites_df %>%
  select(-`Basin/Region`, -Country, -Continent,
         -Land_use, -System_size, -Depth_m, 
         -Width_m, -`avgQ_m3/s`) %>%
  filter(!grepl("DIT", Channel_type), 
         !grepl("CAN", Channel_type), 
         !grepl("DD", Channel_type))

conc_out <- conc_df %>%
  left_join(select(sites_df, Site_Nid, Channel_type)) %>%
  filter(Aggregated_Space != "Yes", 
         !grepl("DIT", Channel_type), 
         !grepl("CAN", Channel_type), 
         !grepl("DD", Channel_type)) %>%
  select(-Season, -CO2measurementtype, -FluxYesNo, -Channel_type)


flux_out <- flux_df %>%
  left_join(select(sites_df, Site_Nid, Channel_type)) %>%
  filter(Aggregated_Space != "Yes", 
         !grepl("DIT", Channel_type), 
         !grepl("CAN", Channel_type), 
         !grepl("DD", Channel_type)) %>%
  select(-Season, -Channel_type)

setdiff(concentrations$Site_Nid, sites_df$Site_Nid)
setdiff(conc_out$Site_Nid, sites_out$Site_Nid)

setdiff(fluxes$Site_Nid, sites_df$Site_Nid)
setdiff(flux_out$Site_Nid, sites_out$Site_Nid)

conc_out_missing_SiteID <- filter(conc_out, Site_Nid %in% setdiff(conc_out$Site_Nid, sites_out$Site_Nid)) 
flux_out_missing_siteID <- filter(flux_out, Site_Nid %in% setdiff(flux_out$Site_Nid, sites_out$Site_Nid)) 


sites_out %>% data.frame() %>% head()
conc_out %>% data.frame() %>% head()
flux_out %>% data.frame() %>% head()


save(papers_out, sites_out, conc_out, 
     file = file.path(path_to_dropbox, "db_processingR", 
                      paste0("GRiMe_tables_converted_Yale_", Sys.Date(), ".rda")))

save(papers_out, sites_out, conc_out, flux_out,
     file = file.path(path_to_dropbox, "db_processingR", 
                      paste0("GRiMe_tables_converted_Internal_", Sys.Date(), ".rda")))

write.csv(papers_out, file.path(path_to_dropbox, "db_processingR", 
                                paste0("GRiMe_papers_", Sys.Date(), ".csv")), 
          row.names = FALSE)

write.csv(sites_out, file.path(path_to_dropbox, "db_processingR", 
                                paste0("GRiMe_sites_", Sys.Date(), ".csv")), 
          row.names = FALSE)

write.csv(conc_out, file.path(path_to_dropbox, "db_processingR", 
                               paste0("GRiMe_concentrations_", Sys.Date(), ".csv")), 
          row.names = FALSE)

write.csv(flux_out, file.path(path_to_dropbox, "db_processingR", 
                              paste0("GRiMe_fluxes_", Sys.Date(), ".csv")), 
          row.names = FALSE)


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
# library(corrr)
# corr.ch4.everything <- concs.gis  %>% 
#   select_if(is.numeric) %>% 
#   select(CH4_umol_l, q_avg_m3s:human_footprint_up_09) %>% 
#   filter(CH4_umol_l > 0.01) %>% 
#   mutate(CH4_umol_l = log(CH4_umol_l)) %>% 
#   correlate() %>% focus(CH4_umol_l)
