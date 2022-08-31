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

#New GIS file that only contains lat, long, and elevation
gis_df3 <- read_csv(file.path(path_to_dropbox, "db_processingR", 
                              "GIS", 
                              "methdb_z_estimated.csv")) %>%
  mutate(Site_Nid = as.character(Site_Nid)) %>%
  distinct()

dim(gis_df3)

length(unique(gis_df3$Site_Nid))

#old gis files
length(unique(gis_df2$Site_Nid))
length(unique(gis_df$Site_Nid))

setdiff(unique(gis_df3$Site_Nid), unique(gis_df$Site_Nid))
missing_sites <- setdiff(unique(gis_df$Site_Nid), unique(gis_df3$Site_Nid))


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
  mutate(Site_Nid = as.character(Site_Nid), 
         Elevation_m = as.numeric(Elevation_m)) 



#Check is all sites are in GIS file
length(unique(gis_df3$Site_Nid))
length(unique(sites_df$Site_Nid))
setdiff(unique(sites_df$Site_Nid), unique(gis_df3$Site_Nid))
setdiff(unique(gis_df3$Site_Nid), unique(sites_df$Site_Nid))

filter(sites_df, Site_Nid %in% setdiff(unique(sites_df$Site_Nid), unique(gis_df3$Site_Nid))) %>% data.frame()

aggregated_sites <- filter(sites_df, `Aggregated?` == "Yes") %>% pull(Site_Nid)
filter(sites_df, `Aggregated?` == "Yes") %>% dim()

filter(gis_df3, Site_Nid %in% aggregated)

sites_df <- sites_df %>%
  left_join(gis_df3 %>% 
              select(Site_Nid, 
                     lat_new = lat_new, lon_new = lon_new, 
                     # lat_old, lon_old,
                     elevation_estimated_m),
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
  left_join(select(sites_df, Site_Nid, Elevation_m, 
                   elevation_m_new = elevation_estimated_m)) %>%
  # left_join(select(gis_df, Site_Nid, elevation_m_new = z_m_combined)) %>% 
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

# filter(conc_df, is.na(CH4mean) & orig_CH4unit == "ppm CH4") 

save(conc_df, flux_df, 
     sites_df, papers_df, gis_df,
     file = file.path(path_to_dropbox, "db_processingR", 
                      "MethDB_tables_converted.rda"))

#Observations missing siteID
conc_missing_SiteNID <- filter(conc_df, Site_Nid %in% setdiff(conc_df$Site_Nid, sites_df$Site_Nid)) 
flux_missing_SiteNID <- filter(flux_df, Site_Nid %in% setdiff(flux_df$Site_Nid, sites_df$Site_Nid)) 

write.csv(conc_missing_SiteNID, file.path(path_to_dropbox, 
                                          "db_processingR",
                                          paste0("Conc_missing_SiteNid_",
                                                 Sys.Date(),
                                                 ".csv")),
          row.names = FALSE)

write.csv(flux_missing_SiteNID, file.path(path_to_dropbox, 
                                          "db_processingR",
                                          paste0("Flux_missing_SiteNid", 
                                                 Sys.Date(), 
                                                 ".csv")),
          row.names = FALSE)


# load(file.path(path_to_dropbox, "db_processingR", 
#                "MethDB_tables_converted.rda"))



#output files for Yale and database
papers_out <- papers_df %>%
  rename(Author_last_name = Authorlastname, 
         Pub_year = PubYear, 
         Additional_data = `Additionaldata?`, 
         # Paper_DOI = PaperDOI, 
         Data_DOI_primary = DataDOIprimary, 
         Data_DOI_supporting = DataDOIsupporting)

sites_out <- sites_df %>%
  select(-Country, -Continent,
         -Land_use, -System_size, -Depth_m, 
         -Width_m) %>%
  # filter(!grepl("DIT", Channel_type), 
  #        !grepl("CAN", Channel_type), 
  #        !grepl("DD", Channel_type)) %>%
  rename(Aggregated = `Aggregated?`, 
         N_sites_aggregated = NSitesAggregated, 
         Basin_Region = `Basin/Region`, 
         avgQ_m3_pers =  `avgQ_m3/s`, 
         Latitude_snapped = lat_new, 
         Longitude_snapped = lon_new, 
         Elevation_estimated_m = elevation_estimated_m
  ) %>%
  select(Publication_Nid:Channel_type, Latitude_snapped:Elevation_estimated_m, Comments, everything())

conc_out <- conc_df %>%
  # left_join(select(sites_df, Site_Nid, Channel_type)) %>%
  # filter(Aggregated_Space != "Yes", 
  #        !grepl("DIT", Channel_type), 
  #        !grepl("CAN", Channel_type), 
  #        !grepl("DD", Channel_type)) %>%
  # select(-Channel_type) %>%
  select(-Season, -CO2measurementtype, 
         -contains("aggregated", ignore.case = FALSE)) %>%
  rename(WaterTemp_degC = WaterTemp_actual, 
         WaterTemp_degC_estimated = WaterTemp_est) %>%
  select(Publication_Nid:Aggregated_Time, 
         FluxYesNo,
         SampleCount, 
         CH4min:CH4median,
         CO2min:CO2median,
         N2Omin:N2Omedian,
         WaterTemp_degC:DO_percentsat, 
         Q, 
         contains("actual"), 
         Comments, 
         contains("new_"), 
         contains("orig_"), 
         everything())


names(conc_out) <- gsub("unit", "_unit", names(conc_out))
names(conc_out) <- gsub("_actual", "", names(conc_out))
names(conc_out) <- gsub("actual", "", names(conc_out))


conc_out %>% data.frame() %>% head()


flux_out <- flux_df %>%
  # left_join(select(sites_df, Site_Nid, Channel_type)) %>%
  # filter(Aggregated_Space != "Yes", 
  #        !grepl("DIT", Channel_type), 
  #        !grepl("CAN", Channel_type), 
  #        !grepl("DD", Channel_type)) %>%
  # select(-Channel_type) %>%
  select(-Season) %>%
  rename(Total_Method = Totalmethod, 
         orig_Diffusive_Flux_unit = Diffusive_Flux_unit, 
         orig_Eb_CH4_Flux_unit = Eb_CH4_Flux_unit,
         orig_Total_Flux_unit = Total_Flux_unit,
         orig_CO2_Flux_unit = CO2_Flux_unit,
         orig_N2O_Flux_unit = N2O_Flux_unit) %>%
  select(Publication_Nid:Diffusive_CH4_Flux_Median, 
         SampleCount_Diffusive:Eb_CH4_Flux_median, 
         SampleCount_Bubble:Total_CH4_Flux_Median, 
         Total_Method:CO2_Flux_Median, 
         CO2_flux_method, 
         N2O_Flux_Min:N2O_Flux_Median, 
         k_method:Comments, 
         contains("new_"), 
         contains("orig_"), 
         everything()
         )

names(flux_out) <- gsub("median", "Median", names(flux_out))
names(flux_out) <- gsub("method", "Method", names(flux_out))
names(flux_out) <- gsub("flux", "Flux", names(flux_out))
names(flux_out) <- gsub("Bubble", "Eb", names(flux_out))


flux_out %>% data.frame() %>% head()


setdiff(concentrations$Site_Nid, sites_df$Site_Nid)
setdiff(conc_out$Site_Nid, sites_out$Site_Nid)

setdiff(fluxes$Site_Nid, sites_df$Site_Nid)
setdiff(flux_out$Site_Nid, sites_out$Site_Nid)

conc_out_missing_SiteID <- filter(conc_out, 
                                  Site_Nid %in% setdiff(conc_out$Site_Nid, sites_out$Site_Nid)) 
flux_out_missing_siteID <- filter(flux_out,
                                  Site_Nid %in% setdiff(flux_out$Site_Nid, sites_out$Site_Nid)) 


papers_out %>% data.frame() %>% head()
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
