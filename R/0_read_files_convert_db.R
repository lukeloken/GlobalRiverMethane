library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx) #this is needed for converToDate function

#Load custom functions
source("R/convert_unit_functions.R")

#Load unit conversion table
unit_convert_table <- readRDS("R/unit_convert_table.rds")

#load GIS data
 gis_df <- read_csv(file.path(path_to_dropbox, "db_processingR", 
                           "methdb_explore", "data", 
                           "methdb_gis.csv")) %>% 
   rename(Site_Nid =site_nid) 

#load methdb papers table
papers_df <- read_excel(file.path(path_to_dropbox, MethDB_filename),
                    sheet = "Papers",  guess_max = 250)  %>% 
  drop_na(Publication_Nid) %>%
  select_if(~sum(!is.na(.)) > 0)

#rename columns
names(papers_df) <- gsub(" ", "", names(papers_df))


#load methdb site table
sites_df <- read_excel(file.path(path_to_dropbox, MethDB_filename),
                    sheet = "Sites", guess_max = 3500)  %>% 
  mutate(Site_Nid = as.character(Site_Nid)) %>%
  distinct()
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
                             sheet = "Concentrations", guess_max = 14600) %>% 
  mutate(Site_Nid = as.character(Site_Nid)) %>%
  left_join(select(sites_df, Site_Nid, Elevation_m)) %>%
  filter(!is.na(Publication_Nid) & !is.na(Site_Nid))

#rename columns
names(concentrations) <- gsub(" ", "", names(concentrations))
names(concentrations) <- gsub("\\(", "", names(concentrations))
names(concentrations) <- gsub("\\)", "", names(concentrations))
names(concentrations) <- gsub("%", "percent", names(concentrations))
names(concentrations) <- gsub("/", "", names(concentrations))
names(concentrations) <- gsub("Flux\\?", "FluxYesNo", names(concentrations))



#There are two types of dates in this dataset. some are excel format, others are m/d/y. 
concentrations <- concentrations %>%
  mutate(across(c("SampleDatestart", "SampleDateend"), 
                convertToDate, .names = "{col}_v1"))

concentrations$SampleDatestart_v1[which(is.na(concentrations$SampleDatestart_v1))] <- 
  as.Date(concentrations$SampleDatestart[which(is.na(concentrations$SampleDatestart_v1))], format = "%m-%d-%Y")
concentrations$SampleDateend_v1[which(is.na(concentrations$SampleDateend_v1))] <- 
  as.Date(concentrations$SampleDateend[which(is.na(concentrations$SampleDateend_v1))], format = "%m-%d-%Y")

concentrations <- concentrations %>%
  select(-SampleDatestart, -SampleDateend) %>%
  rename(SampleDatestart = SampleDatestart_v1,
         SampleDateend = SampleDateend_v1) 

# data.frame(concentrations[which(is.na(concentrations$SampleDatestart)),])


#load methdb fluxes
fluxes <- read_excel(file.path(path_to_dropbox, MethDB_filename), 
                     sheet = "Fluxes", guess_max = 4000)

#Change names of dates, seasons, and counts
names(fluxes)[grepl("Date", names(fluxes))] <- c("SampleDatestart", "SampleDateend", "SampleDatestart_Bubble", "SampleDateend_Bubble")
names(fluxes)[grepl("Season", names(fluxes))] <- c("Season", "Season_Bubble")
                                                   
names(fluxes)[grepl("Count", names(fluxes))] <- paste0("SampleCount_", c("Diffusive", "Bubble", "Total")) 

names(fluxes) <- gsub("\\..*","",names(fluxes))

#rename columns
names(fluxes) <- gsub(" ", "", names(fluxes))


fluxes <- fluxes %>%
  mutate(across(contains("SampleDate"), ~as.Date(.x))) %>%
  filter(!is.na(Site_Nid))

data.frame(fluxes[which(is.na(fluxes$SampleDateend )),])
data.frame(fluxes[which(is.na(fluxes$Site_Nid)),])


summary(select(fluxes, contains("Date")))



#Convert concentration and fluxes to uM and mmol m-2 d-1

conc_df <- convert_conc_units(concentrations, unit_convert_table)
flux_df <- convert_flux_units(fluxes, unit_convert_table)

save(conc_df, flux_df, 
     sites_df, papers_df, gis_df,
     file = file.path(path_to_dropbox, "db_processingR", 
                      "MethDB_tables_converted.rda"))

# load(file.path(path_to_dropbox, "db_processingR", 
#                "MethDB_tables_converted.rda"))

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

