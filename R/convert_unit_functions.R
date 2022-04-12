# Methane database unit conversion
# Luke Loken - Jan 2021
# Data have been saved as a flat .csv file and loaded into R environment

estimate_pressure <- function(elevation){
  pressure <- (1-(.0000225577*elevation))^5.25588
  return(pressure)
}

# Functions to calculate kh (getkh) 

# Henry's Law constants from http://www.henrys-law.org
# Temperature correction using van't Hoff equation , temperature is in (Kelvin)
# LakeKh is in ("Kh, cp") = (mol/L*Atm) at STP

getKh <- function(temperature, gas){
  Kh <- data.frame("O2" = c(1.3*10^-3, 1700))
  Kh <- cbind(Kh, "H2"  = c(7.8*10^-4, 500))
  Kh <- cbind(Kh, "CO2" = c(3.4*10^-2, 2400 ))
  Kh <- cbind(Kh, "N2"  = c(6.1*10^-4, 1300))
  Kh <- cbind(Kh, "He"  = c(3.7*10^-4, 230))
  Kh <- cbind(Kh, "Ne"  = c(4.5*10^-4, 490))
  Kh <- cbind(Kh, "Ar"  = c(1.4*10^-3, 1300))
  Kh <- cbind(Kh, "CO"  = c(9.5*10^-4, 1300))
  Kh <- cbind(Kh, "O3"  = c(1.2*10^-2, 2300))
  Kh <- cbind(Kh, "N2O" = c(2.5*10^-2, 2600))
  Kh <- cbind(Kh, "SF6" = c(2.4*10^-4, 2400))
  Kh <- cbind(Kh, "CH4" = c(1.4*10^-3, 1700))
  Kh <- cbind(Kh, "C3H8"= c(1.4*10^-3, 2700))
  Kh <- cbind(Kh, "NH3" = c(5.6*10^1 , 4200))
  
  if (!is.character(gas)){stop(paste('gas must be a character. was given as',gas))}
  if (!any(names(Kh)==gas)){stop(paste(gas,'not found in list of coded gases'))}
  
  Khprime <-  unlist(Kh[gas])[1]
  C	<-	unlist(Kh[gas])[2]
  
  LakeKh <- as.numeric(Khprime*exp(C*((1/temperature)-(1/298))))
  
  return(LakeKh)
}

# Concentrations (mole fraction, also ppmv) in the atmosphere are approximate for 2013, should be updated over time
# AtmP is in (atmospheres)

# Function to calculate gas saturation 

getSaturation <- function(LakeKh, AtmP, gas){
  Atmosphere <-        data.frame("O2"  = 209460)
  Atmosphere <- cbind(Atmosphere, "H2"  = 0.55)
  Atmosphere <- cbind(Atmosphere, "N2"  = 780840)
  Atmosphere <- cbind(Atmosphere, "Ar"  = 9340)
  Atmosphere <- cbind(Atmosphere, "CO2" = 400)
  Atmosphere <- cbind(Atmosphere, "He"  = 5.24)
  Atmosphere <- cbind(Atmosphere, "Ne"  = 18.18)
  Atmosphere <- cbind(Atmosphere, "CH4" = 1.83)
  Atmosphere <- cbind(Atmosphere, "O3"  = 0.07)#potential max concentration
  Atmosphere <- cbind(Atmosphere, "N2O" = 0.325)
  Atmosphere <- cbind(Atmosphere, "CO"  = 0.1)
  Atmosphere <- cbind(Atmosphere, "NH3" = NA)
  
  if (!is.character(gas)){stop(paste('gas must be a character. was given as',gas))}
  if (!any(names(Atmosphere)==gas)){stop(paste(gas,'not found in list of coded gases'))}
  
  AtmosphereConc <-  unlist(Atmosphere[gas])[1]
  
  EquilSaturation <- AtmosphereConc*LakeKh/AtmP #umol/L, mmol/m3
  
  return(EquilSaturation)
}


replace_BDLs <- function(x, replace = -999999){
  
 x_nospace <- gsub(" ", "", x)
 x_nospace[which(x_nospace == "")] <- NA
 
 x_numeric <- suppressWarnings(as.numeric(x_nospace))
 
 character_row <- which(!is.finite(x_numeric) & !is.na(x_nospace))
 
 x_numeric[character_row] <- replace
 
return(x_numeric)
 
 #look at data that are being classified as below detection
 # x[character_row]
 # x[-character_row]
 
}

#Test function
# x <- c(13, "", "BDL", " ", "   ", "45", NA, "-1", "  ", 5, "<0.001")
# x_out <- replace_BDLs(x)
# data.frame(x, x_out)


# Master function to convert all gas and nutrient concentrations

convert_conc_units <- function(concentrations, unit_convert_table){
  
  library(dplyr)
  
  goodunits <- unit_convert_table %>%
    filter(factor == 1) %>%
    group_by(variable, factor) %>%
    summarize(unit = unit[1], .groups = "drop")
  
  allunits <- unique(unit_convert_table$unit[!is.na(unit_convert_table$unit)])
  
  dataunits <- concentrations %>%
    select(contains("unit")) 
  
  dataunits_vector <- unique(unlist(sapply(dataunits, unique), use.names = FALSE))
  
  missing_units <- setdiff(unique(dataunits_vector), allunits)
  missing_units <- missing_units[!grepl("ppm", missing_units)]
  missing_units <- missing_units[!grepl("uatm", missing_units)]
  missing_units <- missing_units[!grepl("ppb", missing_units)]
  missing_units <- missing_units[!grepl("%sat", missing_units)]
  missing_units <- missing_units[!is.na(missing_units)]
  
  if( length(missing_units > 0)) {
    stop("One of the following units is missing in unit conversion table: ", 
            paste(missing_units, collapse = ", "))
  }
  

  
  # Calculate best temp and elevation available to calculate elevation-based pressure. 
  # These variables are only used for converting ppm and uatm to molar units
  concentrations_out <- concentrations %>%
    mutate(WaterTempUsed = ifelse(is.finite(as.numeric(WaterTemp_actual)), 
                                  as.numeric(WaterTemp_actual),
                                  ifelse(is.finite(WaterTemp_est), 
                                         as.numeric(WaterTemp_est), 
                                         NA)), 
           ElevationUsed = ifelse(is.finite(as.numeric(Elevation_m)), 
                                  as.numeric(Elevation_m),
                                  ifelse(is.finite(elevation_m_new), 
                                         as.numeric(elevation_m_new), 
                                         NA)),
           Pressure = estimate_pressure(ElevationUsed))
  
  # Convert uatm and ppm to umol/L
  # if ppb value*kh/pressure/1000
  # if ppm value*kh/pressure
  # if uatm value*kh
  
  concentrations_out <- concentrations_out %>%
    mutate(kh = getKh(WaterTempUsed + 273.15, "CH4"), 
           sat_CH4 = getSaturation(kh, AtmP = Pressure, gas = "CH4"),
           factor = case_when(CH4unit == "ppm CH4" ~ kh/Pressure,
                              CH4unit == "ppb CH4" ~ kh/Pressure/1000,
                              CH4unit == "uatm CH4" ~ kh, 
                              CH4unit == "%sat_CH4" ~ sat_CH4/100, #Need to confirm this!!
                              !CH4unit %in% c("ppm CH4", "ppb CH4", "uatm CH4", "%sat_CH4")
                              ~ 1), 
           across(c("CH4min", 
                    "CH4max", 
                    "CH4mean",
                    "CH4_SD",
                    "CH4median"),
                  ~suppressWarnings(as.numeric(.x))*factor)) %>%
    mutate(new_CH4unit = ifelse(CH4unit %in% c("ppm CH4", "ppb CH4", 
                                               "uatm CH4", "%sat_CH4"),
                                "umol/L", 
                                NA)) %>%
    rename(orig_CH4unit = CH4unit) %>%
    select(-factor, -kh, -sat_CH4)
  
  concentrations_out <- concentrations_out %>%
    mutate(kh = getKh(WaterTempUsed + 273.15, "CO2"), 
           sat_CO2 = getSaturation(kh, AtmP = Pressure, gas = "CO2"),
           factor = case_when(CO2units == "ppm CO2" ~ kh/Pressure,
                              CO2units == "ppb CO2" ~ kh/Pressure/1000,
                              CO2units == "uatm CO2" ~ kh, 
                              CO2units == "%sat_CO2" ~ sat_CO2/100, #Need to confirm this!!
                              !CO2units %in% c("ppm CO2", "ppb CO2", 
                                               "uatm CO2", "%sat_CO2") ~ 1),  
           across(c("CO2min", 
                    "CO2max", 
                    "CO2mean",
                    "CO2_SD",
                    "CO2median"),
                  ~suppressWarnings(as.numeric(.x))*factor))  %>%
    mutate(new_CO2unit = ifelse(CO2units %in% c("ppm CO2", "ppb CO2", 
                                                "uatm CO2", "%sat_CO2"),
                                "umol/L", 
                                NA)) %>%
    rename(orig_CO2unit = CO2units) %>%
    select(-factor, -kh, -sat_CO2)
  
  
  concentrations_out <- concentrations_out %>%
    # filter(N2Ounits %in% c("ppm N2O", "ppb N2O", "uatm N2O")) %>%
    mutate(kh = getKh(WaterTempUsed + 273.15, "N2O"), 
           sat_N2O = getSaturation(kh, AtmP = Pressure, gas = "N2O"),
           factor = case_when(N2Ounits == "ppm N2O" ~ kh/Pressure,
                              N2Ounits == "ppb N2O" ~ kh/Pressure/1000,
                              N2Ounits == "uatm N2O" ~ kh, 
                              N2Ounits == "%sat_N2O" ~ sat_N2O/100, #Need to confirm this!!
                              !N2Ounits %in% c("ppm N2O", "ppb N2O", 
                                               "uatm N2O", "%sat_N2O") ~ 1),  
           across(c("N2Omin", 
                    "N2Omax", 
                    "N2Omean",
                    "N2O_SD",
                    "N2Omedian"),
                  ~suppressWarnings(as.numeric(.x))*factor)) %>%
    mutate(new_N2Ounit = ifelse(N2Ounits %in% c("ppm N2O", "ppb N2O", 
                                                "uatm N2O", "%sat_N2O"),
                                "umol/L", 
                                NA)) %>%
    rename(orig_N2Ounit = N2Ounits) %>%
    select(-factor, -kh, #-sat_CO2, #this one is removed in the block above
           -Pressure, -WaterTempUsed, 
           -Elevation_m, -elevation_m_new, -ElevationUsed)
  
  
  #Non-pressure based concentration conversions
  
  #CH4 concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "CH4"), 
              by = c("orig_CH4unit" = "unit")) %>%
    mutate(factor = ifelse(orig_CH4unit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("CH4min", 
                            "CH4max", 
                            "CH4mean",
                            "CH4_SD",
                            "CH4median"), 
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_CH4unit = ifelse(orig_CH4unit %in% allunits, 
                                goodunits$unit[which(goodunits$variable == "CH4")],
                                new_CH4unit)) %>%
    select(-variable, -factor)
  
  #CO2 concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "CO2"), 
              by = c("orig_CO2unit" = "unit")) %>%
    mutate(factor = ifelse(orig_CO2unit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("CO2min", 
                            "CO2max", 
                            "CO2mean",
                            "CO2_SD",
                            "CO2median"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_CO2unit = ifelse(orig_CO2unit %in% allunits, 
                                goodunits$unit[which(goodunits$variable == "CO2")],
                                new_CO2unit)) %>%
    select(-variable, -factor)
  
  #N2O concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "N2O"), 
              by = c("orig_N2Ounit" = "unit")) %>%
    mutate(factor = ifelse(orig_N2Ounit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("N2Omin", 
                            "N2Omax", 
                            "N2Omean",
                            "N2O_SD",
                            "N2Omedian"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_N2Ounit = ifelse(orig_N2Ounit %in% allunits, 
                                goodunits$unit[which(goodunits$variable == "N2O")],
                                new_N2Ounit)) %>%
    select(-variable, -factor)
  
  #NO3 concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "NO3"), 
              by = c("NO3units" = "unit")) %>%
    rename(orig_NO3unit = NO3units) %>%
    mutate(across(.cols = c("NO3actual"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_NO3unit = ifelse(orig_NO3unit %in% allunits, 
                                goodunits$unit[which(goodunits$variable == "NO3")],
                                NA)) %>%
    select(-variable, -factor)
  
  #NH4 concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "NH4"), 
              by = c("NH4units" = "unit")) %>%
    rename(orig_NH4unit = NH4units) %>%
    mutate(across(.cols = c("NH4actual"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_NH4unit = ifelse(orig_NH4unit %in% allunits, 
                                goodunits$unit[which(goodunits$variable == "NH4")],
                                NA)) %>%
    select(-variable, -factor)
  
  #TN concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "TN"), 
              by = c("TNunits" = "unit")) %>%
    rename(orig_TNunit = TNunits) %>%
    mutate(across(.cols = c("TNactual"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_TNunit = ifelse(orig_TNunit %in% allunits, 
                               goodunits$unit[which(goodunits$variable == "TN")],
                               NA)) %>%
    select(-variable, -factor)
  
  #TP concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "TP"), 
              by = c("TPunits" = "unit")) %>%
    rename(orig_TPunit = TPunits) %>%
    mutate(across(.cols = c("TPactual"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_TPunit = ifelse(orig_TPunit %in% allunits, 
                               goodunits$unit[which(goodunits$variable == "TP")],
                               NA)) %>%
    select(-variable, -factor)
  
  #SRP concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "SRP"), 
              by = c("SRPunits" = "unit")) %>%
    rename(orig_SRPunit = SRPunits) %>%
    mutate(across(.cols = c("SRPactual"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_SRPunit = ifelse(orig_SRPunit %in% allunits, 
                                goodunits$unit[which(goodunits$variable == "SRP")],
                                NA)) %>%
    select(-variable, -factor)
  
  #DOC concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "DOC"), 
              by = c("DOCunits" = "unit")) %>%
    rename(orig_DOCunit = DOCunits) %>%
    mutate(across(.cols = c("DOCactual"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_DOCunit = ifelse(orig_DOCunit %in% allunits, 
                                goodunits$unit[which(goodunits$variable == "DOC")],
                                NA)) %>%
    select(-variable, -factor)
  
  #Q concentration
  concentrations_out <- concentrations_out %>%
    left_join(filter(unit_convert_table, variable == "Q"), 
              by = c("Qunits" = "unit")) %>%
    rename(orig_Qunit = Qunits) %>%
    mutate(across(.cols = c("Q"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_Qunit = ifelse(orig_Qunit %in% allunits, 
                              goodunits$unit[which(goodunits$variable == "Q")],
                              NA)) %>%
    select(-variable, -factor)
  
  # ###########################################################
  # replace all columns with characters with -999999
  # These are the "BDLs", "<0.06", etc. in the data columns
  # ###########################################################

  data_names <- c("CH4min", "CH4max", "CH4mean", "CH4_SD", "CH4median", 
                  "CO2min", "CO2max", "CO2mean", "CO2_SD", "CO2median", 
                  "N2Omin", "N2Omax", "N2Omean", "N2O_SD", "N2Omedian",
                  "NO3actual", "NH4actual", "TNactual", 
                  "SRPactual", "TPactual", "DOCactual",
                  "Q")
  
  #replace all characters in these columns with "-999999"
  replace_value <- -999999
  conc_BDL <- concentrations %>% 
    mutate(across(all_of(data_names), ~replace_BDLs(.x, replace = replace_value)))
  
  #Look at a few examples to test behavior
  # data.frame(concentrations[which(concentrations$CH4mean == "<0.06"),])
  # data.frame(conc_BDL[which(concentrations$CH4mean == "<0.06"),])
  # 
  # data.frame(new = concentrations$CH4mean[which(concentrations$CH4mean == "BDL")], 
  #            old = conc_BDL$CH4mean[which(concentrations$CH4mean == "BDL")])
  # 
  # data.frame(new = concentrations$CH4mean[which(conc_BDL$CH4mean == replace_value)],
  #            old = conc_BDL$CH4mean[which(conc_BDL$CH4mean == replace_value)])
  # 
  # 
  # data.frame(conc_BDL[which(conc_BDL$CH4mean == -999999),])
  
  
  #Loop through data columns and replace 
  name_i = data_names[3]
for (name_i in data_names){
  concentrations_out[which(conc_BDL[,name_i] == "-999999"),name_i] <- -999999
}
  
  return(concentrations_out)
  
}



# Master function to convert all fluxes

convert_flux_units <- function(fluxes, unit_convert_table){
  
  goodunits = unit_convert_table %>%
    filter(factor == 1) %>%
    group_by(variable, factor) %>%
    summarize(unit = unit[1], .groups = "drop")
  
  allunits <- unique(unit_convert_table$unit[!is.na(unit_convert_table$unit)])
  
  dataunits <- fluxes %>%
    select(contains("unit")) 
  
  dataunits_vector <- unique(unlist(sapply(dataunits, unique), use.names = FALSE))
  
  missing_units <- setdiff(unique(dataunits_vector), allunits)

  
  if( length(missing_units[which(!is.na(missing_units))] > 0)) {
    stop("One of the following units is missing in unit conversion table: ", 
         paste(missing_units, collapse = ", "))
  }
  
  
  #CH4 diffusive flux
  fluxes_out <- fluxes %>%
    left_join(filter(unit_convert_table, variable == "CH4_flux"), 
              by = c("Diffusive_Flux_unit" = "unit")) %>%
    mutate(factor = ifelse(Diffusive_Flux_unit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("Diffusive_CH4_Flux_Min", 
                            "Diffusive_CH4_Flux_Max",
                            "Diffusive_CH4_Flux_Mean", 
                            "Diffusive_CH4_Flux_SD",
                            "Diffusive_CH4_Flux_Median"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_Diffusive_Flux_unit = ifelse(Diffusive_Flux_unit %in% allunits, 
                                             goodunits$unit[which(goodunits$variable == "CH4_flux")],
                                             NA)) %>%
    select(-variable, -factor)
  
  #CH4 bubble flux
  fluxes_out <- fluxes_out %>%
    left_join(filter(unit_convert_table, variable == "CH4_flux"), 
              by = c("Eb_CH4_Flux_unit" = "unit")) %>%
    mutate(factor = ifelse(Eb_CH4_Flux_unit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("Eb_CH4_Flux_Min", 
                            "Eb_CH4_Flux_Max",
                            "Eb_CH4_Flux_Mean", 
                            "Eb_CH4_Flux_SD",
                            "Eb_CH4_Flux_median"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_Eb_CH4_Flux_unit = ifelse(Eb_CH4_Flux_unit %in% allunits, 
                                          goodunits$unit[which(goodunits$variable == "CH4_flux")],
                                          NA)) %>%
    select(-variable, -factor)
  
  #CH4 total flux
  fluxes_out <- fluxes_out %>%
    left_join(filter(unit_convert_table, variable == "CH4_flux"), 
              by = c("Total_Flux_unit" = "unit")) %>%
    mutate(factor = ifelse(Total_Flux_unit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("Total_CH4_Flux_Min", 
                            "Total_CH4_Flux_Max",
                            "Total_CH4_Flux_Mean", 
                            "Total_CH4_Flux_SD",
                            "Total_CH4_Flux_Median"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_Total_Flux_unit = ifelse(Total_Flux_unit %in% allunits, 
                                         goodunits$unit[which(goodunits$variable == "CH4_flux")],
                                         NA)) %>%
    select(-variable, -factor)
  
  #CO2 flux
  fluxes_out <- fluxes_out %>%
    left_join(filter(unit_convert_table, variable == "CO2_flux"), 
              by = c("CO2_Flux_unit" = "unit")) %>%
    mutate(factor = ifelse(CO2_Flux_unit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("CO2_Flux_Min", 
                            "CO2_Flux_Max",
                            "CO2_Flux_Mean", 
                            "CO2_Flux_SD",
                            "CO2_Flux_Median"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_CO2_Flux_unit = ifelse(CO2_Flux_unit %in% allunits, 
                                    goodunits$unit[which(goodunits$variable == "CO2_flux")],
                                    NA)) %>%
    select(-variable, -factor)
  
  #N2O diffusive flux
  fluxes_out <- fluxes_out %>%
    left_join(filter(unit_convert_table, variable == "N2O_flux"), 
              by = c("N2O_Flux_unit" = "unit")) %>%
    mutate(factor = ifelse(N2O_Flux_unit %in% unit_convert_table$unit, 
                           factor, 
                           1))  %>% 
    mutate(across(.cols = c("N2O_Flux_Min", 
                            "N2O_Flux_Max",
                            "N2O_Flux_Mean", 
                            "N2O_Flux_SD",
                            "N2O_Flux_Median"),
                  ~suppressWarnings(as.numeric(.x))*factor), 
           new_N2O_Flux_unit = ifelse(N2O_Flux_unit %in% allunits, 
                                    goodunits$unit[which(goodunits$variable == "N2O_flux")],
                                    NA)) %>%
    select(-variable, -factor)
  
  
  
  # ###########################################################
  # replace all data columns with characters with -999999
  # These are the "BDLs", "<0.06", etc. in the data columns
  # ###########################################################
  
  flux_data_names <- c("Diffusive_CH4_Flux_Min", "Diffusive_CH4_Flux_Max",
                       "Diffusive_CH4_Flux_Mean", "Diffusive_CH4_Flux_SD",
                       "Diffusive_CH4_Flux_Median", 
                       "Eb_CH4_Flux_Min", "Eb_CH4_Flux_Max",
                       "Eb_CH4_Flux_Mean", "Eb_CH4_Flux_SD",
                       "Eb_CH4_Flux_median",
                       "Total_CH4_Flux_Min", "Total_CH4_Flux_Max",
                       "Total_CH4_Flux_Mean", "Total_CH4_Flux_SD",
                       "Total_CH4_Flux_Median",
                       "CO2_Flux_Min", "CO2_Flux_Max",
                       "CO2_Flux_Mean", "CO2_Flux_SD",
                       "CO2_Flux_Median",
                       "N2O_Flux_Min", "N2O_Flux_Max",
                       "N2O_Flux_Mean", "N2O_Flux_SD",
                       "N2O_Flux_Median")
  
  #replace all characters in these columns with "-999999"
  replace_value <- -999999
  flux_BDL <- fluxes %>% 
    mutate(across(all_of(flux_data_names), ~replace_BDLs(.x, replace = replace_value)))
  
  #Look at a few examples to test behavior
  # data.frame(new = fluxes$Diffusive_CH4_Flux_Mean[which(flux_BDL$Diffusive_CH4_Flux_Mean == replace_value)],
  #            old = flux_BDL$Diffusive_CH4_Flux_Mean[which(flux_BDL$Diffusive_CH4_Flux_Mean == replace_value)])
  # 
  # 
  # data.frame(conc_BDL[which(conc_BDL$CH4mean == -999999),])
  
  
  #Loop through data columns and replace 
  name_i = flux_data_names[1]
  for (name_i in flux_data_names){
    fluxes_out[which(flux_BDL[,name_i] == "-999999"),name_i] <- -999999
  }
  
  return(fluxes_out)
}

