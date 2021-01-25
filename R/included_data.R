
#' Tables with unit conversions
#' 
#' A table with unit conversions. Converts CH4, CO2, N2O concentrations/fluxes and availabe nutrients.
#' Preferred units are uM and mmol m-2 d-1. 
#'
#' @name unit_convert_table
#' @return dataframe with columns (variable, unit, factor) 
#' @docType data
#' @rdname unit_convert_table
#' @keywords datasets
#' @examples
#' head(unit_convert_table)
NULL

# library(dplyr)
# 
# unit_convert_table <- read.csv(file.path("original_raw_data", "unit_convert_table.csv")) %>%
#   select(variable, unit, factor) %>%
#   filter(variable != "")
# 
# saveRDS(unit_convert_table, file = "R/unit_convert_table.rds")

# # Old code for manually setting units in R
# # CH4 concentration (preferred unit umol/L)
# CH4units <- c("umol/L", 
#               "nmol/L", 
# 
#               "mgC/L", 
#               "mgCH4/L",
#               
#               "ugC/L", 
#               "ugCH4/L", 
#               
#               "ppb CH4", 
#               "ppm CH4", 
#               "uatm CH4"
#               )
# 
# CH4factors <- c(1, #"umol/L"
#                 0.001, # "nmol/L"
#                 
#                 83.259, #"mgC/L"
#                 62.334, #"mgCH4/L"
#                 
#                 0.083259, #"ugC/L" 
#                 0.062334, #"ugCH4/L"
# 
#                 -Inf, #"ppm CH4" 
#                 -Inf, #"ppb CH4"
#                 -Inf  #"uatm CH4" 
#                 )
# 
# CH4_convert_table <- data.frame(variable = "CH4", unit = CH4units, factor = CH4factors)
# 
# 
# # CO2 concentration (preferred unit umol/L)
# CO2units <- c("umol/L", 
#               "nmol/L",
#               
#               "mgC/L", 
#               "mgCO2/L",
#               
#               "ugC/L",  
#               "ugCO2/L",
#               
#               "ppm CO2",
#               "uatm CO2")
# 
# CO2factors <-  c(1, #"umol/L"
#                 0.001, # "nmol/L"
#                 
#                 83.259, #"mgC/L"
#                 22.722, #mgCO2/L"
#                 
#                 0.083259, #"ugC/L" 
#                 0.022722, #"ugCO2/L"
#                 
#                 -Inf,  #"ppm CO2" 
#                 -Inf # "uatm CO2" 
# )
# 
# CO2_convert_table <- data.frame(variable = "CO2", unit = CO2units, factor = CO2factors)
# 
# 
# 
# 
# unique(concentrations$N2Ounits)
# unique(concentrations$NO3units)
# unique(concentrations$NH4units)
# unique(concentrations$TNunits)
# unique(concentrations$SRPunits)
# unique(concentrations$TPunits)
# unique(concentrations$DOCunits)
# 
# 
# # Discharge (preferred unit m3/s)
# Qunits <- c("m3/s", 
#             "L/s",
#             "ft3/s")
# 
# Qfactors <-  c(1, #"m3/s"
#                0.001, #"L/s"
#                0.0283168 #"ft3/s"
# )
# 
# Q_convert_table <- data.frame(variable = "Q", unit = Qunits, factor = Qfactors)
# 
# 
# unit_convert_table <- bind_rows(CH4_convert_table, CO2_convert_table, Q_convert_table)
# 
