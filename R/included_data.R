
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

