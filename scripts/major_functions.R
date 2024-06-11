# Title: Major functions
# Author: Peter Scully
# Date: 6/11/24


### Imports and Constants ###
library(hector)
library(dplyr)

################################
### Comparison Data Fetchers ###
################################

# get_co2_data - function to get the CO2 data from the appropriate file
#
# args:
#   file - path to historical CO2 data file
#   scenario - name of scenario being run (default: "historical")
#   include_unc - boolean indicating whether to include upper/lower bounds on
#                 values (default: FALSE)
#
# returns: Hector-style data frame with CO2 data
get_co2_data <- function(file, scenario = "historical", include_unc = F) {
  co2_data <- read.table(file, skip = 43, col.names = c("year", "value", "unc"))

  # Adding in new columns to match Hector data frames
  co2_data$scenario <- scenario
  co2_data$variable <- CONCENTRATIONS_CO2()
  co2_data$units <- " ppmv CO2"

  # Adding in upper and lower bounds, if desired
  if (include_unc) {
    co2_data$upper <- co2_data$value + co2_data$unc
    co2_data$lower <- co2_data$value - co2_data$unc
  }

  # Removing uncertainty column
  co2_data$unc <- NULL

  return(co2_data)
}

# get_temp_data - function to get the temperature data from the appropriate file
#
# args:
#   file - path to historical temperature data file
#   scenario - name of scenario being run (default: "historical")
#   include_unc - boolean indicating whether to include upper/lower bounds on
#                 values (default: FALSE)
#
# returns: Hector-style data frame with temperature data
get_temp_data <- function(file, scenario = "historical", include_unc = F) {
  temp_data <- read.csv(file)
  colnames(temp_data) <- c("year", "value", "lower", "upper")

  # Adding in new columns to match Hector data frames
  temp_data$scenario <- scenario
  temp_data$variable <- GLOBAL_TAS()
  temp_data$units <- "degC"

  # Removing upper and lower bounds, if desired
  if (!include_unc) {
    temp_data$lower <- NULL
    temp_data$upper <- NULL
  }

  return(temp_data)
}


# Everything below should be copied from the table metrics script
# TODO: Once finished with that script, copy it in and rework it
