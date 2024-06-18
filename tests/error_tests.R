# File to test the functions in scripts/error_functions.R

# loading functions
source(here::here("scripts", "error_functions.R"))

library(assertthat)
library(dplyr)

# Confirming that mse works as expected
mse_tests <- function() {
  assert_that(mse(0, 0) == 0)
  assert_that(mse(c(0, 1, 2), c(1, 2, 1)) == 1)
  assert_that(mse(c(0, 1, 2), c(0, 1, -1)) == 3)
  assert_that(mse(c(-2, 1, 2), c(0, 3, 0)) == 4)
}

# Trying to use get_var_mse
get_var_mse_tests <- function() {
  # First, we need to create data frames to pass to get_var_mse:
  obs_data <- data.frame(year = 2000:2015, 
                         variable = "GMST",
                         value = 1:16)
  hect_data <- data.frame(year = 2000:2015, 
                         variable = "GMST",
                         value = seq(0, 30, 2))
  
  # Adding in another variable to our data frames
  more_data <- data.frame(year = 2000:2015,
                          variable = "CO2_concentrations",
                          value = 400:415)
  
  obs_data <- rbind(obs_data, more_data)
  hect_data <- rbind(hect_data, more_data)
  
  # Confirming get_var_mse works as intended
  assert_that(get_var_mse(obs_data, 
                          hect_data, 
                          var = "CO2_concentrations", 
                          yrs = 2000:2015) == 0)
  
  assert_that(get_var_mse(obs_data, 
                          hect_data, 
                          var = "CO2_concentrations", 
                          yrs = 2000) == 0)
  
  assert_that(get_var_mse(obs_data, 
                          hect_data, 
                          var = "GMST", 
                          yrs = 2000) == 1)
  
  assert_that(get_var_mse(obs_data, 
                          hect_data, 
                          var = "GMST", 
                          yrs = 2000:2001) == 0.5)
  
  assert_that(get_var_mse(obs_data, 
                          hect_data, 
                          var = "GMST", 
                          yrs = 2000:2004) == 3)
  
  
}
mse_tests()
get_var_mse_tests()