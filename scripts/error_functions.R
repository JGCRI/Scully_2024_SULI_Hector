# Title: Error functions
# Author: Peter Scully
# Date: 6/11/24


########################
### Helper Functions ###
########################


# mse - function to find mean squared error between two vectors
#
# args: 
#   x - observed values
#   y - predicted values
#
# returns: numeric vector length 1 containing the MSE between predicted and
#          observed values
mse <- function(x, y) {
  SE <- (x - y)^2
  return(mean(SE))
}

# rmse - function to find root mean squared error between two vectors
#
# args: 
#   x - observed values
#   y - predicted values
#
# returns: numeric vector length 1 containing the RMSE between predicted and
#          observed values
rmse <- function(x, y) {
  return(sqrt(mse(x, y)))
}


# get_var_mse: function to find MSE between observed and predicted data for
#              a given variable
#
# args: 
#   obs_data    - data frame of observed data formatted like Hector data frame
#   hector_data - data frame outputted by Hector
#   var         - variable name
#   start       - start year for finding MSE
#   end         - end year for finding MSE
#
# Returns: MSE between predicted and observed data for var
get_var_mse <- function(obs_data, hector_data, var, start, end) {
  obs_vec <- 
    filter(obs_data, year >= start & year <= end & variable == var)$value
  
  hector_vec <-
    filter(hector_data, year >= start & year <= end & variable == var)$value
  
  return(mse(obs_vec, hector_vec))
}

# mean_T_CO2_mse: function to find the mean of the temperature and CO2 MSEs 
#                 between observed and predicted data for a given variable
#
# args: 
#   obs_data    - data frame of observed data formatted like Hector data frame
#   hector_data - data frame outputted by Hector
#
# Returns: MSE between predicted and observed data for var
mean_T_CO2_mse <- function(obs_data, hector_data) {
  T_mse <- get_var_mse(obs_data = obs_data, 
                       hector_data = hector_data, 
                       var = GMST(), 
                       start = 1850, 
                       end = 2014)
  CO2_mse <- get_var_mse(obs_data = obs_data, 
                         hector_data = hector_data, 
                         var = CONCENTRATIONS_CO2(), 
                         start = 1850, 
                         end = 2014)
  
  return(mean(c(T_mse, CO2_mse)))
}