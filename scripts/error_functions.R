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