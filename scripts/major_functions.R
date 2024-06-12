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
  temp_data$variable <- GMST()
  temp_data$units <- "degC"

  # Removing upper and lower bounds, if desired
  if (!include_unc) {
    temp_data$lower <- NULL
    temp_data$upper <- NULL
  }

  return(temp_data)
}


############################
### Data Frame Functions ###
############################


# get_var_change - function to find the change in a variable between a start
#                  and end date
#
# args:
#   data  - data frame outputted by fetchvars
#   var   - string containing the Hector variable to find the change in
#   start - start year
#   end   - end year
#
# returns: single value indicating the change in variable from start year to
#          end year
get_var_change <- function(data, var, start, end) {
  initial_val <- filter(data, variable == var & year == start)$value
  final_val   <- filter(data, variable == var & year == end)$value
  return(final_val - initial_val)
}


# get_interval_avg - function to find the average value of a Hector variable
#                    over a given time interval
#
# args:
#   data  - data frame outputted by run_hector (or fetchvars)
#   var   - string containing the Hector variable to average
#   start - start year for finding average
#   end   - end year for finding average
#
# returns: average value of var across the provided interval
get_interval_avg <- function(data, var, start, end) {
  data %>%
    subset(variable == var) %>%
    subset(start <= year & year <= end) %>%
    .$value -> interval_vals
  return(mean(interval_vals))
}


# rel_to_val - function to adjust values of a Hector variable to be relative
#              to a new value
#
# args:
#   data      - data frame outputted by fetchvars
#   var       - string containing the Hector variable to adjust
#   benchmark - value to make variable relative to
#
# returns: data frame containing the new values for var relative to benchmark
rel_to_val <- function(data, var, benchmark) {
  # Updating all of the values for var that are in data
  data %>%
    mutate(value = ifelse(variable == var, value - benchmark, value)) -> data
  return(data)
}


# rel_to_interval - function to normalize values of a Hector variable to a
#                   reference period
#
# args:
#   data  - data frame outputted by fetchvars
#   var   - string containing the Hector variable to adjust
#   start - start year for reference period
#   end   - end year for reference period
#
# returns: data frame containing the normalized values for that variable
rel_to_interval <- function(data, var, start, end) {
  benchmark <- get_interval_avg(data, var, start, end)
  return(rel_to_val(data, var, benchmark))
}


# run_hector - function to run Hector using a given ini file and set of params
#
# args:
#   ini_file    - path to the ini file to use to run Hector
#   params      - vector of Hector parameters to modify
#   vals        - vector of values to use for those Hector parameters
#   yrs         - year range to get Hector data from
#   vars        - Hector variables to get data on
#   include_unc - boolean indicating whether to include upper/lower bounds on
#                 values (default: FALSE)
#
# returns: data frame containing the variables' values for the given date range
#
# note: assumes params and vals are the same length and that each paraneter is
#       stored at the same index as its corresponding value
run_hector <- function(ini_file, params, vals, yrs, vars, include_unc = F) {
  core <- newcore(ini_file)
  
  # Setting parameter values
  for (i in 1:length(params)) {
    setvar(core = core, 
           dates = NA, 
           var = params[i], 
           values = vals[i], 
           unit = getunits(params[i]))
  }
  
  # Running core and fetching data
  run(core)
  data <- fetchvars(core, yrs, vars = vars)
  shutdown(core)
  
  # Rescaling temperatures (if applicable)
  if (GMST() %in% params) {
    data <- rel_to_interval(data = data, var = GMST(), start = 1961, end = 1990)
  }
  
  # Adding in upper and lower bounds (if applicable)
  if (include_unc) {
    data$upper <- data$value
    data$lower <- data$value
  }
  
  return(data)
}


# write_metric - writes the value for a metric to the output file
#
# args:
#   name - name of metric
#   val  - value of metric
#
# returns: nothing
#
# Note: The value of the metric is rounded to 3 sig figs when outputted
write_metric <- function(name, val, file) {
  write(paste(name, signif(val, 3)), file = file, append = TRUE)
}


# objective_fn - flexible objective function that can be used to calculate the
#                error between observed and predicted data for a Hector run with
#                the given values for a set of parameters
#
# args:
#   obs_data    - Hector-style data frame with observed data
#   ini_file    - path to the ini file to use to run Hector
#   params      - vector of Hector parameters to modify
#   pars        - vector of values to use for those Hector parameters
#   yrs         - year range to get Hector data from
#   vars        - Hector variables to get data on
#   error_fn    - function to calculate error between observed/predicted vals
#   include_unc - boolean indicating whether upper/lower bounds needed for 
#                 error_fn (default: FALSE)
#
# returns: error between observed and predicted data calculated via error_fn
#
# note: uses an error function from error_functions.R
# note: assumes error_fn takes obs_data and hector_data as inputs and outputs a
#       real-valued error
objective_fn <- function(obs_data, ini_file, params, par, yrs, vars, error_fn, 
                         include_unc = F) {
  # Running Hector
  hector_data <- run_hector(ini_file = ini_file,
                            params = params,
                            vals = par,
                            yrs = yrs,
                            vars = vars,
                            include_unc = include_unc)
  
  # Returning error from model run
  return(error_fn(obs_data, hector_data))
}

# run_optim - function to run optim with an objective function based on a given
#             set of Hector parameters and an error function. Also outputs 
#             results of optim call
#
# args:
#   obj_fn      - objective function to use
#   obs_data    - Hector-style data frame with observed data
#   ini_file    - path to the ini file to use to run Hector
#   params      - vector of Hector parameters to modify
#   yrs         - year range to get Hector data from
#   vars        - Hector variables to get data on
#   error_fn    - function to calculate error between observed/predicted vals
#   include_unc - boolean indicating whether upper/lower bounds needed for 
#                 error_fn (default: FALSE)
#   output_file - path to file to append table to
#
# returns: Nothing, but outputs optimal parameters and objective function
#          value to given output file
#
# note: uses an error function from error_functions.R
run_optim <- function(obj_fn, obs_data, ini_file, params, yrs, vars, error_fn, 
                      include_unc = F, output_file) {
  # Creating vector of default parameters
  default_core <- newcore(ini_file)
  defaults <- fetchvars(default_core, dates = NA, vars = params)$value
  shutdown(default_core)
  
  # Applying optim
  optim_output <- optim(par = defaults, 
                        fn = obj_fn, 
                        obs_data = obs_data, 
                        ini_file = ini_file, 
                        params = params,
                        yrs = yrs,
                        vars = vars, 
                        error_fn = error_fn, 
                        include_unc = include_unc)
  
  # Extract vals from optim output
  best_pars <- optim_output$par
  min_error <- optim_output$value
  
  # Output optimal parameterization
  write.table(data.frame(parameters = best_pars, values = min_error),
              file = output_file,
              append = TRUE,
              quote = FALSE,
              sep = "\t",
              row.names = FALSE)
  
  # Output value of objective function
  write_metric("Objective Function Value:", min_error, output_file)
  
}

# Everything below should be copied from the table metrics script
# TODO: Once finished with that script, copy it in and rework it
# TODO: Delete old version of run_hector when table making script is copied in
