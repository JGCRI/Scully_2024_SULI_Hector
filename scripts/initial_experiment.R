# Script to re-implement the calibration of beta, q10, and diff from the V3
# manuscript
# Author: Peter Scully
# Date: 6/13/24

### Constants and Imports ###

# Setting up file paths
COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")
RESULTS_DIR <- file.path(here::here(), "results")

CO2_PATH <- file.path(COMP_DATA_DIR, "co2_annmean_mlo.txt")
TEMP_PATH <-
  file.path(COMP_DATA_DIR,
            "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY())

OUTPUT <- file.path(RESULTS_DIR, "initial_experiment.txt")

# Importing libraries
library(hector)
source(file.path(SCRIPTS_DIR, "major_functions.R"))

### Getting observational data ###
co2_data <- get_co2_data(CO2_PATH)
temp_data <- get_temp_data(TEMP_PATH)
obs_data <- rbind(co2_data, temp_data)

### Calling optim ###
best_pars <- run_optim(obs_data = obs_data,
                       ini_file = INI_FILE,
                       params = PARAMS,
                       yrs = 1850:2024,
                       vars = c(GMST(), CONCENTRATIONS_CO2()),
                       error_fn = mean_T_CO2_mse,
                       output_file = OUTPUT)

### Outputting individual MSEs ###
hector_data <- run_hector(ini_file = INI_FILE, 
                          params = PARAMS, 
                          vals = best_pars, 
                          yrs = 1850:2024, 
                          vars = c(GMST(), CONCENTRATIONS_CO2()))

T_mse <- get_var_mse(obs_data = obs_data, 
                     hector_data = hector_data, 
                     var = GMST(), 
                     start = 1850, 
                     end = 2021)
CO2_mse <- get_var_mse(obs_data = obs_data, 
                       hector_data = hector_data, 
                       var = CONCENTRATIONS_CO2(), 
                       start = 1959, 
                       end = 2021)

write_metric("CO2 MSE:", co2_mse, OUTPUT)
write_metric("T MSE:  ", T_mse, OUTPUT)

### Outputting table metrics ###
calc_table_metrics(PARAMS, best_pars, OUTPUT)