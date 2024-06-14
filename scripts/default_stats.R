# Script to output stats for default Hector parameterization
# Author: Peter Scully
# Date: 6/14/24

### Constants and Imports ###

# Importing libraries
library(hector)

# Setting up file paths
COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")
RESULTS_DIR <- file.path(here::here(), "results")

CO2_PATH <- file.path(COMP_DATA_DIR,
                      "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv")
TEMP_PATH <-
  file.path(COMP_DATA_DIR,
            "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY())

OUTPUT <- file.path(RESULTS_DIR, "default_stats.txt")


source(file.path(SCRIPTS_DIR, "major_functions.R"))


### Getting observational data ###
co2_data <- get_co2_data(CO2_PATH)
temp_data <- get_temp_data(TEMP_PATH)
obs_data <- rbind(co2_data, temp_data)


### Outputting default values ###
default_core <- newcore(INI_FILE)
param_table <- fetchvars(default_core, dates = NA, vars = PARAMS)
shutdown(default_core)

write.table(param_table,
            file = OUTPUT,
            append = TRUE,
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)
write("", OUTPUT, append = TRUE)


### Running Hector and getting errors ###
hector_data <- run_hector(ini_file = INI_FILE, 
                          params = NULL, 
                          vals = best_pars, 
                          yrs = 1750:2014, 
                          vars = c(GMST(), CONCENTRATIONS_CO2()))


write_metric("Mean of T, CO2 MSEs:", 
             mean_T_CO2_mse(obs_data, hector_data), 
             OUTPUT)
write("", OUTPUT, append = TRUE)

# Outputting individual MSEs
T_mse <- get_var_mse(obs_data = obs_data, 
                     hector_data = hector_data, 
                     var = GMST(), 
                     yrs = 1850:2014)
CO2_mse <- get_var_mse(obs_data = obs_data, 
                       hector_data = hector_data, 
                       var = CONCENTRATIONS_CO2(), 
                       yrs = c(1750, 1850:2014))

write_metric("CO2 MSE:", CO2_mse, OUTPUT)
write_metric("T MSE:  ", T_mse, OUTPUT)
write_metric("RMSE:   ", sqrt(mean(CO2_mse, T_mse)), OUTPUT) # not 100% sure this is how we want to calculate this
write("", OUTPUT, append = TRUE)

### Outputting table metrics ###
calc_table_metrics(NULL, NULL, OUTPUT)