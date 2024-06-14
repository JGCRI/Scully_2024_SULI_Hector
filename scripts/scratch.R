# Script for testing functions
# Author: Peter Scully
# Date: 6/11/24

# Setting constants
COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")

CO2_PATH <- file.path(COMP_DATA_DIR,
 "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv")
TEMP_PATH <-
  file.path(COMP_DATA_DIR,
            "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

MAJ_FUNC_PATH <- file.path(SCRIPTS_DIR, "major_functions.R")

# Importing functions
source(MAJ_FUNC_PATH)

# # Testing get_var_mse
data1 <- get_co2_data(CO2_PATH)
data1
# 
# ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
# data2 <- run_hector(ini_file, 
#                     params = NULL, 
#                     vals = NULL, 
#                     yrs = 1959:2023, 
#                     vars = c(CONCENTRATIONS_CO2()))
# 
# get_var_mse(data1, data2, CONCENTRATIONS_CO2(), 1959, 2023)
# 
# 
# # Testing objective_fn
# 
# # Creating an error function
# get_co2_mse <- function(obs_data, hector_data) {
#   return(get_var_mse(obs_data = obs_data, 
#                      hector_data = hector_data, 
#                      var = CONCENTRATIONS_CO2(),
#                      start = 1959,
#                      end = 2023))
# }
# 
# # This call should match get_var_mse
# objective_fn(obs_data = data1, 
#              ini_file = system.file("input/hector_ssp245.ini", package = "hector"),
#              params = NULL, 
#              par = NULL, 
#              yrs = 1959:2023, 
#              vars = c(CONCENTRATIONS_CO2()), 
#              error_fn = get_co2_mse)

# Testing calc_table_metrics
calc_table_metrics(params = NULL,
                   vals = NULL,
                   output_file = here::here("results", "original_params.txt"))

