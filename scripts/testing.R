# Script for testing functions
# Author: Peter Scully
# Date: 6/11/24

# Setting constants
COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")

CO2_PATH <- file.path(COMP_DATA_DIR, "co2_annmean_mlo.txt")
TEMP_PATH <-
  file.path(COMP_DATA_DIR,
            "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

MAJ_FUNC_PATH <- file.path(SCRIPTS_DIR, "major_functions.R")

# Importing functions
source(MAJ_FUNC_PATH)

# Testing functions
data1 <- get_co2_data(CO2_PATH)

ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)
data2 <- fetchvars(core, 2000:2005, vars = c(CONCENTRATIONS_CO2(), GLOBAL_TAS()))

data3 <- get_temp_data(TEMP_PATH)

rbind(data1, data2, data3)
