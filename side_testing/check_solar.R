library(hector)
library(ggplot2)

# Setting up file paths
COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")
RESULTS_DIR <- file.path(here::here(), "results")

CO2_PATH <- file.path(COMP_DATA_DIR,
                      "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv")
TEMP_PATH <-
  file.path(COMP_DATA_DIR,
            "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

OHC_PATH <- file.path(COMP_DATA_DIR, "OHC_ensemble_Kuhlbrodt_etal_2022.csv")


INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")
SOLAR_FILE <- here::here("side_testing", 
                         "hector_inputs", 
                         "hector_ssp245_solar.ini")
YRS <- 1750:2300
VARS <- GMST()


source(file.path(SCRIPTS_DIR, "major_functions.R"))

default_data <- run_hector(INI_FILE, 
                           params = NULL, 
                           vals = NULL, 
                           yrs = YRS, 
                           vars = VARS)
default_data$scenario <- "Default"

solar_data <- run_hector(SOLAR_FILE, 
                         params = NULL, 
                         vals = NULL, 
                         yrs = YRS, 
                         vars = VARS)
solar_data$scenario <- "With solar forcing"

temp_data <- get_temp_data(TEMP_PATH, include_unc = F)

comb_data <- rbind(default_data, solar_data, temp_data)

ggplot(data = comb_data, aes(x = year, y = value, col = scenario)) +
  geom_line()


