# Script to make smoothed temperature comparison plot
# Author: Peter Scully
# Date: 6/17/24

# Importing libraries
library(hector)
library(zoo)
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

INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")

# Setting other constants
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY())
BEST_PARS <- c(0.528263386504877, 1.76, 2.4) # from 3 year results

OUTPUT <- file.path(RESULTS_DIR, "graph_smooth_temps.jpeg")



source(file.path(SCRIPTS_DIR, "major_functions.R"))

### Getting observational data ###
temp_data <- get_temp_data(TEMP_PATH)

obs_data <- temp_data

# Getting smoothed temperatures
for (k in c(3, 10)) {
  smoothed_temps <- rollmean(temp_data$value, 
                             k = k, 
                             align = "center",
                             fill = NA)
  smoothed_temp_data <- temp_data
  smoothed_temp_data$value <- smoothed_temps
  smoothed_temp_data$scenario <- paste("Smoothed, k =", k)
  obs_data <- rbind(obs_data, smoothed_temp_data)
}

### Getting Hector data ###
hector_data <- run_hector(ini_file = INI_FILE, 
                          params = PARAMS, 
                          vals = BEST_PARS, 
                          yrs = 1850:2014, 
                          vars = GMST())
hector_data$scenario <- "Fit to Smoothed (and unsmoothed) Data"

### Plotting data ###
comb_data <- rbind(obs_data, hector_data)
ggplot(data = comb_data) + 
  geom_line(aes(x = year, y = value, color = scenario))
ggsave(OUTPUT, width = 9)