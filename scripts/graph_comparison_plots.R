# Script to compare plots that use different hector variables
# Author: Peter Scully
# Date: 6/13/24

### Constants and Imports ###

# Importing libraries
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

INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY())

OUTPUT <- file.path(RESULTS_DIR, "nmse_box_comparison_plots.jpeg")


source(file.path(SCRIPTS_DIR, "major_functions.R"))

### Getting observational data ###
co2_data <- get_co2_data(CO2_PATH)
co2_data$lower <- co2_data$value
co2_data$upper <- co2_data$value

temp_data <- get_temp_data(TEMP_PATH, include_unc = T)
temp_data <- filter(temp_data, year <= 2014)

obs_data <- rbind(co2_data, temp_data)

### Running Hector ###
default_data <- run_hector(ini_file = INI_FILE, 
                           params = NULL,
                           vals = NULL,
                           yrs = 1750:2014, 
                           vars = c(GMST(), CONCENTRATIONS_CO2()))
default_data$scenario <- "Hector - Default Fit"

nmse_data <- run_hector(ini_file = INI_FILE,
                        params = PARAMS,
                        vals = c(0.732, 2.64, 2.4),
                        yrs = 1750:2014,
                        vars = c(GMST(), CONCENTRATIONS_CO2()))
nmse_data$scenario <- "Hector - Fit to NMSEs"

nmse_bb_data <- run_hector(ini_file = INI_FILE,
                        params = PARAMS,
                        vals = c(1.196, 3.52, 2),
                        yrs = 1750:2014,
                        vars = c(GMST(), CONCENTRATIONS_CO2()))

nmse_bb_data$scenario <- "Hector - Fit to NMSEs, Big Box"

nmse_bb_smooth_data <- run_hector(ini_file = INI_FILE,
                        params = PARAMS,
                        vals = c(1.147, 3.52, 2),
                        yrs = 1750:2014,
                        vars = c(GMST(), CONCENTRATIONS_CO2()))
nmse_bb_smooth_data$scenario <- "Hector - NMSEs, Big Box, Smoothed"

hector_data <- rbind(default_data, nmse_data, nmse_bb_data, nmse_bb_smooth_data)
hector_data$lower <- hector_data$value
hector_data$upper <- hector_data$value

comb_data <- rbind(obs_data, hector_data)

ggplot(data = comb_data, aes(x = year, y = value, color = scenario)) + 
  geom_ribbon(data = 
               filter(comb_data, scenario == "historical" & variable == GMST()),
              aes(ymin = lower, ymax = upper),
              fill = 'orchid1',
              color = NA) +
  geom_line() +
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Comparing Parameterizations")
ggsave(OUTPUT, width = 15)