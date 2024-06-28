# Script to compare runs that use different hector parameterizations
# This script only makes a CO2 plot
# However, since it's based on a script to plot both CO2 and T, it 
# (inefficiently) also records T data
# Author: Peter Scully
# Date: 6/13/24

### Constants and Imports ###

# Importing libraries
library(hector)
library(ggplot2)
theme_set(theme_bw(base_size = 20))

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
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY(), ECS(), AERO_SCALE())

OUTPUT <- file.path(RESULTS_DIR, "all_run_CO2_comparison.jpeg")


source(file.path(SCRIPTS_DIR, "major_functions.R"))

### Getting observational data ###
co2_data <- get_co2_data(CO2_PATH)
co2_data$lower <- co2_data$value
co2_data$upper <- co2_data$value

temp_data <- get_temp_data(TEMP_PATH, include_unc = T)
temp_data <- filter(temp_data, year <= 2014)

obs_data <- rbind(co2_data, temp_data)

### Running Hector ###
# Default (and initial smoothing) Results [Exp. 1-4]
default_data <- run_hector(ini_file = INI_FILE, 
                           params = NULL,
                           vals = NULL,
                           yrs = 1750:2014, 
                           vars = c(GMST(), CONCENTRATIONS_CO2()))
default_data$scenario <- "Hector - Default"

# NMSE 3-Parameter Results [Exp.5-9]
exp5_9A <- run_hector(ini_file = INI_FILE,
                        params = PARAMS,
                        vals = c(0.268, 2.64, 2.2, 3, 1),
                        yrs = 1750:2014,
                        vars = c(GMST(), CONCENTRATIONS_CO2()))
exp5_9A$scenario <- "Hector - NMSE"

exp5B <- run_hector(ini_file = INI_FILE,
                           params = PARAMS,
                           vals = c(0, 1.5, 2.6, 3, 1),
                           yrs = 1750:2014,
                           vars = c(GMST(), CONCENTRATIONS_CO2()))
exp5B$scenario <- "Hector - NMSE \nBig Box"

exp6B <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0, 1.58, 2.6, 3, 1),
                    yrs = 1750:2014,
                    vars = c(GMST(), CONCENTRATIONS_CO2()))
exp6B$scenario <- "Hector - NMSE, Smoothing (k = 3) \nBig Box"

exp8B <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0, 1.95, 2.6, 3, 1),
                    yrs = 1750:2014,
                    vars = c(GMST(), CONCENTRATIONS_CO2()))
exp8B$scenario <- "Hector - NMSE, Smoothing (k = 10) \nBig Box"

exp9B <- run_hector(ini_file = INI_FILE,
                        params = PARAMS,
                        vals = c(0.028, 1.76, 2.6, 3, 1),
                        yrs = 1750:2014,
                        vars = c(GMST(), CONCENTRATIONS_CO2()))
exp9B$scenario <- "Hector - NMSE w/ unc \nBig Box"


# Optimizing S, Alpha [Exp. 10-11]
exp10A <- run_hector(ini_file = INI_FILE,
                                  params = PARAMS,
                                  vals = c(0.268, 1.95, 2.6, 3.97, 1),
                                  yrs = 1750:2014,
                                  vars = c(GMST(), CONCENTRATIONS_CO2()))
exp10A$scenario <- "Hector - NMSE w/ unc \nTuning S"

exp10B <- run_hector(ini_file = INI_FILE,
                       params = PARAMS,
                       vals = c(0.006, 1, 2.6, 3.16, 1),
                       yrs = 1750:2014,
                       vars = c(GMST(), CONCENTRATIONS_CO2()))
exp10B$scenario <- "Hector - NMSE w/ unc \nBig Box, Tuning S"

exp11A <- run_hector(ini_file = INI_FILE,
                       params = PARAMS,
                       vals = c(0.57, 1.76, 2.38, 2.96, 0.492),
                       yrs = 1750:2014,
                       vars = c(GMST(), CONCENTRATIONS_CO2()))
exp11A$scenario <- "Hector - NMSE w/ unc \nTuning S, Alpha"

exp11B <- run_hector(ini_file = INI_FILE,
                            params = PARAMS,
                            vals = c(0.502, 0.99, 2, 2.88, 0.5),
                            yrs = 1750:2014,
                            vars = c(GMST(), CONCENTRATIONS_CO2()))
exp11B$scenario <- "Hector - NMSE w/ unc \nBig Box, Tuning S, Alpha"

# Optimizing for OHC & Further Refinements [Exp. 12-16]
exp12 <- run_hector(ini_file = INI_FILE,
                         params = PARAMS,
                         vals = c(0.65, 1.76, 1.04, 2.33, 0.438),
                         yrs = 1750:2014,
                         vars = c(GMST(), CONCENTRATIONS_CO2()))
exp12$scenario <- "Hector - NMSE w/ unc, incl. OHC \nTuning S, Alpha"

exp13 <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.53, 2.31, 1.04, 2.83, 1.405),
                    yrs = 1750:2014,
                    vars = c(GMST(), CONCENTRATIONS_CO2()))
exp13$scenario <- "Hector - MVSSE, incl. OHC \nTuning S, Alpha"

exp14A <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.732, 1.76, 1.04, 3, 0.613),
                    yrs = 1750:2014,
                    vars = c(GMST(), CONCENTRATIONS_CO2()))
exp14A$scenario <- "Hector - NMSE w/ unc, incl. OHC \nTuning Alpha"

exp14B <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.904, 0.88, 0.806, 3, 0.46),
                     yrs = 1750:2014,
                     vars = c(GMST(), CONCENTRATIONS_CO2()))
exp14B$scenario <- "Hector - NMSE w/ unc, incl. OHC \nBig Box, Tuning Alpha"

exp15 <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.57, 2.49, 1.06, 3.14, 1.08),
                     yrs = 1750:2014,
                     vars = c(GMST(), CONCENTRATIONS_CO2()))
exp15$scenario <- "Hector - MAE w/ unc, incl. OHC \nTuning S, Alpha"

exp16 <- run_hector(ini_file = INI_FILE,
                             params = PARAMS,
                             vals = c(0.59, 1.76, 1.04, 2.17, 0.411),
                             yrs = 1750:2014,
                             vars = c(GMST(), CONCENTRATIONS_CO2()))
exp16$scenario <- "Hector - NMAE w/ unc, incl. OHC \nTuning S, Alpha"

# Coloring all unimportant runs grey
grey_data <- rbind(exp5_9A, exp5B, exp6B, exp8B, exp9B,  # NMSEs
                     exp10A, exp10B,                       # Add S
                     exp11B,                               # Add alpha
                     exp12,                                # Add OHC, Mat Diff
                     exp13,                                # Try MVSSE
                     exp14A, exp14B,                       # Try remove S
                     exp15)                                # Try MAE
grey_data$exp <- "Hector - Other Experiments"

# Coloring important runs
key_data <- rbind(default_data, exp11A, exp16)             # Best Runs
key_data$exp <- key_data$scenario

#Combining all Hector data
hector_data <- rbind(grey_data, key_data)
hector_data$lower <- hector_data$value
hector_data$upper <- hector_data$value

# Filtering data to look nice for graph
hector_data <- filter(hector_data, variable == CONCENTRATIONS_CO2() | 
                        (year >= 1850 & variable == GMST()))

obs_data$exp <- "Historical"
comb_data <- rbind(obs_data, hector_data)

#Filtering for CO2 data
comb_data <- filter(comb_data, variable == CONCENTRATIONS_CO2())

ggplot(data = comb_data, aes(x = year, y = value, color = exp)) + 
  # Plotting uncertainty in Temperature
  geom_ribbon(data = 
               filter(comb_data, scenario == "historical" & variable == GMST()),
              aes(ymin = lower, ymax = upper),
              fill = 'hotpink2',
              color = NA,
              alpha = 0.5) +
  # Plotting background runs
  geom_line(data = filter(comb_data, exp == "Hector - Other Experiments" |
                            (scenario == "historical" & year >= 1850)),
            aes(group = scenario)) +
  # Plotting foreground runs
  geom_line(data = filter(comb_data, exp != "Hector - Other Experiments" & 
                            scenario != "historical")) +
  # Plotting 1750 CO2 data point
  geom_point(data = filter(comb_data, scenario == "historical" & year < 1850)) +
  
  # Cleaning up plot
  scale_color_manual(name = "Experiments",
                     values = c("blue",  "#009E73","#D55E00", "grey", "#CC79A7")) + 
  theme(legend.text = element_text(size = 15), 
        legend.key.height = unit(2, "cm")) +
  ylab(expression('CO'[2]*' Concentration (ppmv)')) +
  xlab("Year")
ggsave(OUTPUT, width = 12)