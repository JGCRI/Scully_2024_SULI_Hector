# Script to create all plots of historical data for AGU Poster
# By: Peter Scully
# Date: 7/22/24

# Importing libraries
library(hector)
library(ggplot2)
theme_set(theme_bw(base_size = 20))

# Setting up file paths
COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")
RESULTS_DIR <- file.path(here::here(), "results", "deliverable_plots")

CO2_PATH <- file.path(COMP_DATA_DIR,
                      "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv")
TEMP_PATH <-
  file.path(COMP_DATA_DIR,
            "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

OHC_PATH <- file.path(COMP_DATA_DIR, "OHC_ensemble_Kuhlbrodt_etal_2022.csv")

INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY(), ECS(), AERO_SCALE())
VARS <- c(GMST(), CONCENTRATIONS_CO2(), HEAT_FLUX())

T_OUTPUT <- file.path(RESULTS_DIR, "AGU_T_comparison.jpeg")
CO2_OUTPUT <- file.path(RESULTS_DIR, "AGU_CO2_comparison.jpeg")
OHC_OUTPUT <- file.path(RESULTS_DIR, "AGU_OHC_comparison.jpeg")


source(file.path(SCRIPTS_DIR, "major_functions.R"))

### Getting observational data ###
co2_data <- get_co2_data(CO2_PATH)
co2_data$lower <- co2_data$value
co2_data$upper <- co2_data$value

temp_data <- get_temp_data(TEMP_PATH, include_unc = T)
temp_data <- filter(temp_data, year <= 2014)

ohc_data <- get_ohc_data(OHC_PATH, include_unc = T)
ohc_data <- filter(ohc_data, year <= 2014)

obs_data <- rbind(co2_data, temp_data, ohc_data)

### Running Hector ###
# Default (and initial smoothing) Results [Exp. 1-4]
default_data <- run_hector(ini_file = INI_FILE, 
                           params = NULL,
                           vals = NULL,
                           yrs = 1750:2014, 
                           vars = VARS)
default_data$scenario <- "Hector - Default"

# NMSE 3-Parameter Results [Exp.5-9]
exp5_9A <- run_hector(ini_file = INI_FILE,
                      params = PARAMS,
                      vals = c(0.268, 2.64, 2.2, 3, 1),
                      yrs = 1750:2014,
                      vars = VARS)
exp5_9A$scenario <- "Hector - NMSE"

exp5B <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0, 1.5, 2.6, 3, 1),
                    yrs = 1750:2014,
                    vars = VARS)
exp5B$scenario <- "Hector - NMSE \nBig Box"

exp6B <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0, 1.58, 2.6, 3, 1),
                    yrs = 1750:2014,
                    vars = VARS)
exp6B$scenario <- "Hector - NMSE, Smoothing (k = 3) \nBig Box"

exp8B <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0, 1.95, 2.6, 3, 1),
                    yrs = 1750:2014,
                    vars = VARS)
exp8B$scenario <- "Hector - NMSE, Smoothing (k = 10) \nBig Box"

exp9B <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.028, 1.76, 2.6, 3, 1),
                    yrs = 1750:2014,
                    vars = VARS)
exp9B$scenario <- "Hector - NMSE w/ unc \nBig Box"


# Optimizing S, Alpha [Exp. 10-11]
exp10A <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.268, 1.95, 2.6, 3.97, 1),
                     yrs = 1750:2014,
                     vars = VARS)
exp10A$scenario <- "Hector - NMSE w/ unc \nTuning S"

exp10B <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.006, 1, 2.6, 3.16, 1),
                     yrs = 1750:2014,
                     vars = VARS)
exp10B$scenario <- "Hector - NMSE w/ unc \nBig Box, Tuning S"

exp11A <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.57, 1.76, 2.38, 2.96, 0.492),
                     yrs = 1750:2014,
                     vars = VARS)
exp11A$scenario <- "Hector - NMSE w/ unc \nTuning S, Alpha"

exp11B <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.502, 0.99, 2, 2.88, 0.5),
                     yrs = 1750:2014,
                     vars = VARS)
exp11B$scenario <- "Hector - NMSE w/ unc \nBig Box, Tuning S, Alpha"

# Optimizing for OHC & Further Refinements [Exp. 12-16]
exp12 <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.65, 1.76, 1.04, 2.33, 0.438),
                    yrs = 1750:2014,
                    vars = VARS)
exp12$scenario <- "Hector - NMSE"

exp13 <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.53, 2.31, 1.04, 2.83, 1.405),
                    yrs = 1750:2014,
                    vars = VARS)
exp13$scenario <- "Hector - MVSSE, incl. OHC \nTuning S, Alpha"

exp14A <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.732, 1.76, 1.04, 3, 0.613),
                     yrs = 1750:2014,
                     vars = VARS)
exp14A$scenario <- "Hector - NMSE w/ unc, incl. OHC \nTuning Alpha"

exp14B <- run_hector(ini_file = INI_FILE,
                     params = PARAMS,
                     vals = c(0.904, 0.88, 0.806, 3, 0.46),
                     yrs = 1750:2014,
                     vars = VARS)
exp14B$scenario <- "Hector - NMSE w/ unc, incl. OHC \nBig Box, Tuning Alpha"

exp15 <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.57, 2.49, 1.06, 3.14, 1.08),
                    yrs = 1750:2014,
                    vars = VARS)
exp15$scenario <- "Hector - MAE" 

exp16 <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.59, 1.76, 1.04, 2.17, 0.411),
                    yrs = 1750:2014,
                    vars = VARS)
exp16$scenario <- "Hector - NMAE"

exp17 <- run_hector(ini_file = INI_FILE,
                    params = PARAMS,
                    vals = c(0.53, 1.86, 1.26, 2.87, 1.33),
                    yrs = 1750:2014,
                    vars = VARS)
exp17$scenario <- "Hector - MSE"


# Coloring all unimportant runs grey
grey_data <- rbind(exp5_9A, exp5B, exp6B, exp8B, exp9B,  # NMSEs
                   exp10A, exp10B,                       # Add S
                   exp11A, exp11B,                               # Add alpha
                   exp13,                                # Try MVSSE
                   exp14A, exp14B)
grey_data$exp <- "Hector - Other Experiments"
grey_data$metric <- "0"

# Coloring important runs
default_data$metric <- "0"
exp12$metric <- "norm"
exp15$metric <- "reg"
exp16$metric <- "norm"
exp17$metric <- "reg"
key_data <- rbind(default_data, exp12, exp17,     # MSE runs
                  exp15, exp16)             # MAE runs
key_data$exp <- key_data$scenario

#Combining all Hector data
hector_data <- rbind(grey_data, key_data)
hector_data$lower <- hector_data$value
hector_data$upper <- hector_data$value

# Filtering data to look nice for graph
hector_data <- filter(hector_data, variable == CONCENTRATIONS_CO2() | 
                        (year >= 1850 & variable == GMST()) |
                        variable == "OHC")

obs_data$exp <- "Historical"
obs_data$metric <- "0"
comb_data <- rbind(obs_data, hector_data)

### Making plots ###

# Temperature plot
temp_data <- filter(comb_data, variable == GMST())

ggplot(data = temp_data, aes(x = year, y = value, color = exp)) + 
  # Plotting uncertainty in Temperature
  geom_ribbon(data = 
                filter(temp_data, scenario == "historical"),
              aes(ymin = lower, ymax = upper),
              fill = 'grey',
              color = NA,
              alpha = 0.5) +
  # Plotting background runs
  geom_line(data = filter(temp_data, exp == "Hector - Other Experiments"),
            aes(group = scenario)) +
  # Plotting foreground runs
  geom_line(data = filter(temp_data, scenario == "historical" & year >= 1850),
            linewidth = 1.25) +
  geom_line(data = filter(temp_data, exp != "Hector - Other Experiments" & 
                            scenario != "historical"),
            linewidth = 1.5,
            aes(linetype = metric)) +
  
  # Cleaning up plot
  scale_color_manual(name = "Experiments",
                     values = c("orange", "skyblue", "blue", "#009E73", "#CC79A7", "snow4", "black"))  + 
  scale_linetype(guide = "none") + 
  theme(legend.text = element_text(size = 15), 
        legend.key.height = unit(2, "cm")) +
  ylab("Temperature Anomaly (\u00B0C)") +
  xlab("Year")
ggsave(T_OUTPUT, width = 12, height = 8)

# CO2 Plot
co2_data <- filter(comb_data, variable == CONCENTRATIONS_CO2())

ggplot(data = co2_data, aes(x = year, y = value, color = exp)) + 
  # Plotting background runs
  geom_line(data = filter(co2_data, exp == "Hector - Other Experiments"),
            aes(group = scenario)) +
  # Plotting foreground runs
  geom_line(data = filter(co2_data, exp != "Hector - Other Experiments" & 
                            (year >= 1850 | scenario != "historical")),
            aes(linetype = metric),
            linewidth = 1) +
  # Plotting 1750 CO2 data point
  geom_point(data = filter(co2_data, scenario == "historical" & year < 1850)) +
  
  # Cleaning up plot
  scale_color_manual(name = expression(bold("Hector Runs")),
                     values = c("orange", "skyblue", "blue", "#009E73", "#CC79A7", "snow4", "black"),
                     labels = c("Default", 
                                expression("MAE (CO"[2]*" RMSE = 1.95)"),
                                expression("MSE (CO"[2]*" RMSE = 1.89)"),
                                expression("NMAE (CO"[2]*" RMSE = 2.06)"),
                                expression("NMSE (CO"[2]*" RMSE = 2.93)"),
                                "Other Runs",
                                "\n\n\nObservations\n\n\n")) + 
  scale_linetype(guide = F) +
  theme(legend.text = element_text(size = 18), 
        legend.key.height = unit(2, "cm")) +
  ylab(expression('CO'[2]*' Concentration (ppmv)')) +
  xlab("Year")
ggsave(CO2_OUTPUT, width = 12, height = 8)

#OHC plot
ohc_data <- filter(comb_data, variable == "OHC" & year <= 2014)

ggplot(data = ohc_data, aes(x = year, y = value, color = exp)) + 
  # Plotting uncertainty in Temperature
  geom_ribbon(data = 
                filter(ohc_data, scenario == "historical"),
              aes(ymin = lower, ymax = upper),
              fill = 'grey',
              color = NA,
              alpha = 0.5) +
  # Plotting background runs
  geom_line(data = filter(ohc_data, exp == "Hector - Other Experiments"),
            aes(group = scenario)) +
  # Plotting foreground runs
  geom_line(data = filter(ohc_data, exp != "Hector - Other Experiments"),
            linewidth = 1.0,
            aes(linetype = metric)) +
  
  # Cleaning up plot
  scale_color_manual(name = "Experiments",
                     values = c("orange", "skyblue", "blue", "#009E73", "#CC79A7", "snow4", "black")) + 
  scale_linetype(guide = F) +
  theme(legend.text = element_text(size = 15), 
        legend.key.height = unit(2, "cm")) +
  ylab(expression('Global Ocean Heat Content Anomaly (ZJ)')) +
  xlab("Year")
ggsave(OHC_OUTPUT, width = 12, height = 8)
