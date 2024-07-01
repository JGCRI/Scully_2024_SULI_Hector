# Script to compare future temperatures for different Hector parameterizations
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

INI_PREFIX <- system.file("input/hector_ssp", package = "hector") #
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY(), ECS(), AERO_SCALE())

OUTPUT <- file.path(RESULTS_DIR, "future_T_comparison.jpeg")


source(file.path(SCRIPTS_DIR, "major_functions.R"))


# Running Hector (modified from calc_table_metrics in major_functions.R)

# Setting up variables
future_yrs <- 1850:2100
future_vars <- GMST()

# Getting the names of each scenario file
scenarios <- c("119", "245", "585")
scenario_names <- paste("input/hector_ssp", scenarios, ".ini", sep = "")
scenario_files <- system.file(scenario_names, package = "hector")

# Setting up results data frame
future_results <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(future_results) <- c("scenario", "variable", "value", "units", "run")

# Doing default run
for (scen_counter in 1:length(scenario_files)) {
  # Adding default data
  default_data <- run_hector(ini_file = scenario_files[scen_counter],
                          params = NULL,
                          vals = NULL,
                          yrs = future_yrs,
                          vars = future_vars)
  default_data$scenario <- paste("ssp", scenarios[scen_counter], sep="")
  default_data$run <- "Default Parameterization"
  
  # Adding best run data
  new_data <- run_hector(ini_file = scenario_files[scen_counter],
                          params = PARAMS,
                          vals = c(0.59, 1.76, 1.04, 2.17, 0.411),
                          yrs = future_yrs,
                          vars = future_vars)
  new_data$scenario <- paste("ssp", scenarios[scen_counter], sep="")
  new_data$run <- "New Parameterization"
  
  # Adding both data frames to results data frame
  future_results <- rbind(future_results, new_data, default_data)
}

# Plot results
ggplot(data = future_results, aes(x = year, y = value, color = run)) + 
  geom_line() +
  facet_wrap(~ scenario) +
  
  # Cleaning up plot
  scale_color_manual(name = "",
                     values = c("blue",  "#009E73")) + 
  theme(legend.text = element_text(size = 15), 
        legend.key.height = unit(2, "cm")) +
  ylab("Temperature Anomaly (\u00B0C)") +
  xlab("Year")
ggsave(OUTPUT, width = 20)