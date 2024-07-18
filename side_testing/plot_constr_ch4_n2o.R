# Script to make plots to confirm that CH4, N2O constrained runs produce similar
# results to default Hector run
# Author: Peter Scully
# Date: 6/19/24

# Constants, imports
library(hector)
library(ggplot2)
library(dplyr)

COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")
RESULTS_DIR <- file.path(here::here(), "results", "other_plots")

CO2_PATH <- file.path(COMP_DATA_DIR,
                      "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv")
TEMP_PATH <-
  file.path(COMP_DATA_DIR,
            "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")

INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")
PLOT_VARS <- c(CONCENTRATIONS_CO2(), GMST())

OUTPUT <- file.path(RESULTS_DIR, "ch4_n2o_constr_comparison.jpeg")

source(file.path(SCRIPTS_DIR, "major_functions.R"))


### Getting observational data ###
co2_data <- get_co2_data(CO2_PATH)

temp_data <- get_temp_data(TEMP_PATH)
temp_data <- filter(temp_data, year <= 2014)

obs_data <- rbind(co2_data, temp_data)


### Running Hector ###  
default_data <- run_hector(INI_FILE, 
                        params = NULL, 
                        vals = NULL, 
                        yrs = c(1750:2014),
                        vars = PLOT_VARS)
default_data$scenario <- "Hector Default"

### Getting constrained data ###
constr_df <- read.csv(system.file("input/tables/ssp245_emiss-constraints_rf.csv",
                                  package = "hector"),
                      skip = 5)
constr_df <- constr_df[c("Date", "CH4_constrain", "N2O_constrain")]

ch4_constr <- constr_df$CH4_constrain
n2o_constr <- constr_df$N2O_constrain


### Running constrained runs ###

# Note that all runs need temperatures normalized to 1961-1990 to match hist data

# CH4 constrained run
core <- newcore(INI_FILE)
setvar(core,
       dates = 1745:2300,
       var = CH4_CONSTRAIN(),
       values = ch4_constr,
       unit = getunits(CH4_CONSTRAIN()))
reset(core)

run(core)
ch4_constr_data <- fetchvars(core, 1750:2014, vars = PLOT_VARS)
ch4_constr_data <- rel_to_interval(data = ch4_constr_data, 
                                   var = GMST(), 
                                   start = 1961, 
                                   end = 1990)
ch4_constr_data$scenario <- "[CH4] Constraints Only"
shutdown(core)

# N2O constrained run
core <- newcore(INI_FILE)
setvar(core,
       dates = 1745:2300,
       var = N2O_CONSTRAIN(),
       values = n2o_constr,
       unit = getunits(N2O_CONSTRAIN()))
reset(core)

run(core)
n2o_constr_data <- fetchvars(core, 1750:2014, vars = PLOT_VARS)
n2o_constr_data <- rel_to_interval(data = n2o_constr_data, 
                                   var = GMST(), 
                                   start = 1961, 
                                   end = 1990)
n2o_constr_data$scenario <- "[N2O] Constraints Only"
shutdown(core)

# Double constrained run
core <- newcore(INI_FILE)
setvar(core,
       dates = 1745:2300,
       var = CH4_CONSTRAIN(),
       values = ch4_constr,
       unit = getunits(CH4_CONSTRAIN()))
setvar(core,
       dates = 1745:2300,
       var = N2O_CONSTRAIN(),
       values = n2o_constr,
       unit = getunits(N2O_CONSTRAIN()))
reset(core)

run(core)
both_constr_data <- fetchvars(core, 1750:2014, vars = PLOT_VARS)
both_constr_data <- rel_to_interval(data = both_constr_data, 
                                   var = GMST(), 
                                   start = 1961, 
                                   end = 1990)
both_constr_data$scenario <- "Both Constraints"
shutdown(core)

### Comparing Results ###
comb_data <- rbind(obs_data, 
                   default_data, 
                   ch4_constr_data, 
                   n2o_constr_data, 
                   both_constr_data)

ggplot(data = comb_data, aes(x = year, y = value, color = scenario)) + 
  geom_line() +
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Comparing CH4 and N2O Constrained Runs")
ggsave(OUTPUT, width = 15)