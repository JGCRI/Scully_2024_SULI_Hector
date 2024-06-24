# Script to make plots to confirm that CH4, N2O data from Hector align w/ obs.
# Author: Peter Scully
# Date: 6/19/24

# Constants, imports
library(hector)
library(ggplot2)
library(dplyr)

COMP_DATA_DIR <- file.path(here::here(), "comparison_data")
SCRIPTS_DIR <- file.path(here::here(), "scripts")
RESULTS_DIR <- file.path(here::here(), "results", "other_plots")

GAS_PATH <- file.path(COMP_DATA_DIR,
                      "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv")

INI_FILE <- system.file("input/hector_ssp245.ini", package = "hector")

OUTPUT <- file.path(RESULTS_DIR, "ch4_n2o_plot.jpeg")

source(file.path(SCRIPTS_DIR, "major_functions.R"))


### Reading in observational data ###
gas_data <- read.table(GAS_PATH, 
                       skip = 23, 
                       sep = ",",
                       colClasses = c("numeric", "NULL", "character", "character",
                                        "NULL", "NULL", "NULL", "NULL"))
  
# Fixing table formatting
gas_data <- na.omit(gas_data)
colnames(gas_data) <- c("year", "CH4", "N2O")

ch4_data <- gas_data[c("year", "CH4")]
n2o_data <- gas_data[c("year", "N2O")]

colnames(ch4_data) <- c("year", "value")
colnames(n2o_data) <- c("year", "value")

ch4_data$variable <- CONCENTRATIONS_CH4()
n2o_data$variable <- CONCENTRATIONS_N2O()

ch4_data$units <- "ppb CH4"
n2o_data$units <- "ppb N2O"

obs_data <- rbind(ch4_data, n2o_data)
obs_data$scenario <- "historical"

# Cleaning up numbers with commas in them
obs_data$value <- as.numeric(gsub(",", "", obs_data$value))


### Running Hector ###  
hect_data <- run_hector(INI_FILE, 
                        params = NULL, 
                        vals = NULL, 
                        yrs = c(1750, 1850:2014),
                        vars = c(CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O()))
hect_data$scenario <- "Hector Default"

### Gettting constrained data ###
constr_df <- read.csv(system.file("input/tables/ssp245_emiss-constraints_rf.csv",
                                  package = "hector"),
                      skip = 5)
constr_df <- constr_df[c("Date", "CH4_constrain", "N2O_constrain")]

ch4_constr <- constr_df[c("Date", "CH4_constrain")]
n2o_constr <- constr_df[c("Date", "N2O_constrain")]

colnames(ch4_constr) <- c("year", "value")
colnames(n2o_constr) <- c("year", "value")

ch4_constr$variable <- CONCENTRATIONS_CH4()
n2o_constr$variable <- CONCENTRATIONS_N2O()

ch4_constr$units <- "ppb CH4"
n2o_constr$units <- "ppb N2O"

constr_data <- rbind(ch4_constr, n2o_constr)
constr_data$scenario <- "constrained emissions"
constr_data <- filter(constr_data, year %in% 1750:2014)


### Comparing Results ###
comb_data <- rbind(obs_data, hect_data, constr_data)

ggplot(data = comb_data, aes(x = year, y = value, color = scenario)) + 
  geom_line() +
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Comparing CH4 and N2O Data")
ggsave(OUTPUT, width = 15)