## `scripts` Directory

This directory contains all of the scripts used to run experimental optimization protocols, calculate relevant statistics, and produce plots.

### Directory Contents

The contents of this directory are as follows:

-   deliverable_plot_making - directory containing scripts to make plots used in certain deliverables relevant to this project
-   experiments - directory containing scripts to run every experiment ran as a part of this project.
-   other - directory containing other miscellaneous scripts. Most of these scripts are designed to make plots showing how sensitive Hector's temperature and CO2 predictions are to changes in a single parameter. There is also a script in here to calculate OHC RMSE, which was not calculated for many runs because most scripts were written before we really closely considered OHC
-   other_plot_making - directory containing miscellaneous scripts for creating various plots to check that things are working or examine the performance of certain experimental parameterizations
-   `error_functions.R` - R file containing functions for measuring the error between predictions and observations. Used by `major_functions.R` and thus by most scripts within this directory.
-   `major_functions.R` - R file containing functions useful for running different optimization protocols and analyzing their results. This file's functions are used by most scripts within this directory.
-   `scratch.R` - R file used for miscellaneous testing/scratch work.

All output of these files is routed to the results directory (or one of its subdirectories)

### How to Run Experiments

Scripts for running experiments are all found within the experiments directory. Scripts are numbered by the order in which they were ran when we were developing these experiments and trying them for the first time. All experiment scripts use optim to find the optimal parameter set for the given parameters/parameter bounds/error function. The resulting parameterization and its performance on a range of metrics is then output to a file in the results directory. Note that this output is always set to be appended to the output file and will not overwrite any pre-existing data.

Note that to run the "big box"/"large range" version of an experiment, you need triple the size of the parameter box for beta, Q10, and diffusivity (the first 3 parameters), although you should leave the lower bound for beta at 0 instead of a negative number. To do this, you need to modify the `lower` and `upper` parameters in the `run_optim` call within the given script. For example, if this is the initial `run_optim` call:

```         
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY(), ECS(), AERO_SCALE())
...

...
### Calling optim ###
best_pars <- run_optim(obs_data = obs_data,
                       ini_file = INI_FILE,
                       params = PARAMS,
                       lower = c(0.5 - 0.232, 2.2 - 0.44, 2.3 - 0.1, 2, 0),
                       upper = c(0.5 + 0.232, 2.2 + 0.44, 2.3 + 0.1, 5, 3),
                       yrs = 1750:2014,
                       vars = c(GMST(), CONCENTRATIONS_CO2()),
                       error_fn = mean_T_CO2_nmse_unc,
                       include_unc = T,
                       method = "L-BFGS-B",
                       output_file = OUTPUT)
```

Then you'll want to edit this call to be as follows:

```         
PARAMS <- c(BETA(), Q10_RH(), DIFFUSIVITY(), ECS(), AERO_SCALE())
...

...
### Calling optim ###
best_pars <- run_optim(obs_data = obs_data,
                       ini_file = INI_FILE,
                       params = PARAMS,
                       lower = c(0,               2.2 - 0.44 * 3, 2.3 - 0.1 * 3, 2, 0),
                       upper = c(0.5 + 0.232 * 3, 2.2 + 0.44 * 3, 2.3 + 0.1 * 3, 5, 3),
                       yrs = 1750:2014,
                       vars = c(GMST(), CONCENTRATIONS_CO2()),
                       error_fn = mean_T_CO2_nmse_unc,
                       include_unc = T,
                       method = "L-BFGS-B",
                       output_file = OUTPUT)
```
