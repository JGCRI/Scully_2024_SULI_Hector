# hector-calibration-summer-2024

Code and data related to historical calibration experiments by Hector's 2024 SULI

## Directories

-   .github - directory containing workflow for CI tests of error functions
-   comparison_data - directory containing observational datasets used to compare Hector against
-   idealized_inputs - directory containing ini files to run Hector for idealized scenarios (used to calculate transient climate response)
-   results - directory containing the results of every optimization attempt, including optimal parameters, performance on different error metrics, historical metrics, and future metrics. Also contains several plots comparing different parameterizations' temperature/CO2/ocean heat content time series with historical record
-   scripts - directory containing all scripts used to run experiments and produce plots
-   side_testing - directory containing files pertaining to checks to ensure other aspects of Hector are working properly
-   tests - directory containing file with all the CI tests run to ensure error functions working properly

## How to Run Experiments

Each experiment can be run using the corresponding script in the experiments subdirectory of the scripts directory. A table of each experiment is given below:

[TODO: Make table]

## How to Create Figures

The scripts for creating all figures used in the deliverables related to this project can be found in the deliverable_plot_making subdirectory of the scripts directory. All of these figures are output to the deliverable_plots subdirectory of the results directory.
