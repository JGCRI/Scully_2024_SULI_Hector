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

| Experiment \#  | Added Params | Temp. smoothed?   | Error Function | Datasets Optimized for |
|---------------|---------------|---------------|---------------|---------------|
| 1 (Default)    | None         | No                | MSE            | CO2, T                 |
| 2-4            | None         | Yes, k = 3, 5, 10 | MSE            | CO2, T                 |
| 5              | None         | No                | NMSE           | CO2, T                 |
| 6-8            | None         | Yes, k = 3, 5, 10 | NMSE           | CO2, T                 |
| 9              | None         | No                | NMSE w/ unc    | CO2, T                 |
| 10             | S            | No                | NMSE w/ unc    | CO2, T                 |
| 11             | S, alpha     | No                | NMSE w/ unc    | CO2, T                 |
| 12             | S, alpha     | No                | NMSE w/ unc    | CO2, T, OHC            |
| 13             | S, alpha     | No                | MVSSE          | CO2, T, OHC            |
| 14             | alpha        | No                | NMSE w/ unc    | CO2, T, OHC            |
| 15             | S, alpha     | No                | MAE w/ unc     | CO2, T, OHC            |
| 16             | S, alpha     | No                | NMAE w/ unc    | CO2, T, OHC            |
| CO2 only (17A) | S, alpha     | No                | MSE            | CO2                    |
| T only (17B)   | S, alpha     | No                | MSE w/ unc     | T                      |
| OHC only (17C) | S, alpha     | No                | MSE w/ unc     | OHC                    |

Abbreviations Key:

-   OHC = Ocean Heat Content
-   MSE = Mean Squared Error
-   NMSE = Normalized Mean Squared Error
-   w/ unc = accounting for uncertainty (see manuscript for more details)
-   MVSSE = Mean Variance-Standardized Squared Error
-   MAE = Mean Absolute Error
-   NMAE = Normalized Mean Absolute Error

Experiments with tripled parameter ranges were run for experiments 5-11 and 14. For how to run these experiments, see the scripts directory README. For how to change k for the smoothing experiments, see the scripts for those specific experiments. Note that experiment 7 was never run because, after running experiments 6 and 8, it seemed unnecessary and highly unlikely to produce any useful results.

## How to Create Figures

The scripts for creating all figures used in the deliverables related to this project can be found in the deliverable_plot_making subdirectory of the scripts directory. All of these figures are output to the deliverable_plots subdirectory of the results directory. Note that, for the AGU plots, I included legends in the code to make the temperature and OHC plots but cropped them out on the actual poster.
