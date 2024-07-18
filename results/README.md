## `results` Directory

This directory contains output files from each experiment (and for the default Hector run). These files contain both the parameter values that were determined to be optimal in the given experiment and the performance of that parameterization on different emergent, historical, and future metrics.

In addition to these files, this directory also contains several subdirectories:

-   deliverable_plots - subdirectory containing plots created for various deliverables related to this project
-   other_plots - subdirectory containing other plots created to compare different experiments' results or do sanity checks
-   pre_bug_fix_results - subdirectory containing all results files and graphs created before a major bug was identified in the `run_hector` function. This bug caused all Hector runs' temperature data to remain unnormalized, leading to inaccurate results. After this bug was identified and fixed, all of the experiments were re-run with the correct version of `run_hector` and the new results were outputted to the main results directory while the old results were moved into the pre_bug_fix_results subdirectory.
