# File to test the functions in scripts/error_functions.R

# loading functions
source(here::here("scripts", "error_functions.R"))

library(assertthat)

# Confirming that mse works as expected
mse_tests <- function() {
  assert_that(mse(0, 0) == 0)
  assert_that(mse(c(0, 1, 2), c(1, 2, 1)) == 1)
  assert_that(mse(c(0, 1, 2), c(0, 1, -1)) == 3)
  assert_that(mse(c(-2, 1, 2), c(0, 3, 0)) == 4)
}

mse_tests()