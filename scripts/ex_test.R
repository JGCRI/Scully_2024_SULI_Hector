
# load the functions
source(here::here("scripts", "ex_fxns.R"))

library(assertthat)


# Let's test to see if the right things are returne
assert_that(is.string(hello_world()))
assert_that(is.numeric(my_sum(1, 2)))

# Checking to see if the expected value is returned
assert_that(my_sum(1, 2) == 9)


