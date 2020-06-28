# This function tests, by comparing to a saved image of correctly loaded data

# source
library(testthat)

# change dir to load fresh batch of data

# we expect an error when we pick an invalid state number
testthat::expect_error(fars_map_state(100,2013))

#generate tests
testthat::test_that('check filename is created properly', {
  filename_2014 <- make_filename(2014)

  expect_that(filename_2014, is_a('character'))
})
