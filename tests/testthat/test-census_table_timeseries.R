library(tibble)
context("census_table_timeseries")

test_that("use", {
  expect_type(census_table_timeseries(),"list")
})
