library(testthat)

test_that("fars_read function returns data frame", {
  expect_type(fars_read("/Users/Sam/Documents/R/Course 2 Package Dev/data/accident_2015.csv.bz2"), "list")
})

test_that("make_filename function returns character string", {
  expect_type(make_filename(2021), "character")
})
