library(testthat)
library(dplyr)

test_that("set arithmetic functions work", {
  df1 <- tibble::tibble(id = 1:2, val = list(TRUE, FALSE))
  ds1 <- dataset_build(df1, "id")
  
  df2 <- tibble::tibble(id = 2:3, val = list(TRUE, TRUE))
  ds2 <- dataset_build(df2, "id")
  
  # Test Equality
  expect_true(dataset_equality(ds1, ds1))
  expect_false(dataset_equality(ds1, ds2))
  
  # Test Union
  u <- dataset_union(ds1, ds2)
  expect_equal(nrow(u), 3)
  
  # Test Intersect
  i <- dataset_intersect(ds1, ds2)
  expect_equal(nrow(i), 1)
  
  # Test Minus
  m <- dataset_minus(ds1, ds2)
  expect_equal(nrow(m), 1)
})

test_that("dataset_frame and dataset_diff work", {
  df <- tibble::tibble(id = 1, val = list(TRUE))
  ds <- dataset_build(df, "id")
  
  # Test Frame
  f <- dataset_frame(ds)
  expect_true(f$val[[1]])
  
  # Test Diff
  d <- dataset_diff(ds, ds)
  expect_equal(nrow(d), 0)
})
