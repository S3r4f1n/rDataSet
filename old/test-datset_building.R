library(testthat)
test_that("Set creation works", {
  A <- dataset_build(tibble(i = 1:10, b = 1:10), "i")
  expect("dataset" %in% class(A), "dataset should be instance of dataset")
  expect_equal(attr(A, "dataset_ids"), "i")
})
