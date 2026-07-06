library(testthat)
library(dplyr)

context("dataset_transfrom and helper transformation functions")

# Utility to build a simple wide dataset
build_wide <- function(id_col = "id", ...) {
  df <- tibble::tibble(..., .rows = if (is.null(..1)) 0 else length(..1))
  dataset_build(df, id_col)
}

test_that("to_wide preserves wide datasets", {
  ds <- build_wide("id", id = 1:2, height = c(10, 20), weight = c(100, 200))
  expect_true(dataset_equality(to_wide(ds), ds))
})

test_that("to_wide converts long to wide", {
  ds_wide <- build_wide("id", id = c("a", "b"), x = 1:2, y = 3:4)
  ds_long <- wide_to_long(ds_wide)
  expect_equal(state(ds_long), "long")
  
  back <- to_wide(ds_long)
  expect_equal(state(back), "wide")
  expect_true(dataset_equality(back, ds_wide))
})

test_that("to_wide converts decomposed to wide", {
  ds_wide <- build_wide("id", id = 1:2, x = 1:2, y = 3:4)
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  
  back <- to_wide(ds_decomp)
  expect_equal(state(back), "wide")
  expect_true(dataset_equality(back, ds_wide))
})

test_that("to_long preserves long datasets", {
  ds_wide <- build_wide("id", id = 1:2, a = 10:11)
  ds_long <- wide_to_long(ds_wide)
  expect_true(dataset_equality(to_long(ds_long), ds_long))
})

test_that("to_long converts wide to long", {
  ds_wide <- build_wide("id", id = 1:2, a = 10:11, b = 20:21)
  ds_long <- to_long(ds_wide)
  expect_equal(state(ds_long), "long")
  expect_equal(
    ds_long %>%
      as_tibble() %>%
      arrange(across(everything())),
    wide_to_long(ds_wide) %>%
      as_tibble() %>%
      arrange(across(everything()))
  )
})

test_that("to_long converts decomposed to long", {
  ds_wide <- build_wide("id", id = 1:2, a = 1:2)
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  
  ds_long <- to_long(ds_decomp)
  expect_equal(state(ds_long), "long")
  expect_equal(
    ds_long %>%
      as_tibble() %>%
      arrange(across(everything())),
    wide_to_long(ds_wide) %>%
      as_tibble() %>%
      arrange(across(everything()))
  )
})

test_that("to_decomposed preserves decomposed datasets", {
  ds_wide <- build_wide("id", id = 1:2, a = 1:2)
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  expect_equal(to_decomposed(ds_decomp, strategy = hirarchical_paths), ds_decomp)
})

test_that("to_decomposed converts wide to decomposed", {
  ds_wide <- build_wide("id", id = 1:2, a = 1:2)
  ds_decomp <- to_decomposed(ds_wide, strategy = hirarchical_paths)
  expect_equal(ds_decomp, dataset_decompose(ds_wide, strategy = hirarchical_paths))
})

test_that("to_decomposed converts long to decomposed", {
  ds_wide <- build_wide("id", id = 1:2, a = 1:2, b = 3:4)
  ds_long <- wide_to_long(ds_wide)
  ds_decomp <- to_decomposed(ds_long, strategy = hirarchical_paths)
  expected <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  expect_equal(ds_decomp, expected)
})

test_that("roundtrip wide -> long -> wide restores original", {
  ds <- build_wide("id", id = 1:3, x = runif(3), y = runif(3))
  expect_true(dataset_equality(to_wide(to_long(ds)), ds))
})

test_that("roundtrip wide -> decomposed -> wide restores original", {
  ds <- build_wide("id", id = letters[1:2], v1 = 5:6, v2 = 7:8)
  decomp <- to_decomposed(ds, strategy = hirarchical_paths)
  back <- to_wide(decomp)
  expect_true(dataset_equality(back, ds))
})

test_that("roundtrip long -> wide -> long restores original", {
  ds_wide <- build_wide("id", id = 1:2, a = 1:2, b = 3:4)
  ds_long <- wide_to_long(ds_wide)
  back <- to_long(to_wide(ds_long))
  expect_true(dataset_equality(back, ds_long))
})

test_that("roundtrip decomposed -> wide -> decomposed restores original", {
  ds_wide <- build_wide("id", id = 1:2, a = 1:2, b = 3:4)
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  back <- to_decomposed(to_wide(ds_decomp), strategy = hirarchical_paths)
  expect_equal(back, ds_decomp)
})

test_that("dataset_transfrom rejects unsupported target", {
  ds <- build_wide("id", id = 1, a = 2)
  expect_error(dataset_transfrom(ds, "unknown"), "Unsupported transformation")
})

test_that("empty wide dataset is preserved by to_wide and to_long", {
  empty <- build_wide("id", id = integer(0))
  expect_true(dataset_equality(to_wide(empty), empty))
  # empty should produce empty long, but we only check no error
  long_empty <- to_long(empty)
  expect_equal(nrow(long_empty), 0)
  expect_equal(state(long_empty), "long")
})

test_that("empty wide to decomposed works", {
  empty <- build_wide("id", id = integer(0))
  decomp <- to_decomposed(empty, strategy = hirarchical_paths)
  # empty dataset generates empty list? expect_silent
  expect_true(is.list(decomp))
})
