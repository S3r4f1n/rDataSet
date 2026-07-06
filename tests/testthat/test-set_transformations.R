library(testthat)
library(dplyr)

test_that("to_wide preserves wide datasets", {
  df <- tibble::tibble(id = 1:2, height = c(10, 20), weight = c(100, 200))
  ds <- dataset_build(df, "id")
  expect_true(dataset_equality(to_wide(ds), ds))
})

test_that("to_wide converts long to wide", {
  df_wide <- tibble::tibble(id = c("a", "b"), x = 1:2, y = 3:4)
  ds_wide <- dataset_build(df_wide, "id")
  ds_long <- wide_to_long(ds_wide)
  expect_equal(state(ds_long), "long")

  back <- to_wide(ds_long, x_axis = "variable")
  expect_equal(state(back), "wide")
  expect_true(dataset_equality(back, ds_wide))
})

test_that("to_wide converts decomposed to wide", {
  df_wide <- tibble::tibble(id = 1:2, x = 1:2, y = 3:4)
  ds_wide <- dataset_build(df_wide, "id")
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)

  back <- to_wide(ds_decomp)
  expect_equal(state(back), "wide")
  expect_true(dataset_equality(back, ds_wide))
})

test_that("to_long preserves long datasets", {
  df_wide <- tibble::tibble(id = 1:2, a = 10:11)
  ds_wide <- dataset_build(df_wide, "id")
  ds_long <- wide_to_long(ds_wide)
  expect_true(dataset_equality(to_long(ds_long), ds_long))
})

test_that("to_long converts wide to long", {
  df_wide <- tibble::tibble(id = 1:2, a = 10:11, b = 20:21)
  ds_wide <- dataset_build(df_wide, "id")
  ds_long <- to_long(ds_wide)
  expect_equal(state(ds_long), "long")
})

test_that("to_long converts decomposed to long", {
  df_wide <- tibble::tibble(id = 1:2, a = 1:2)
  ds_wide <- dataset_build(df_wide, "id")
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)

  ds_long <- to_long(ds_decomp)
  expect_equal(state(ds_long), "long")
})

test_that("to_decomposed preserves decomposed datasets", {
  df_wide <- tibble::tibble(id = 1:2, a = 1:2)
  ds_wide <- dataset_build(df_wide, "id")
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  expect_equal(
    to_decomposed(ds_decomp, strategy = hirarchical_paths),
    ds_decomp
  )
})

test_that("to_decomposed converts wide to decomposed", {
  df_wide <- tibble::tibble(id = 1:2, a = 1:2)
  ds_wide <- dataset_build(df_wide, "id")
  ds_decomp <- to_decomposed(ds_wide, strategy = hirarchical_paths)
  expect_equal(
    ds_decomp,
    dataset_decompose(ds_wide, strategy = hirarchical_paths)
  )
})

test_that("to_decomposed converts long to decomposed", {
  df_wide <- tibble::tibble(id = 1:2, a = 1:2, b = 3:4)
  ds_wide <- dataset_build(df_wide, "id")
  ds_long <- wide_to_long(ds_wide)
  ds_decomp <- to_decomposed(ds_long, strategy = hirarchical_paths)
  expected <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  expect_equal(ds_decomp, expected)
})

test_that("roundtrip wide -> long -> wide restores original", {
  df <- tibble::tibble(id = 1:3, x = c(0.1, 0.2, 0.3), y = c(0.4, 0.5, 0.6))
  ds <- dataset_build(df, "id")
  expect_true(dataset_equality(to_wide(to_long(ds), x_axis = "variable"), ds))
})

test_that("roundtrip wide -> decomposed -> wide restores original", {
  df <- tibble::tibble(id = c("a", "b"), v1 = 5:6, v2 = 7:8)
  ds <- dataset_build(df, "id")
  decomp <- to_decomposed(ds, strategy = hirarchical_paths)
  back <- to_wide(decomp)
  expect_true(dataset_equality(back, ds))
})

test_that("roundtrip long -> wide -> long restores original", {
  df_wide <- tibble::tibble(id = 1:2, a = 1:2, b = 3:4)
  ds_wide <- dataset_build(df_wide, "id")
  ds_long <- wide_to_long(ds_wide)
  back <- to_long(to_wide(ds_long, x_axis = "variable"))
  expect_true(dataset_equality(back, ds_long))
})

test_that("roundtrip decomposed -> wide -> decomposed restores original", {
  df_wide <- tibble::tibble(id = 1:2, a = 1:2, b = 3:4)
  ds_wide <- dataset_build(df_wide, "id")
  ds_decomp <- dataset_decompose(ds_wide, strategy = hirarchical_paths)
  back <- to_decomposed(to_wide(ds_decomp), strategy = hirarchical_paths)
  expect_equal(back, ds_decomp)
})

test_that("dataset_transfrom rejects unsupported target", {
  df <- tibble::tibble(id = 1, a = 2)
  ds <- dataset_build(df, "id")
  expect_error(dataset_transfrom(ds, "unknown"), "Unsupported transformation")
})

test_that("empty wide dataset is preserved by to_wide and to_long", {
  df <- tibble::tibble(id = integer(0), a = numeric(0))
  ds <- dataset_build(df, "id")
  expect_true(dataset_equality(to_wide(ds), ds))
  long_empty <- to_long(ds)
  expect_equal(nrow(long_empty), 0)
  expect_equal(state(long_empty), "long")
})
