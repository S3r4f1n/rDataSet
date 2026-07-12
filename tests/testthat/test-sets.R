library(testthat)

test_that("dataset_build with valid inputs", {
  df <- data.frame(
    id1 = c(1, 2),
    id2 = c("a", "b"),
    variable = c("x", "y"),
    val = c(10, 20),
    stringsAsFactors = FALSE
  )
  ids <- c("id1", "id2")
  ds <- dataset_build(df, ids)

  expect_s3_class(ds, "dataset")
  expect_equal(attr(ds, "dataset_ids"), c("id1", "id2", "variable"))
  expect_equal(attr(ds, "dataset_x_axis"), "variable")
  expect_equal(attr(ds, "dataset_state"), "wide")
})

test_that("dataset_build errors", {
  expect_error(
    dataset_build(data.frame(a = 1:2), 5),
    "ids must be vector of character"
  )
  expect_error(
    dataset_build(matrix(1:4, 2, 2), c("col1", "col2")),
    "the df must be a dataframe"
  )
})

test_that("empty_set", {
  empty <- empty_set()
  expect_s3_class(empty, "dataset")
  expect_true(is_empty_set(empty))
  expect_equal(nrow(empty), 0)
  expect_equal(ids(empty), "variable")
  expect_equal(x_axis(empty), "variable")
  expect_equal(state(empty), "wide")
  expect_equal(id_cols(empty), character(0))
  expect_equal(val_cols(empty), character(0))
})

test_that("is_empty_set on non-empty dataset", {
  df <- data.frame(id1 = 1, variable = "x", val = 10, stringsAsFactors = FALSE)
  ds <- dataset_build(df, "id1")
  expect_false(is_empty_set(ds))
})

test_that("x_axis, ids, state, id_cols, val_cols", {
  df <- data.frame(
    id1 = 1:2,
    variable = c("x", "y"),
    val = 10:11,
    stringsAsFactors = FALSE
  )
  ds <- dataset_build(df, "id1")

  expect_equal(x_axis(ds), "variable")
  expect_equal(ids(ds), c("id1", "variable"))
  expect_equal(state(ds), "wide")
  expect_equal(id_cols(ds), "id1")
  expect_equal(val_cols(ds), c("variable", "val"))
})

test_that("set_attr allows custom attributes", {
  df <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  res <- set_attr(df, c("a", "b", "c"), "x", "long")
  expect_s3_class(res, "dataset")
  expect_equal(attr(res, "dataset_ids"), c("a", "b", "c"))
  expect_equal(attr(res, "dataset_x_axis"), "x")
  expect_equal(attr(res, "dataset_state"), "long")
})

test_that("empty_rows and empty_cols with NA values", {
  df <- data.frame(
    id1 = 1:3,
    variable = c("x", "y", "z"),
    val = c(10, NA, 30),
    stringsAsFactors = FALSE
  )
  ds <- dataset_build(df, "id1")

  er <- empty_rows(ds)
  expect_length(er, 3)
  expect_equal(er, c(FALSE, FALSE, FALSE))

  ec <- empty_cols(ds)
  expect_equal(unname(ec), c(FALSE, FALSE, FALSE))
})

test_that("dataset_collapse removes empty rows and cols", {
  df <- data.frame(
    id1 = 1:2,
    variable = c("x", "y"),
    val = c(NA, 20),
    stringsAsFactors = FALSE
  )
  ds <- dataset_build(df, "id1")

  ds_collapsed <- dataset_collapse(ds)

  expect_equal(nrow(ds_collapsed), 2)
  expect_equal(names(ds_collapsed), c("id1", "variable", "val"))

  expect_equal(ids(ds_collapsed), c("id1", "variable"))
  expect_equal(ds_collapsed$id1, c(1, 2))
  expect_equal(ds_collapsed$variable, c("x", "y"))
})

test_that("id_integrity TRUE for unique ids and FALSE for duplicates (bypass build)", {
  df_uniq <- data.frame(
    id1 = 1:2,
    variable = c("a", "b"),
    val = 10:11,
    stringsAsFactors = FALSE
  )
  ds_uniq <- set_attr(df_uniq, c("id1", "variable"), "variable", "wide")
  expect_true(id_integrity(ds_uniq))

  df_dup <- data.frame(
    id1 = c(1, 1),
    variable = c("a", "b"),
    val = 10:11,
    stringsAsFactors = FALSE
  )
  ds_dup <- set_attr(df_dup, c("id1", "variable"), "variable", "wide")
  expect_false(id_integrity(ds_dup))
})

test_that("dataset_valid stops on duplicates", {
  df_dup <- data.frame(
    id1 = c(1, 1),
    variable = c("a", "b"),
    val = 10:11,
    stringsAsFactors = FALSE
  )
  ds_dup <- set_attr(df_dup, c("id1", "variable"), "variable", "wide")
  expect_error(dataset_valid(ds_dup), "ids do not uniquely identify rows")
})

test_that("dataset_valid stops on invalid state", {
  df <- data.frame(id1 = 1, variable = "x", val = 10, stringsAsFactors = FALSE)
  ds <- set_attr(df, c("id1", "variable"), "variable", "invalid_state")
  expect_error(dataset_valid(ds), "error invalid state: invalid_state")
})

test_that("print.dataset output contains expected text", {
  df <- data.frame(id1 = 1, variable = "x", val = 10, stringsAsFactors = FALSE)
  ds <- dataset_build(df, "id1")

  expect_output(print(ds), "Dataset - State")
  expect_output(print(ds), "Cols: id1 | val, x-Axis: variable")
})
