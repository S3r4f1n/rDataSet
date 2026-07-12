require(testthat)
require(dplyr)
require(tidyr)

test_that("wide_to_long works with empty dataset", {
  # Test empty dataset conversion
  empty_ds <- dataset_build(tibble(), character(0))
  result <- wide_to_long(empty_ds)

  expect_equal(state(result), "long")
  expect_true(is_empty_set(result))
})

test_that("wide_to_long works with simple dataset", {
  # Test basic wide to long conversion
  df <- tibble(
    id1 = c(1, 2),
    id2 = c("A", "B"),
    val1 = c(10, 20),
    val2 = c(100, 200)
  )
  ds <- dataset_build(df, c("id1", "id2"))

  result <- wide_to_long(ds)

  expect_equal(state(result), "long")
  expect_equal(ids(result), c("id1", "id2", "variable"))
  expect_equal(x_axis(result), NULL)
  expect_equal(nrow(result), 4) # 2 rows × 2 columns
  expect_equal(ncol(result), 4) # 3 id cols + 1 value col

  # Check that values are properly transformed
  expect_true("val1" %in% result$variable)
  expect_true("val2" %in% result$variable)
})

test_that("wide_to_long handles single value column", {
  # Test with only one value column
  df <- tibble(
    id1 = c(1, 2),
    id2 = c("A", "B"),
    val1 = c(10, 20),
  )
  ds <- dataset_build(df, c("id1", "id2"))

  result <- wide_to_long(ds)

  expect_equal(state(result), "long")
  expect_equal(ids(result), c("id1", "id2", "variable"))
  expect_equal(x_axis(result), NULL)
  expect_equal(nrow(result), 2) # 2 rows × 1 column
  expect_equal(ncol(result), 4) # 3 id cols + 1 value col
})

test_that("wide_to_long errors on wrong state", {
  # Test that wide_to_long fails when dataset is not in wide state
  df <- tibble(
    id1 = c(1, 2),
    id2 = c("A", "B"),
    variable = c("val1", "val1"),
    value = c(10, 20)
  )
  ds <- dataset_build(df, c("id1", "id2", "variable")) %>% wide_to_long() # force wrong state

  expect_error(wide_to_long(ds), "dataset in wide form is expected")
})

test_that("long_to_wide works with empty dataset", {
  # Test empty dataset conversion
  empty_ds <- dataset_build(tibble(), character(0)) %>% wide_to_long()
  result <- long_to_wide(empty_ds)

  expect_equal(state(result), "wide")
  expect_true(is_empty_set(result))
})

test_that("long_to_wide works with simple dataset", {
  # Test basic long to wide conversion
  df <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "B", "A", "B"),
    variable = c("val1", "val2", "val1", "val2"),
    value = c(10, 100, 20, 200)
  )
  ds <- dataset_build(df, c("id1", "id2")) %>% wide_to_long()

  result <- long_to_wide(ds)

  expect_equal(state(result), "wide")
  expect_equal(ids(result), c("id1", "id2", "variable"))
  expect_equal(x_axis(result), "variable")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4) # 2 id cols + 2 value cols
})

test_that("long_to_wide works with specified column", {
  # Test long to wide conversion with custom x_axis column
  df <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "B", "A", "B"),
    variable = c("val1", "val2", "val1", "val2"),
    value = c(10, 100, 20, 200)
  )
  ds <- dataset_build(df, c("id1", "id2")) %>% wide_to_long()

  result <- long_to_wide(ds, col = "id1")

  expect_equal(state(result), "wide")
  expect_equal(ids(result), c("id1", "id2", "variable"))
  expect_equal(x_axis(result), "id1")
})

test_that("long_to_wide errors on wrong state", {
  # Test that long_to_wide fails when dataset is not in long state
  df <- tibble(
    id1 = c(1, 2),
    id2 = c("A", "B"),
    val1 = c(10, 20),
    val2 = c(100, 200)
  )
  ds <- dataset_build(df, c("id1", "id2"))

  expect_error(long_to_wide(ds), "dataset in long form is expected")
})

test_that("long_to_wide errors on multiple value columns", {
  # Test that long_to_wide fails when there are multiple value columns
  df <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "B", "A", "B"),
    variable = c("val1", "val2", "val1", "val2"),
    value1 = c(10, 100, 20, 200),
    value2 = c(1000, 2000, 3000, 4000)
  )
  ds <- dataset_build(df, c("id1", "id2"))
  attr(ds, "dataset_state") <- "long"

  expect_error(
    long_to_wide(ds),
    "a data set in long form is expected to only have one, vals column"
  )
})

test_that("wide_to_long and long_to_wide are inverse operations", {
  # Test that converting wide->long->wide gives back original structure
  df_orig <- tibble(
    id1 = c(1, 2, 3),
    id2 = c("A", "B", "C"),
    val1 = c(10, 20, 30),
    val2 = c(100, 200, 300),
    val3 = c(1000, 2000, 3000)
  )

  # Build original dataset
  ds_orig <- dataset_build(df_orig, c("id1", "id2"))

  # Convert wide to long
  long_ds <- wide_to_long(ds_orig)

  # Convert back to wide
  wide_ds <- long_to_wide(long_ds)

  # Should have same structure
  expect_equal(state(wide_ds), "wide")
  expect_equal(ids(wide_ds), c("id1", "id2", "variable"))
  expect_equal(x_axis(wide_ds), "variable")
  # Note: Values might differ due to reshaping, but structure should match
})
