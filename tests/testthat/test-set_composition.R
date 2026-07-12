require(testthat)
require(dplyr)

test_that("dataset_compose works with empty dataset", {
  # Test empty dataset composition
  empty_ds <- dataset_build(tibble(), character(0))
  # Decompose first to get proper input for compose
  decomposed <- dataset_decompose(empty_ds)
  result <- dataset_compose(decomposed)
  expect_true(is_empty_set(result))
})

test_that("dataset_compose works with single dataset", {
  # Test composing a single dataset
  df <- tibble(id1 = c(1, 2), id2 = c("A", "B"), val1 = c(10, 20))
  ds <- dataset_build(df, c("id1", "id2"))

  # Need to decompose first to get proper input for compose
  decomposed <- dataset_decompose(ds)
  result <- dataset_compose(decomposed)
  expect_equal(state(result), "wide")
  expect_equal(ids(result), c("id1", "id2", "variable"))
  expect_equal(x_axis(result), "variable")
})

test_that("dataset_compose works with multiple datasets", {
  # Test composing multiple datasets
  df1 <- tibble(id1 = c(1, 2), id2 = c("A", "B"), val1 = c(10, 20))
  df2 <- tibble(id1 = c(1, 2), id2 = c("A", "B"), val2 = c(30, 40))

  ds1 <- dataset_build(df1, c("id1", "id2"))
  ds2 <- dataset_build(df2, c("id1", "id2"))

  # Decompose both datasets first
  decomposed1 <- dataset_decompose(ds1)
  decomposed2 <- dataset_decompose(ds2)

  # Combine the decomposed lists
  combined_list <- set_attr(
    c(decomposed1, decomposed2),
    c("id1", "id2"),
    "variable",
    "decomposed"
  )
  result <- dataset_compose(combined_list)
  expect_equal(state(result), "wide")
  expect_equal(ids(result), c("id1", "id2"))
  expect_equal(x_axis(result), "variable")
  expect_true("val1" %in% names(result))
  expect_true("val2" %in% names(result))
})

test_that("dataset_compose errors on wrong state", {
  # Test that compose fails when dataset is not in decomposed state
  df <- tibble(id1 = c(1, 2), id2 = c("A", "B"), val1 = c(10, 20))
  ds <- dataset_build(df, c("id1", "id2"))

  expect_error(
    dataset_compose(ds),
    "dataset in composed form is expected but attr.dataset, 'dataset_state'. is: wide"
  )
})

test_that("dataset_decompose works with empty dataset", {
  # Test decomposing an empty dataset
  empty_ds <- dataset_build(tibble(), character(0))
  result <- dataset_decompose(empty_ds)

  expect_equal(state(result), "decomposed")
  expect_true(is_empty_set(result[[1]]))
})

test_that("dataset_decompose works with simple dataset", {
  # Test decomposing a simple dataset
  df <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "B", "A", "B"),
    val1 = c(10, 20, 30, 40),
    val2 = c(100, 200, 300, 400)
  )
  ds <- dataset_build(df, c("id1", "id2"))

  result <- dataset_decompose(ds)
  expect_equal(state(result), "decomposed")
  expect_equal(length(result), 2) # Should decompose into 2 parts
})

test_that("slice_dataset works correctly", {
  # Test slicing a dataset
  df <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "B", "A", "B"),
    val1 = c(10, 20, 30, 40),
    val2 = c(100, 200, 300, 400)
  )
  ds <- dataset_build(df, c("id1", "id2"))

  result <- slice_dataset(ds, c("id1"), c("val1"), "variable")
  expect_equal(state(result), "wide")
  expect_equal(ids(result), c("id1", "variable"))
  expect_equal(x_axis(result), "variable")
  expect_true("val1" %in% names(result))
})

test_that("df_functional_dependence works correctly", {
  # Test functional dependence detection
  df <- tibble(
    id1 = c(1, 1, 2, 3),
    id2 = c("A", "A", "B", "B"),
    val1 = c(10, 10, 20, 20),
    val2 = c(100, 200, 300, 400)
  )

  result <- df_functional_dependence(df$id1, df)
  expect_equal(result, c(TRUE, TRUE, TRUE, FALSE)) # id1 should be functionally dependent on itself

  result <- df_functional_dependence(df$id2, df)
  expect_equal(result, c(FALSE, TRUE, TRUE, FALSE)) # id2 should be functionally dependent on itself
})

test_that("hirarchical_paths works correctly", {
  # Test hierarchical paths generation
  df <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "B", "A", "B"),
    val1 = c(10, 20, 30, 40)
  )
  ds <- dataset_build(df, c("id1", "id2"))

  result <- hirarchical_paths(ds)
  expect_equal(nrow(result), 2)
  expect_equal(colnames(result), c("ids", "paths"))
})

test_that("efficient_paths works correctly", {
  # Test efficient paths generation
  df <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("A", "B", "A", "B"),
    id3 = c("A", "B", "A", "C"),
    val1 = c(10, 20, 30, 40)
  )
  ds <- dataset_build(df, c("id1", "id2", "id3"))

  result <- efficient_paths(ds)
  expect_true(nrow(result) >= 1)
  expect_equal(colnames(result), c("ids", "paths"))
  expect_equal(nrow(result), sum(choose(3, 1:3)))
})

test_that("dataset_decompose handles complex relationships", {
  # Test decomposing dataset with complex functional dependencies
  df <- tibble(
    id1 = c(1, 1, 1, 1, 2, 2, 2, 2),
    id2 = c("A", "A", "B", "B", "A", "A", "B", "B"),
    id3 = c("X", "Y", "X", "Y", "X", "Y", "X", "Y"),
    val1 = c(10, 10, 20, 20, 30, 30, 40, 40),
    val2 = c(100, 200, 300, 400, 500, 600, 700, 800),
    val3 = c(1000, 1000, 2000, 2000, 3000, 3000, 4000, 4000)
  )
  ds <- dataset_build(df, c("id1", "id2", "id3"))

  result <- dataset_decompose(ds)
  expect_equal(state(result), "decomposed")
  expect_true(length(result) >= 1)
})
