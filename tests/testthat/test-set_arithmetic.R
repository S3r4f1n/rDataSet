test_that("dataset_minus works with basic case", {
  # Create test datasets
  a <- dataset_build(tibble(
    id1 = c("A", "B", "C"),
    id2 = c("X", "Y", "Z"),
    val1 = c(1, 2, 3),
    val2 = c(4, 5, 6)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("A", "B", "C"),
    id2 = c("X", "Y", "Z"),
    val1 = c(NA, 2, NA),
    val2 = c(4, NA, 6)
  ), ids = c("id1", "id2"))
  
  result <- dataset_minus(a, b)
  
  # Should keep values from a where b has NA
  expect_equal(result$val1, c(1, NA, 3))  # val1: A(1), B(NA), C(3)
  expect_equal(result$val2, c(NA, 5, NA)) # val2: A(NA), B(5), C(NA)
})

test_that("dataset_minus handles empty datasets", {
  # Empty a
  a <- dataset_build(tibble(), character(0))
  b <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  result <- dataset_minus(a, b)
  expect_equal(nrow(result), 0)
  
  # Empty b
  a <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  b <- dataset_build(tibble(), character(0))
  
  result <- dataset_minus(a, b)
  expect_equal(result$val1, 1)
})

test_that("dataset_minus errors on mismatched IDs", {
  a <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = "A",
    id3 = "X",  # Different ID column
    val1 = 1
  ), ids = c("id1", "id3"))
  
  expect_error(dataset_minus(a, b))
})

test_that("dataset_intersect works with basic case", {
  # Create test datasets
  a <- dataset_build(tibble(
    id1 = c("A", "B", "C"),
    id2 = c("X", "Y", "Z"),
    val1 = c(1, 2, 3),
    val2 = c(4, 5, 6)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("A", "B", "C"),
    id2 = c("X", "Y", "Z"),
    val1 = c(NA, 2, NA),
    val2 = c(4, NA, 6)
  ), ids = c("id1", "id2"))
  
  result <- dataset_intersect(a, b)
  
  # Should keep values from a where b has non-NA
  expect_equal(result$val1, c(NA, 2, NA))  # val1: A(NA), B(2), C(NA)
  expect_equal(result$val2, c(4, NA, 6))   # val2: A(4), B(NA), C(6)
})

test_that("dataset_intersect handles empty datasets", {
  # Empty a
  a <- dataset_build(tibble(), character(0))
  b <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  result <- dataset_intersect(a, b)
  expect_equal(nrow(result), 0)
  
  # Empty b
  a <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  b <- dataset_build(tibble(), character(0))
  
  result <- dataset_intersect(a, b)
  expect_equal(nrow(result), 0)
})

test_that("dataset_intersect errors on mismatched IDs", {
  a <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = "A",
    id3 = "X",  # Different ID column
    val1 = 1
  ), ids = c("id1", "id3"))
  
  expect_error(dataset_intersect(a, b))
})

test_that("dataset_union works with basic case", {
  # Create test datasets
  a <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 2),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("A", "C"),
    id2 = c("X", "Z"),
    val1 = c(NA, 3),
    val2 = c(4, 6)
  ), ids = c("id1", "id2"))
  
  result <- dataset_union(a, b)
  
  # Should have all rows and columns
  expect_equal(nrow(result), 3)  # A, B, C
  expect_equal(ncol(result), 4)  # id1, id2, val1, val2
  
  # Should take precedence from a when both exist
  expect_equal(result$val1[1], 1)  # A from a
  expect_equal(result$val2[1], 4)  # A from a
  expect_equal(result$val1[2], 2)  # B from a
  expect_equal(result$val2[2], 5)  # B from a
  expect_equal(result$val1[3], 3)  # C from b (since A has NA in val1)
  expect_equal(result$val2[3], 6)  # C from b (since C doesn't exist in a)
})

test_that("dataset_union handles empty datasets", {
  # Empty a
  a <- dataset_build(tibble(), character(0))
  b <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  result <- dataset_union(a, b)
  expect_equal(nrow(result), 1)
  expect_equal(result$val1, 1)
  
  # Empty b
  a <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  b <- dataset_build(tibble(), character(0))
  
  result <- dataset_union(a, b)
  expect_equal(nrow(result), 1)
  expect_equal(result$val1, 1)
})

test_that("dataset_union errors on mismatched IDs", {
  a <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = "A",
    id3 = "X",  # Different ID column
    val1 = 1
  ), ids = c("id1", "id3"))
  
  expect_error(dataset_union(a, b))
})

test_that("dataset_equality works with identical datasets", {
  a <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 2),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 2),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  expect_true(dataset_equality(a, b))
})

test_that("dataset_equality works with different values", {
  a <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 2),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 3),  # Different value
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  expect_false(dataset_equality(a, b))
})

test_that("dataset_equality works with NA values", {
  a <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, NA),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, NA),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  expect_true(dataset_equality(a, b))
})

test_that("dataset_equality works with missing columns", {
  a <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 2),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 2)
    # Missing val2 column
  ), ids = c("id1", "id2"))
  
  expect_false(dataset_equality(a, b))
})

test_that("dataset_equality works with empty datasets", {
  a <- dataset_build(tibble(), character(0))
  b <- dataset_build(tibble(), character(0))
  
  expect_true(dataset_equality(a, b))
})

test_that("dataset_equality handles one empty dataset", {
  a <- dataset_build(tibble(), character(0))
  b <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  expect_false(dataset_equality(a, b))
})

test_that("dataset_equality errors on mismatched IDs", {
  a <- dataset_build(tibble(
    id1 = "A",
    id2 = "X",
    val1 = 1
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = "A",
    id3 = "X",  # Different ID column
    val1 = 1
  ), ids = c("id1", "id3"))
  
  expect_error(dataset_equality(a, b))
})

test_that("dataset_equality with different row orders", {
  a <- dataset_build(tibble(
    id1 = c("A", "B"),
    id2 = c("X", "Y"),
    val1 = c(1, 2),
    val2 = c(4, 5)
  ), ids = c("id1", "id2"))
  
  b <- dataset_build(tibble(
    id1 = c("B", "A"),  # Different order
    id2 = c("Y", "X"),
    val1 = c(2, 1),
    val2 = c(5, 4)
  ), ids = c("id1", "id2"))
  
  expect_true(dataset_equality(a, b))
})
