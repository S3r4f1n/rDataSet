# Test fixtures
A <- dataset_build(tibble(i = 1:10, b = if_else(1:10 %% 2 == 0, NA, 1:10), what = "hi"), "i")
B <- dataset_build(tibble(i = 1:10, b = na_if(1:10, 3), what = "hi"), "i")
C <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "i")
D <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "b")

test_that("basic roundtrip", {
  expect_equal(A == (A %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
  expect_equal(B == (B %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
  expect_equal(C == (C %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
  expect_equal(D == (D %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
})

test_that("double roundtrip", {
  expect_equal(A == (A %>% dataset_to_long() %>% dataset_to_wide() %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
  expect_equal(C == (C %>% dataset_to_long() %>% dataset_to_wide() %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
})

test_that("long format structure", {
  long_A <- dataset_to_long(A)
  expect_equal("long", attr(long_A, "dataset_state"))
  expect_true("variable" %in% names(ids(long_A)))
  expect_true("value" %in% names(long_A))
})

test_that("wide format restores structure", {
  long_A <- dataset_to_long(A)
  wide_A <- dataset_to_wide(long_A)
  expect_equal("wide", attr(wide_A, "dataset_state"))
  expect_equal(ncol(A), ncol(wide_A))
  expect_equal(nrow(A), nrow(wide_A))
})

test_that("to_wide validates input state", {
  expect_error(dataset_to_wide(A), "dataset in long form is expected")
})

test_that("to_long validates input state", {
  long_A <- dataset_to_long(A)
  expect_error(dataset_to_long(long_A), "dataset in wide form is expected")
})

test_that("to_wide validates value column", {
  long_invalid <- dataset_to_long(A)
  names(long_invalid)[names(long_invalid) == "value"] <- "wrong"
  expect_error(dataset_to_wide(long_invalid), "vals column named value")
})

test_that("triple roundtrip", {
  expect_equal(A == (A %>% dataset_to_long() %>% dataset_to_wide() %>% dataset_to_long() %>% dataset_to_wide() %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
})

test_that("custom x_axis column", {
  long_A <- dataset_to_long(A)
  wide_custom <- dataset_to_wide(long_A, "variable")
  expect_equal(A == wide_custom, TRUE)
})

test_that("x_axis attribute preserved", {
  original_x_axis <- attr(A, "dataset_x_axis")
  result <- A %>% dataset_to_long() %>% dataset_to_wide()
  expect_equal(original_x_axis, attr(result, "dataset_x_axis"))
})

test_that("mixed roundtrip", {
  expect_equal(A == (A %>% dataset_to_long() %>% dataset_to_wide() %>% dataset_decompose() %>% dataset_compose()), TRUE)
  expect_equal(A == (A %>% dataset_decompose() %>% dataset_compose() %>% dataset_to_long() %>% dataset_to_wide()), TRUE)
})

test_that("type preservation - numeric values", {
  # C has numeric columns
  result <- C %>% dataset_to_long() %>% dataset_to_wide()

  # Check that numeric columns are still numeric after roundtrip
  expect_equal(class(C$b), class(result$b))
  expect_equal(class(C$c), class(result$c))
  expect_true(is.numeric(result$b))
  expect_true(is.numeric(result$c))
})

test_that("type preservation - integer values", {
  # A has integer ID column
  result <- A %>% dataset_to_long() %>% dataset_to_wide()

  # ID column should preserve type
  expect_equal(class(A$i), class(result$i))
})

test_that("type preservation - character values", {
  # A has character column
  result <- A %>% dataset_to_long() %>% dataset_to_wide()

  # Character column should stay character
  expect_equal(class(A$what), class(result$what))
  expect_true(is.character(result$what))
})
