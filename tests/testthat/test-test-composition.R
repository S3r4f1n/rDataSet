# Test fixtures
A <- dataset_build(tibble(i = 1:10, b = if_else(1:10 %% 2 == 0, NA, 1:10), what = "hi"), "i")
B <- dataset_build(tibble(i = 1:10, b = na_if(1:10, 3), what = "hi"), "i")
C <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "i")
D <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "b")

test_that("basic roundtrip", {
  expect_equal(A == (A %>% dataset_decompose() %>% dataset_compose()), TRUE)
  expect_equal(B == (B %>% dataset_decompose() %>% dataset_compose()), TRUE)
  expect_equal(C == (C %>% dataset_decompose() %>% dataset_compose()), TRUE)
  expect_equal(D == (D %>% dataset_decompose() %>% dataset_compose()), TRUE)
})

test_that("double roundtrip", {
  expect_equal(A == (A %>% dataset_decompose() %>% dataset_compose() %>% dataset_decompose() %>% dataset_compose()), TRUE)
  expect_equal(C == (C %>% dataset_decompose() %>% dataset_compose() %>% dataset_decompose() %>% dataset_compose()), TRUE)
})

test_that("decompose returns proper structure", {
  decomp <- dataset_decompose(A)
  expect_equal("decomposed", attr(decomp, "dataset_state"))
  expect_true(all(purrr::map_lgl(decomp, ~ "dataset" %in% class(.x))))
})

test_that("decompose handles constant values", {
  decomp_C <- dataset_decompose(C)
  recomposed <- dataset_compose(decomp_C)
  expect_true("c" %in% names(recomposed))
  expect_equal(rep(2, 5), recomposed$c)
})

test_that("compose validates input state", {
  expect_error(dataset_compose(A), "dataset in composed form is expected")
})

test_that("decompose validates input state", {
  long_A <- dataset_to_long(A)
  expect_error(dataset_decompose(long_A), "dataset in wide form is expected")
})

test_that("triple roundtrip", {
  expect_equal(A == (A %>% dataset_decompose() %>% dataset_compose() %>% dataset_decompose() %>% dataset_compose() %>% dataset_decompose() %>% dataset_compose()), TRUE)
})

test_that("decompose preserves structure", {
  result <- A %>% dataset_decompose() %>% dataset_compose()
  expect_equal(nrow(A), nrow(result))
  expect_equal(names(A), names(result))
})

test_that("different ID columns", {
  expect_equal(D == (D %>% dataset_decompose() %>% dataset_compose()), TRUE)
  expect_equal(attr(D, "dataset_ids"), attr(D %>% dataset_decompose() %>% dataset_compose(), "dataset_ids"))
})

test_that("type preservation - numeric values", {
  # C has numeric columns
  result <- C %>% dataset_decompose() %>% dataset_compose()

  # Check that numeric columns preserve type
  expect_equal(class(C$b), class(result$b))
  expect_equal(class(C$c), class(result$c))
  expect_true(is.numeric(result$b))
  expect_true(is.numeric(result$c))
})

test_that("type preservation - integer values", {
  # A has integer ID column
  result <- A %>% dataset_decompose() %>% dataset_compose()

  # ID column should preserve type
  expect_equal(class(A$i), class(result$i))
})

test_that("type preservation - character values", {
  # A has character column
  result <- A %>% dataset_decompose() %>% dataset_compose()

  # Character column should stay character
  expect_equal(class(A$what), class(result$what))
  expect_true(is.character(result$what))
})
