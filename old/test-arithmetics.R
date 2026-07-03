A <- dataset_build(tibble(i = 1:10, b = if_else(1:10 %% 2 == 0, NA, 1:10), what = "hi"), "i")
B <- dataset_build(tibble(i = 1:10, b = na_if(1:10, 3), what = "hi"), "i")
C <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "i")
D <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "b")


test_that("Testing basic equality", {
  expect_equal(A == A, TRUE) # should always hold
  expect_equal(B == B, TRUE) # should always hold
  expect_equal(C == C, TRUE) # should always hold
})

test_that("Testing inequality", {
  expect_warning(expect_equal(A == B, FALSE))
  expect_warning(expect_equal(B == A, FALSE))

})

test_that("Testing incomparabilty", {
  # if id cols missatch all operations should thrwo errors
  expect_error(A > D)
  expect_error(A - D)
  expect_error(A + D)
  expect_error(A == D)
})

test_that("Testing Intersection", {
  expect_equal(A == (A > A), TRUE)
  expect_equal(B == (B > B), TRUE)
  expect_equal(C == (C > C), TRUE)
})

test_that("Testing Union", {
  expect_equal(A == (A + A), TRUE)
  expect_equal(B == (B + B), TRUE)
  expect_equal(C == (C + C), TRUE)
})

test_that("Testing Minus", {
  empty_set <-  dataset_build(tibble(i = 1, b = NA), "i")
  expect_equal(empty_set == (A - A), TRUE)
  expect_equal(empty_set == (B - B), TRUE)
  expect_equal(empty_set == (C - C), TRUE)
})

test_that("Testing Arithmetics", {
  # always holds
  # $a = (a \cap b) \cup (a \setminus b)$
  expect_equal(A == (A > A) + (A - A), TRUE)
  expect_equal(A == (A > B) + (A - B), TRUE)
  expect_equal(A == (A > C) + (A - C), TRUE)
  expect_equal(B == (B > A) + (B - A), TRUE)
  expect_equal(B == (B > B) + (B - B), TRUE)
  expect_equal(B == (B > C) + (B - C), TRUE)
  expect_equal(C == (C > A) + (C - A), TRUE)
  expect_equal(C == (C > B) + (C - B), TRUE)
  expect_equal(C == (C > C) + (C - C), TRUE)
})
