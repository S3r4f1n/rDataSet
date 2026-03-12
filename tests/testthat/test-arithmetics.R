A <- dataset_build(tibble(i = 1:10, b = if_else(1:10 %% 2 == 0, NA, 1:10)), "i")
B <- dataset_build(tibble(i = 1:10, b = na_if(1:10, 3)), "i")
C <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "i")


test_that("Testing basic equality", {
  expect_equal(all(vals(A == A), na.rm = TRUE), TRUE) # should always hold
  expect_equal(all(vals(B == B), na.rm = TRUE), TRUE) # should always hold
  expect_equal(all(vals(C == C), na.rm = TRUE), TRUE) # should always hold
})

test_that("Testing inequality", {
  expect_equal(any(!vals(A == B), na.rm = TRUE), TRUE) # should always hold
  expect_equal(sum(vals(A == B)), 10-6) # 6 differing vals
  expect_equal(any(!vals(B == A), na.rm = TRUE), TRUE) # should always hold
  expect_equal(sum(vals(B == A)), 10-6) # 6 differing vals

  expect_error(A == C) # missmatch in rows and cols
  expect_error(A == (C > A)) # missmatch in rows
  expect_error((A > C) == C) # missmatch in cols
  # missmatch all values missmatch
  expect_equal(all(!vals((A > C) == (C > A)), na.rm = TRUE), TRUE)
})

test_that("Testing Arithmetics", {
  # always holds
  # $a = (a \cap b) \cup (a \setminus b)$
  expect_equal(all(vals(A == (A > A) + (A - A)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(A == (A > B) + (A - B)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(A == (A > C) + (A - C)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(B == (B > A) + (B - A)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(B == (B > B) + (B - B)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(B == (B > C) + (B - C)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(C == (C > A) + (C - A)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(C == (C > B) + (C - B)), na.rm = TRUE), TRUE)
  expect_equal(all(vals(C == (C > C) + (C - C)), na.rm = TRUE), TRUE)
})
