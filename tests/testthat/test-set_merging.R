library(dplyr)
library(testthat)

# helper to replicate combine_datasets' NULL‑to‑NA logic
replace_nulls <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  i <- which(lengths(x) == 0)
  if (length(i)) {
    x[i] <- list(NA)
  }
  x
}

# ---------- combine_datasets --------------------------------------------------
test_that("combine_datasets returns left and right columns", {
  a <- dataset_build(tibble(id = 1:2, a = c(1, 3), b = c(2, 4)), "id")
  b <- dataset_build(tibble(id = 2:3, a = c(5, 7), b = c(6, 8)), "id")

  out <- combine_datasets(
    a,
    b,
    strict = "equal",
    left_name = "L",
    right_name = "R"
  )

  expect_true(all(c("L", "R") %in% names(out)))
  expect_equal(sort(ids(out)), sort(c("id", "variable")))

  # manual expectation via full_join on long forms
  long_a <- to_long(a) %>% rename(L = value)
  long_b <- to_long(b) %>% rename(R = value)
  expected <- full_join(
    long_a,
    long_b,
    by = intersect(names(long_a), names(long_b))
  ) %>%
    mutate(L = replace_nulls(L), R = replace_nulls(R))

  out_df <- as.data.frame(out)
  expect_equal(nrow(out_df), nrow(expected))
  for (col in c("id", "variable", "L", "R")) {
    expect_equal(out_df[[col]], expected[[col]])
  }
})

test_that("combine_datasets errors when ids are not in strict", {
  a <- dataset_build(tibble(id = 1:2, v = c(10, 20)), "id")
  # b has an extra id column
  b <- dataset_build(
    tibble(id = 1:2, extra = c("x", "y"), v = c(1, 2)),
    c("id", "extra")
  )

  expect_error(
    combine_datasets(a, b, strict = "equal"),
    "ids should be one of: equal"
  )
})

test_that("combine_datasets respects strict = greater", {
  a <- dataset_build(tibble(id = 1:2, x = c(1, 3)), "id")
  # b has more ids -> compare_ids returns "less", not in c("equal","greater")
  b <- dataset_build(
    tibble(id = 1:2, grp = c("A", "B"), x = c(5, 6)),
    c("id", "grp")
  )

  expect_error(
    combine_datasets(a, b, strict = c("equal", "greater")),
    "ids should be one of: equal, greater"
  )
})

# ---------- merge_func --------------------------------------------------------
test_that("merge_func left operation with left prec", {
  f <- merge_func("left", "left")
  a <- c(1, NA, 2)
  b <- c(NA, 5, 6)
  expect_equal(f(a, b), c(1, NA, 2))
})

test_that("merge_func right operation with right prec", {
  f <- merge_func("right", "right")
  a <- c(NA, 2, NA)
  b <- c(3, 4, 5)
  # right: !is.na(b) -> take prec_fn (right: prefer right)
  # prec_fn right: if_else(is.na(b), a, b)
  expect_equal(f(a, b), c(3, 4, 5))
})

test_that("merge_func diff operation with left prec", {
  f <- merge_func("setdiff", "left")
  a <- c(1, NA, 2, 3)
  b <- c(NA, 5, NA, 7)
  # diff: !is.na(a) & is.na(b) -> take prec_fn (left: if_else(is.na(a), b, a))
  expect_equal(f(a, b), c(1, NA, 2, NA))
})

test_that("merge_func xor operation with right prec", {
  f <- merge_func("symdiff", "right")
  a <- c(1, NA, 2, NA)
  b <- c(NA, 5, 6, NA)
  # xor: exactly one present
  # prec right: if_else(is.na(b), a, b)
  expect_equal(f(a, b), c(1, 5, NA, NA))
})

test_that("merge_func and operation with left prec", {
  f <- merge_func("intersect", "left")
  a <- c(1, NA, 2, 3)
  b <- c(4, 5, NA, 7)
  # and: both present
  # prec left: if_else(is.na(a), b, a)
  expect_equal(f(a, b), c(1, NA, NA, 3))
})

test_that("merge_func or operation with right prec", {
  f <- merge_func("union", "right")
  a <- c(NA, NA, 2, 3)
  b <- c(4, NA, 6, NA)
  # or: at least one present
  # prec right: if_else(is.na(b), a, b)
  expect_equal(f(a, b), c(4, NA, 6, 3))
})

# ---------- merg_helper -------------------------------------------------------
test_that("merg_helper returns a dataset in original state", {
  a <- dataset_build(tibble(id = 1:2, x = c(10, 30), y = c(20, 40)), "id")
  b <- dataset_build(tibble(id = 2:3, x = c(5, 7), y = c(6, 8)), "id")

  # use merge_func that returns left values
  mf <- merge_func("left", "left")

  out <- merg_helper(
    a,
    b,
    merge_func = mf,
    strict = "equal",
    keep = FALSE,
    left_name = "LL",
    right_name = "RR"
  )

  expect_equal(state(out), "wide")
  expect_equal(x_axis(out), "variable")
  expect_equal(id_cols(out), "id")

  # values: only rows where left is present (id = 1,2)
  out_df <- as.data.frame(out)
  expect_equal(sort(out_df$id), c(1, 2))
  # for id=1, variable values should come from a (b has no row for id=1)
  # for id=2, left prec gives a's values
  expect_equal(
    out_df %>% filter(id == 1),
    a %>% filter(id == 1) %>% as.data.frame()
  )
  expect_equal(
    out_df %>% filter(id == 2),
    a %>% filter(id == 2) %>% as.data.frame()
  )
})

test_that("merg_helper with keep = FALSE drops rows where result is NA", {
  a <- dataset_build(tibble(id = 1:2, x = c(1, NA)), "id")
  b <- dataset_build(tibble(id = 1:2, x = c(NA, 5)), "id")

  # left operation: keeps only where a is not NA => row 1 has value, row 2 NA
  mf <- merge_func("left", "left")
  out <- merg_helper(a, b, merge_func = mf, strict = "equal", keep = FALSE)

  out_df <- as.data.frame(out)
  expect_equal(nrow(out_df), 1)
  expect_equal(out_df$id, 1)
  expect_equal(out_df$x, 1)
})

# ---------- mask_with ---------------------------------------------------------
test_that("mask_with replaces values where b has values", {
  a <- dataset_build(tibble(id = 1:3, x = c(10, NA, 30)), "id")
  b <- dataset_build(tibble(id = 1:3, x = c(1, 2, NA)), "id")

  out <- mask_with(a, b)
  out_df <- as.data.frame(out)
  # id=1: b has value -> replaced by b; id=2: b has value -> replaced; id=3: b NA -> keep original
  expect_equal(out_df %>% arrange(id) %>% pull(x), c(1, 2, 30))
})

test_that("mask_with allows b to have additional ids (strict = greater)", {
  a <- dataset_build(tibble(id = 1:2, x = c(10, 20)), "id")
  # b has extra id column 'grp'
  b <- dataset_build(
    tibble(id = 1:2, grp = c("A", "B"), x = c(1, 2)),
    c("id", "grp")
  )
  out <- mask_with(b, a) # strict = c("equal","greater") internally
  out_df <- as.data.frame(out)
  expect_equal(out_df %>% arrange(id) %>% pull(x), c(10, 20))
})

# ---------- fill_with ---------------------------------------------------------
test_that("fill_with fills missing values in a from b", {
  a <- dataset_build(tibble(id = 1:3, x = c(10, NA, NA)), "id")
  b <- dataset_build(tibble(id = 1:3, x = c(NA, 2, 3)), "id")

  out <- fill_with(a, b)
  out_df <- as.data.frame(out)
  expect_equal(out_df %>% arrange(id) %>% pull(x), c(10, 2, 3))
})

# ---------- merge_with --------------------------------------------------------
test_that("merge_with works with left set operation and left prec", {
  a <- dataset_build(tibble(id = 1:2, x = c(100, 200)), "id")
  b <- dataset_build(tibble(id = 2:3, x = c(5, 7)), "id")

  out <- merge_with(
    a,
    b,
    set_op = "left",
    prec = "left",
    strict = "equal",
    keep = TRUE
  )
  out_df <- as.data.frame(out)
  # left operation: only rows where a has value (id = 1,2)
  expect_equal(sort(unique(out_df$id)), c(1, 2, 3))
  expect_equal(
    out_df %>% filter(id == 1) %>% pull(x) %>% as.list(),
    pull(filter(to_long(a), id == 1), value)
  )
  expect_equal(
    out_df %>% filter(id == 2) %>% pull(x) %>% as.list(),
    pull(filter(to_long(a), id == 2), value)
  )
})

test_that("merge_with diff operation with right prec", {
  a <- dataset_build(tibble(id = 1:3, x = c(1, NA, 3)), "id")
  b <- dataset_build(tibble(id = 1:3, x = c(NA, 2, NA)), "id")
  out <- merge_with(
    a,
    b,
    set_op = "setdiff",
    prec = "right",
    strict = "equal",
    keep = TRUE
  )
  out_df <- as.data.frame(out)
  # diff: !is.na(a) & is.na(b) -> take prec_fn (right: if_else(is.na(b), a, b))
  # id=1: a=1, b=NA -> TRUE, prec right: if_else(is.na(b), a, b) => a=1
  # id=2: a=NA => FALSE
  # id=3: a=3, b=NA => TRUE, prec right => a=3
  expect_equal(sort(out_df$id), c(1, 2, 3))
})

test_that("merge_with and operation with left prec", {
  a <- dataset_build(tibble(id = 1:3, x = c(1, 2, NA)), "id")
  b <- dataset_build(tibble(id = 1:3, x = c(4, NA, 6)), "id")
  out <- merge_with(
    a,
    b,
    set_op = "intersect",
    prec = "left",
    strict = "equal",
    keep = FALSE
  )
  out_df <- as.data.frame(out)
  # and: both present -> id=1 only
  expect_equal(out_df$id, 1)
  expect_equal(out_df$x, 1)
})

test_that("merge_with or operation with right prec", {
  a <- dataset_build(tibble(id = 1:3, x = c(NA, 2, NA)), "id")
  b <- dataset_build(tibble(id = 1:3, x = c(4, NA, 6)), "id")
  out <- merge_with(
    a,
    b,
    set_op = "union",
    prec = "right",
    strict = "equal",
    keep = TRUE
  )
  out_df <- as.data.frame(out)
  # or: at least one present -> all ids
  expect_equal(sort(out_df$id), 1:3)
  # right prec: id1: b=4, id2: a=2, id3: b=6
  expect_equal(out_df %>% arrange(id) %>% pull(x), c(4, 2, 6))
})
