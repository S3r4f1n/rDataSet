context("set_utils")

library(dplyr)

# Helper: build a simple dataset with one id column, a fixed variable name, and one value column
make_ds <- function(id, val, variable = "x") {
  df <- tibble::tibble(
    id   = id,
    variable = variable,
    val  = val
  )
  dataset_build(df, "id")
}

# Helper: build a dataset with two id columns (id, group)
make_ds2 <- function(id, group, val) {
  df <- tibble::tibble(
    id    = id,
    group = group,
    variable = "x",
    val   = val
  )
  dataset_build(df, c("id", "group"))
}

# --------------------------------------------------------------- dataset_diff
test_that("dataset_diff detects differing values", {
  a <- make_ds(1:3, c(10, 20, 30))
  b <- make_ds(1:3, c(10, 25, 30))

  res <- dataset_diff(a, b)
  expect_s3_class(res, "dataset")
  # only the row with a differing value should appear (id = 2)
  expect_equal(nrow(res), 1)
})

test_that("dataset_diff returns empty when datasets are identical", {
  a <- make_ds(1:3, c(10, 20, 30))
  b <- make_ds(1:3, c(10, 20, 30))

  res <- dataset_diff(a, b)
  expect_s3_class(res, "dataset")
  expect_equal(nrow(res), 0)
})

test_that("dataset_diff handles extra rows in b (strict = 'equal' default)", {
  a <- make_ds(1:2, c(10, 20))
  b <- make_ds(1:3, c(10, 25, 30))

  res <- dataset_diff(a, b)
  expect_s3_class(res, "dataset")
  # rows: id=1 values equal => omitted; id=2 differs; id=3 only in b => appears
  expect_gt(nrow(res), 0)
})

# --------------------------------------------------------------- ds_filter
test_that("ds_filter keeps rows that satisfy the condition", {
  ds <- make_ds(1:5, c(1, 5, 10, 20, 30))
  f  <- ds_filter(ds, val > 10)

  expect_s3_class(f, "dataset")
  expect_setequal(f %>% as_tibble() %>% pull(id), c(4L, 5L))
})

test_that("ds_filter with condition yielding zero rows returns empty dataset", {
  ds <- make_ds(1:3, c(1, 2, 3))
  f  <- ds_filter(ds, val > 100)

  expect_s3_class(f, "dataset")
  expect_equal(nrow(f), 0)
})

# --------------------------------------------------------------- select_ids
test_that("select_ids retains only selected id columns", {
  ds <- make_ds2(id = 1:3, group = c("a","b","a"), val = c(10, 20, 30))
  # select only 'id'
  sel <- select_ids(ds, id)

  expect_s3_class(sel, "dataset")
  expect_equal(id_cols(sel), "id")
  # the value column should still be present
  expect_true("val" %in% names(sel))
})

test_that("select_ids with all id columns returns original structure", {
  ds <- make_ds2(id = 1:3, group = c("x","y","z"), val = c(1, 2, 3))
  sel <- select_ids(ds, id, group)

  expect_s3_class(sel, "dataset")
  expect_equal(sort(id_cols(sel)), c("group", "id"))
  expect_equal(sort(as_tibble(sel)$id), c(1, 2, 3))
})

# --------------------------------------------------------------- intersect_with
test_that("intersect_with keeps rows of b that match both datasets", {
  a <- make_ds(1:3, c(10, 20, 30))
  b <- make_ds(2:4, c(200, 300, 400))

  ii <- intersect_with(a, b)
  expect_s3_class(ii, "dataset")
  ids_in_res <- as_tibble(ii) %>% pull(id) %>% sort()
  expect_equal(ids_in_res, c(2L, 3L))
  # values come from b
  vals <- as_tibble(ii) %>% pull(val) %>% sort()
  expect_equal(vals, c(200, 300))
})

test_that("intersect_with returns empty when no overlap", {
  a <- make_ds(1:2, c(10, 20))
  b <- make_ds(3:4, c(300, 400))

  ii <- intersect_with(a, b)
  expect_s3_class(ii, "dataset")
  expect_equal(nrow(ii), 0)
})

# --------------------------------------------------------------- mask_with
test_that("mask_with overwrites a's values with b's values where b has data", {
  a <- make_ds(1:3, c(10, NA_real_, 30))
  b <- make_ds(2:4, c(200, 300, 400))

  res <- mask_with(a, b, framed = TRUE)
  expect_s3_class(res, "dataset")

  # only rows present in a (id 1,2,3) should appear, with values 10,200,300
  tbl <- as_tibble(res) %>% select(id, val) %>% arrange(id)
  expect_equal(tbl$id,  1:3)
  expect_equal(tbl$val, c(10, 200, 300))
})

test_that("mask_with with framed = FALSE includes rows outside a's frame", {
  a <- make_ds(1:3, c(10, 20, 30))
  b <- make_ds(2:4, c(200, 300, 400))

  res_noframe <- mask_with(a, b, framed = FALSE)
  tbl <- as_tibble(res_noframe) %>% select(id, val) %>% arrange(id)
  # id 4 is included because frame is not applied
  expect_setequal(tbl$id, c(1L,2L,3L,4L))
})

# --------------------------------------------------------------- fill_with
test_that("fill_with fills missing a values from b; keeps existing a values", {
  a <- make_ds(1:3, c(10, NA_real_, 30))
  b <- make_ds(2:4, c(200, 300, 400))

  res <- fill_with(a, b, framed = TRUE)
  expect_s3_class(res, "dataset")

  tbl <- as_tibble(res) %>% select(id, val) %>% arrange(id)
  expect_equal(tbl$id,  1:3)
  expect_equal(tbl$val, c(10, 200, 30))   # id2 missing from a => 200
})

test_that("fill_with with framed = FALSE includes rows outside a's frame", {
  a <- make_ds(1:3, c(10, NA_real_, 30))
  b <- make_ds(2:4, c(200, 300, 400))

  res_noframe <- fill_with(a, b, framed = FALSE)
  tbl <- as_tibble(res_noframe) %>% select(id, val) %>% arrange(id)
  # id 4 is included because we skip the frame restriction
  expect_equal(sort(tbl$id), c(1L,2L,3L,4L))
})

# --------------------------------------------------------------- edge cases
test_that("dataset_diff works when b is empty", {
  a <- make_ds(1:3, c(10, 20, 30))
  b <- empty_set()

  res <- dataset_diff(a, b)
  expect_s3_class(res, "dataset")
  # All rows of a should be considered as differing because b has no row
  expect_equal(nrow(res), 3)
})

test_that("mask_with and fill_with handle empty a correctly", {
  a <- empty_set()
  b <- make_ds(1:2, c(100, 200))

  # mask_with
  masked <- mask_with(a, b, framed = TRUE)
  expect_s3_class(masked, "dataset")
  expect_equal(nrow(masked), 0)

  # fill_with
  filled <- fill_with(a, b, framed = TRUE)
  expect_s3_class(filled, "dataset")
  expect_equal(nrow(filled), 0)
})

test_that("ds_filter works with datasets that are already empty", {
  ds <- empty_set()
  f <- ds_filter(ds, val > 10)
  expect_s3_class(f, "dataset")
  expect_equal(nrow(f), 0)
})

test_that("select_ids with empty dataset returns empty dataset", {
  ds <- empty_set()
  sel <- select_ids(ds, id)
  expect_s3_class(sel, "dataset")
  expect_equal(nrow(sel), 0)
})
