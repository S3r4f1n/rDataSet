require(dplyr)

#' Set difference (a - b)
#'
#' Returns cells from `a` that are missing (or differ) in `b`.
#' Under the hood this is a `setdiff` merge with left precedence.
#'
#' @param a A dataset object (minuend).
#' @param b A dataset object (subtrahend).
#' @return A dataset containing only values from `a` where `b` is `NA`.
#' @export
dataset_minus <- function(a, b) {
  merge_with(a, b, "setdiff", "left")
}

#' Set intersection (a & b)
#'
#' Keeps cells from `a` that also exist in `b`.
#'
#' @param a A dataset object.
#' @param b A dataset object.
#' @return A dataset with the intersection values taken from `a`.
#' @export
dataset_intersect <- function(a, b) {
  merge_with(a, b, "intersect", "left")
}

#' Set union (a | b)
#'
#' Returns all cells present in either dataset, giving precedence to `a`
#' when both sides have a value.
#'
#' @param a A dataset object (left operand).
#' @param b A dataset object (right operand).
#' @return A dataset containing the union of values.
#' @export
dataset_union <- function(a, b) {
  merge_with(a, b, "union", "left")
}

#' Test equality of two datasets
#'
#' Compares two datasets cell-by-cell, including ID columns and value
#' columns. Issues a warning if value columns differ.
#'
#' @param a A dataset object.
#' @param b A dataset object.
#' @return `TRUE` if the datasets are identical, `FALSE` otherwise.
#' @export
dataset_equality <- function(a, b) {
  id_a <- ids(a)
  id_b <- ids(b)

  if (!identical(id_a, id_b)) {
    stop(
      "ids cols don't match, datasets are incomparable:",
      "\nids a: ",
      paste(id_a, collapse = ", "),
      "\nids b: ",
      paste(id_b, collapse = ", ")
    )
  }

  val_a <- val_cols(a)
  val_b <- val_cols(b)

  if (!setequal(val_a, val_b)) {
    warning(
      "value columns don't match:",
      "\ncommon value cols: ",
      paste(intersect(val_a, val_b), collapse = ", "),
      "\nextra value cols in a: ",
      paste(setdiff(val_a, val_b), collapse = ", "),
      "\nextra value cols in b: ",
      paste(setdiff(val_b, val_a), collapse = ", ")
    )
    return(FALSE)
  }

  if (is_empty_set(a) && is_empty_set(b)) {
    return(TRUE)
  }
  if (is_empty_set(a) || is_empty_set(b)) {
    return(FALSE)
  }

  long_a <- to_long(a) %>%
    dplyr::arrange(dplyr::across(dplyr::everything()))
  long_b <- to_long(b) %>%
    dplyr::arrange(dplyr::across(dplyr::everything()))

  isTRUE(all.equal(long_a, long_b, check.attributes = FALSE))
}
