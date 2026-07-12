library(dplyr)

#' Set Difference
#'
#' Returns values from dataset `a` that are not present in dataset `b`.
#'
#' @param a A dataset object (minuend).
#' @param b A dataset object (subtrahend).
#' @return A dataset containing values from `a` where corresponding values in `b` are `NA`.
#' @export
dataset_minus <- function(a, b) {
  merge_with(a, b, "setdiff", "left")
}

#' Set Intersection
#'
#' Returns values from dataset `a` that are also present in dataset `b`.
#'
#' @param a A dataset object (left operand).
#' @param b A dataset object (right operand, used as filter).
#' @return A dataset containing values from `a` where corresponding values in `b` are not `NA`.
#' @export
dataset_intersect <- function(a, b) {
  merge_with(a, b, "intersect", "left")
}

#' Set Union
#'
#' Returns all values from either dataset `a` or dataset `b`.
#'
#' @param a A dataset object (left operand, takes precedence).
#' @param b A dataset object (right operand).
#' @return A dataset containing values from `a` where available, otherwise from `b`.
#' @export
dataset_union <- function(a, b) {
  merge_with(a, b, "union", "left")
}

#' Set Equality
#'
#' Compares two datasets cell-by-cell and returns `TRUE` if they are identical.
#'
#' @param a A dataset object.
#' @param b A dataset object.
#' @return `TRUE` if datasets are identical, `FALSE` otherwise.
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
