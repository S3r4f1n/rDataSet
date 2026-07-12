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

  if (is_empty_set(a)) {
    return(a)
  }
  if (is_empty_set(b)) {
    return(a)
  }

  ids <- id_a
  state <- state(a)
  x_axis <- x_axis(a)

  a <- to_long(a) %>% rename(value_dataset_a_ending = "value")
  b <- to_long(b) %>% rename(value_dataset_b_ending = "value")

  merged <- left_join(a, b, by = id_a, keep = FALSE)

  out <- merged %>%
    mutate(
      value = if_else(
        is.na(value_dataset_b_ending),
        value_dataset_a_ending,
        list(NA)
      )
    ) %>%
    select(-matches("_dataset_._ending$"))

  set_attr(out, ids, NULL, "long") |>
    dataset_transfrom(state, x_axis) |>
    dataset_collapse()
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

  if (is_empty_set(a)) {
    return(a)
  }
  if (is_empty_set(b)) {
    return(b)
  }

  ids <- id_a
  state <- state(a)
  x_axis <- x_axis(a)

  a_long <- to_long(a) %>% rename(value_dataset_a_ending = "value")
  b_long <- to_long(b) %>% rename(value_dataset_b_ending = "value")

  merged <- inner_join(a_long, b_long, by = ids, keep = FALSE)

  out <- merged %>%
    mutate(
      value = if_else(
        !is.na(value_dataset_b_ending),
        value_dataset_a_ending,
        list(NA)
      )
    ) %>%
    select(-matches("_dataset_._ending$"))

  set_attr(out, ids, NULL, "long") |>
    dataset_transfrom(state, x_axis) |>
    dataset_collapse()
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

  if (is_empty_set(a)) {
    return(b)
  }
  if (is_empty_set(b)) {
    return(a)
  }

  ids <- id_a
  state <- state(a)
  x_axis <- x_axis(a)

  a_long <- to_long(a) %>% rename(value_dataset_a_ending = "value")
  b_long <- to_long(b) %>% rename(value_dataset_b_ending = "value")

  merged <- full_join(a_long, b_long, by = id_a, keep = FALSE)

  replace_nulls_with_na <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    i <- which(lengths(x) == 0)
    if (length(i)) {
      x[i] <- list(NA)
    }
    x
  }

  out <- merged %>%
    mutate(
      value = if_else(
        is.na(replace_nulls_with_na(value_dataset_a_ending)),
        value_dataset_b_ending,
        value_dataset_a_ending
      )
    ) %>%
    select(-matches("_dataset_._ending$"))

  set_attr(out, ids, NULL, "long") |>
    dataset_transfrom(state, x_axis) |>
    dataset_collapse()
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
