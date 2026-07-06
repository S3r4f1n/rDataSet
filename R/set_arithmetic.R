library(dplyr)
#' Set Difference
#'
#' Returns values from dataset `a` that are not present in dataset `b`.
#' A value is considered present if it is not `NA`.
#'
#' **Row/Column Handling:**
#' - **Rows:** All rows from `a` are retained (left join on ID columns)
#' - **Columns:** Only value columns that exist in both datasets are considered; ID columns must match
#'
#' **Value Operation:**
#' - For each cell at (row_id, col_id), the value from `a` is kept only if the corresponding cell in `b` is `NA`
#'
#' @param a A dataset object (minuend).
#' @param b A dataset object (subtrahend).
#' @return A dataset containing values from `a` where corresponding values in `b` are `NA`.
#' @details Error if ID columns of `a` and `b` do not match.
#' @keywords internal
dataset_minus <- function(a, b) {
  id_a <- id_cols(a)
  id_b <- id_cols(b)
  if (!identical(id_a, id_b)) {
    stop(
      "ids cols don't match, datasets are incomparable:",
      "\nids a: ",
      paste(id_a, collapse = ", "),
      "\nids b: ",
      paste(id_b, collapse = ", ")
    )
  }

  # empty case
  if (nrow(a) == 0) {
    return(a)
  }
  if (nrow(b) == 0) {
    return(a)
  }

  ids <- id_a
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- left_join(
    a %>%
      rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>%
      select(all_of(c(ids, common))) %>%
      rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids,
    keep = FALSE
  )
  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(is.na(y), x, NA)
      )
    ) %>%
    rename_with(
      function(x) sub("_dataset_a_ending$", "", x),
      matches("_dataset_a_ending$")
    ) %>%
    select(-matches("_dataset_b_ending$"))

  out <- set_attr(out, ids(a), x_axis(a), "wide")
  dataset_collapse(out)
}

#' Set Intersection
#'
#' Returns values from dataset `a` that are also present in dataset `b`.
#' A value is considered present if it is not `NA`. Dataset `b` acts as a filter
#' determining which values from `a` appear in the result.
#'
#' **Row/Column Handling:**
#' - **Rows:** Only rows present in both `a` and `b` (inner join on ID columns)
#' - **Columns:** Only value columns that exist in both datasets; ID columns must match
#'
#' **Value Operation:**
#' - For each cell at (row_id, col_id), the value from `a` is kept only if the corresponding cell in `b` is not `NA`
#'
#' @param a A dataset object (left operand).
#' @param b A dataset object (right operand, used as filter).
#' @return A dataset containing values from `a` where corresponding values in `b` are not `NA`.
#' @details Error if ID columns of `a` and `b` do not match.
#' @keywords internal
dataset_intersect <- function(a, b) {
  id_a <- id_cols(a)
  id_b <- id_cols(b)
  if (!identical(id_a, id_b)) {
    stop(
      "ids cols don't match, datasets are incomparable:",
      "\nids a: ",
      paste(id_a, collapse = ", "),
      "\nids b: ",
      paste(id_b, collapse = ", ")
    )
  }

  if (nrow(a) == 0) {
    return(a)
  }
  if (nrow(b) == 0) {
    return(b)
  }

  ids <- id_a
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- inner_join(
    a %>%
      select(all_of(c(ids, common))) %>%
      rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>%
      select(all_of(c(ids, common))) %>%
      rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids,
    keep = FALSE
  )

  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(!is.na(y), x, NA)
      )
    ) %>%
    rename_with(
      function(x) sub("_dataset_a_ending$", "", x),
      matches("_dataset_a_ending$")
    ) %>%
    select(-matches("_dataset_b_ending$"))

  out <- set_attr(out, ids(a), x_axis(a), "wide")
  dataset_collapse(out)
}

#' Set Union
#'
#' Returns all values from either dataset `a` or dataset `b`.
#' For cells present in both datasets, the value from `a` takes precedence.
#'
#' **Row/Column Handling:**
#' - **Rows:** All rows from both `a` and `b` (full join on ID columns)
#' - **Columns:** All value columns from both datasets; ID columns must match
#'
#' **Value Operation:**
#' - For each cell at (row_id, col_id), uses the value from `a` if present; otherwise falls back to `b`
#' - A value is considered present if it is not `NA`
#'
#' @param a A dataset object (left operand, takes precedence).
#' @param b A dataset object (right operand).
#' @return A dataset containing values from `a` where available, otherwise from `b`.
#' @details Error if ID columns of `a` and `b` do not match.
#' @keywords internal
dataset_union <- function(a, b) {
  id_a <- id_cols(a)
  id_b <- id_cols(b)
  if (!identical(id_a, id_b)) {
    stop(
      "ids cols don't match, datasets are incomparable:",
      "\nids a: ",
      paste(id_a, collapse = ", "),
      "\nids b: ",
      paste(id_b, collapse = ", ")
    )
  }

  # empty case
  if (nrow(a) == 0) {
    return(b)
  }
  if (nrow(b) == 0) {
    return(a)
  }

  ids <- id_a
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- full_join(
    a %>%
      rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>%
      rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids,
    keep = FALSE
  )

  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(is.na(x), y, x)
      )
    ) %>%
    rename_with(
      function(x) sub("_dataset_a_ending$", "", x),
      matches("_dataset_a_ending$")
    ) %>%
    select(-matches("_dataset_b_ending$"))

  out <- set_attr(out, ids(a), x_axis(a), "wide")
  dataset_collapse(out)
}

#' Set Equality
#'
#' Compares two datasets cell-by-cell and returns `TRUE` if they are identical,
#' `FALSE` otherwise. This is a strict equality check that requires both datasets
#' to have the same structure and values.
#'
#' **Comparison Criteria:**
#' - **ID Columns:** Must match exactly (same names and values)
#' - **Value Columns:** Must have the same column names
#' - **Rows:** Must have the same rows (by ID)
#' - **Values:** All corresponding cells must have identical values
#'
#' **Value Comparison:**
#' - Both cells must have equal non-`NA` values for a match
#' - A mismatch occurs if one cell is `NA` and the other is not
#' - If both cells are `NA`, they are considered equal
#'
#' @param a A dataset object (left operand).
#' @param b A dataset object (right operand).
#' @return `TRUE` if datasets are identical, `FALSE` otherwise.
#' @details This function implements strict mathematical set equality:
#'   `a == b` iff `a ⊆ b` and `b ⊆ a`. Unlike element-wise comparison
#'   operators, this returns a single boolean value. Throws an error if
#'   ID columns do not match; returns `FALSE` with warnings for other
#'   mismatches (columns, rows, or values).
#' @keywords internal
#'
#' @todo make it work for other formates
dataset_equality <- function(a, b) {
  id_a <- id_cols(a)
  id_b <- id_cols(b)

  # considered an error. Ids cols must match
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
      "\ncommon cols: ",
      paste(intersect(val_a, val_b), collapse = ", "),
      "\nextra cols in a: ",
      paste(setdiff(val_a, val_b), collapse = ", "),
      "\nextra cols in b: ",
      paste(setdiff(val_b, val_a), collapse = ", ")
    )
    return(FALSE)
  }

  # empty case
  if (nrow(a) == 0 && nrow(b) == 0) {
    return(TRUE)
  }
  if (nrow(a) == 0 || nrow(b) == 0) {
    return(FALSE)
  }

  long_a <- to_long(a) %>%
    dplyr::arrange(dplyr::across(dplyr::everything()))
  long_b <- to_long(b) %>%
    dplyr::arrange(dplyr::across(dplyr::everything()))

  isTRUE(all.equal(long_a, long_b, check.attributes = FALSE))
}
