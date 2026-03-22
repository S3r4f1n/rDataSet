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
  id_a <- ids(a)
  id_b <- ids(b)
  if(any(names(id_a) != names(id_b))) {
    stop("ids cols don't match, datasets are incomparable:",
      "\nids a: ", paste(names(id_a), collapse = ", "),
      "\nids b: ", paste(names(id_b), collapse = ", ")
    )
  }

  # empty case
  if(nrow(a) == 0) return(a)
  if(nrow(b) == 0) return(a)

  ids <- names(id_a)
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- left_join(
    a %>% rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>% select(all_of(c(ids, common))) %>% rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids
  )
  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(is.na(y), x, NA)
      )
    ) %>%
    rename_with(function(x) sub("_dataset_a_ending$", "", x), matches("_dataset_a_ending$")) %>%
    select(-matches("_dataset_b_ending$"))
  
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
  id_a <- ids(a)
  id_b <- ids(b)
  if(any(names(id_a) != names(id_b))) {
    stop("ids cols don't match, datasets are incomparable:",
      "\nids a: ", paste(names(id_a), collapse = ", "),
      "\nids b: ", paste(names(id_b), collapse = ", ")
    )
  }

  if(nrow(a) == 0) return(a)
  if(nrow(b) == 0) return(b)

  ids <- names(id_a)
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- inner_join(
    a %>% select(all_of(c(ids, common))) %>% rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>% select(all_of(c(ids, common))) %>% rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids
  )

  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(!is.na(y), x, NA)
      )
    ) %>%
    rename_with(function(x) sub("_dataset_a_ending$", "", x), matches("_dataset_a_ending$")) %>%
    select(-matches("_dataset_b_ending$"))
  
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
  id_a <- ids(a)
  id_b <- ids(b)
  # if(any(names(id_a) != names(id_b))) {
  #   stop("ids cols don't match, datasets are incomparable:",
  #     "\nids a: ", paste(names(id_a), collapse = ", "),
  #     "\nids b: ", paste(names(id_b), collapse = ", ")
  #   )
  # }

  # empty case
  if(nrow(a) == 0) return(b)
  if(nrow(b) == 0) return(a)

  ids <- c(names(id_a), names(id_b))
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- full_join(
    a %>% rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>% rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common))
  )

  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(is.na(x), y, x)
      )
    ) %>%
    rename_with(function(x) sub("_dataset_a_ending$", "", x), matches("_dataset_a_ending$")) %>%
    select(-matches("_dataset_b_ending$"))
  
  class(out) <- c("dataset", class(out))
  dataset_integrity(out)
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
dataset_equality <- function(a, b) {
  id_a <- ids(a)
  id_b <- ids(b)

  # considered an error. Ids cols must match
  if(any(names(id_a) != names(id_b))) {
    stop("ids cols don't match, datasets are incomparable:",
      "\nids a: ", paste(names(id_a), collapse = ", "),
      "\nids b: ", paste(names(id_b), collapse = ", ")
    )
  }

  val_a <- vals(a)
  val_b <- vals(b)

  col_match <- intersect(names(val_a), names(val_b))

  if(any(!names(val_a) %in% col_match, !names(val_b) %in% col_match)) {
    warning("collumns don't match:",
      "\ncommon cols: ", paste(names(col_match), collapse = ", "),
      "\nextra cols in a: ", paste(setdiff(names(val_a), col_match), collapse = ", "),
      "\nextra cols in b: ", paste(setdiff(names(val_b), col_match), collapse = ", ")
    )
    return(FALSE)
  }

  # empty case
  if(nrow(a) == 0 && nrow(b) == 0) return(TRUE)
  if(xor(nrow(a) == 0, nrow(b) == 0)) return(FALSE)

  id_match <- inner_join(id_a, id_b)

  if(any(nrow(id_a) != nrow(id_match), nrow(id_b) != nrow(id_match))) {
    warning("rows missmatch:",
      "\nextra rows in a: ", paste(anti_join(id_a, id_match), collapse = ", "),
      "\nextra rows in b: ", paste(anti_join(id_b, id_match), collapse = ", ")
    )
    return(FALSE)
  }

  long_a <- dataset_to_long(a)
  long_b <- dataset_to_long(b)

  long_match <- inner_join(long_a, long_b)

  if(any(nrow(long_a) != nrow(long_match), nrow(long_b) != nrow(long_match))) {
    warning("rows missmatch:",
      "\nextra rows in a: ", paste(anti_join(long_a, long_match), collapse = ", "),
      "\nextra rows in b: ", paste(anti_join(long_b, long_match), collapse = ", ")
    )
    return(FALSE)
  }

  return(TRUE)
}
