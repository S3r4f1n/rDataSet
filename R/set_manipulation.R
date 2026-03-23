library(dplyr)

#' Filter Dataset by Logical Conditions on Values
#'
#' Filters a dataset based on logical conditions applied to its value columns.
#' The dataset is temporarily converted to long format for filtering, then
#' converted back to wide format.
#'
#' **Filtering Process:**
#' - Converts the dataset from wide to long format (one row per cell)
#' - Applies the provided filter conditions using dplyr semantics
#' - Converts the filtered result back to wide format
#'
#' @param data A dataset to filter.
#' @param ... Logical conditions for filtering (passed to `dplyr::filter()`).
#'   Conditions can reference the `value` and `variable` columns available in
#'   long format.
#' @return A filtered dataset in wide format containing only rows that satisfy
#'   the filter conditions.
#' @details The filtering operates on individual cells (long format), not entire
#'   rows. Use conditions that account for this cell-level operation.
#' @keywords internal
dataset_filter <- function(data, ...) {
  dots <- enquos(...)

  long <- dataset_to_long(data)
  filtered <- filter(long, !!!dots)
  dataset_to_wide(filtered)
}

#' Convert Dataset from Wide to Long Format
#'
#' Transforms a dataset from wide format (one row per entity, multiple value columns)
#' to long format (one row per cell, with `variable` and `value` columns).
#'
#' **Transformation:**
#' - Each value cell becomes a separate row
#' - Creates `variable` column containing the original column name
#' - Creates `value` column containing the cell value (as character)
#' - Adds `variable` as an additional ID column
#'
#' **State Transition:** `"wide"` → `"long"`
#'
#' @param dataset A dataset in wide format.
#' @return A dataset in long format with ID columns plus `variable` and `value` columns.
#' @details Requires the input dataset to be in "wide" state. All values are
#'   converted to character type. The long format is useful for cell-level
#'   operations and filtering.
#' @keywords internal
dataset_to_long <- function(dataset) {
  ids <- ids(dataset) %>% names()
  x_axis <- attr(dataset, "dataset_x_axis")

  if(attr(dataset, "dataset_state") != "wide") stop("dataset in wide form is expected but attr(dataset, 'dataset_state') is: ", attr(dataset, "dataset_state"))

  long <- dataset %>%
    tidyr::pivot_longer(-all_of(ids), values_transform = as.character) %>%
    rename_with(~ x_axis, name) %>%
    dataset_build(ids = c(ids, x_axis))

  attr(long, "dataset_state") <- "long"
  dataset_integrity(long)
}

#' Convert Dataset from Long to Wide Format
#'
#' Transforms a dataset from long format (one row per cell) back to wide format
#' (one row per entity with multiple value columns).
#'
#' **Transformation:**
#' - Pivots the `variable` column into separate value columns
#' - Uses the `value` column to populate cell values
#' - Removes `variable` from the ID columns
#'
#' **State Transition:** `"long"` → `"wide"`
#'
#' This is the inverse operation of `dataset_to_long()`.
#'
#' @param dataset A dataset in long format.
#' @return A dataset in wide format with the original structure restored.
#' @details Requires the input dataset to be in "long" state with exactly one
#'   value column named `value` and an ID column named `variable`. After
#'   conversion, the dataset is automatically collapsed to remove empty rows
#'   and columns.
#' @keywords internal
dataset_to_wide <- function(dataset, col = NULL) {
  ids <- ids(dataset) %>% names()
  vals <- vals(dataset) %>% names()

  x_axis <- if(is.null(col)) attr(dataset, "dataset_x_axis") else col

  if(attr(dataset, "dataset_state") != "long") stop("dataset in long form is expected but attr(dataset, 'dataset_state') is: ", attr(dataset, "dataset_state"))
  if(any(vals != "value")) stop("a data set in long form is expected to only have one, vals column named value")
  if(!x_axis %in% ids) stop("a data set in long form is expected to have a id column which is called by the x_axis here: ", x_axis)

  wide <- dataset %>%
    tidyr::pivot_wider(names_from = all_of(x_axis), values_from = value) %>%
    dataset_build(ids = setdiff(c(ids), x_axis))

  attr(wide, "dataset_x_axis") <- x_axis
  attr(wide, "dataset_state") <- "wide"
  dataset_collapse(wide)
}
