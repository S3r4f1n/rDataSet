# do things. not sure what i need here to be honest, maybe go further away and jsut call it diff
# for difference

library(dplyr)
dataset_diff <- function(a, b) {
  dataset_integrity(a)
  dataset_integrity(b)

  suppressWarnings(if(a == b) return(NULL))

  long_a <- dataset_to_long(a) %>% rename(left = value)
  long_b <- dataset_to_long(b) %>% rename(right = value)

  int <- full_join(long_a, long_b, by = intersect(names(long_a), names(long_b)), keep = FALSE)
  diff <- int %>%
    filter(xor(is.na(left), is.na(right)) | left != right)

  diff
}


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
  wide <- dataset_to_wide(filtered)
  dataset_collapse(wide)
}
