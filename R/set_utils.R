# Utility functions for dataset operations
library(dplyr)

# @todo test suite, wait with this as this is likely going to change in the futur
#
dataset_diff <- function(
  a,
  b,
  strict = "equal"
) {
  combine_datasets(a, b, strict = strict, "left", "right") %>%
    filter(xor(is.na(left), is.na(right)) | left != right) %>%
    set_attr(., ids(.), "source", state = "wide")
}

ds_filter <- function(ds, ...) {
  dots <- enquos(...)

  long <- to_long(ds)
  filtered <- filter(long, !!!dots) |>
    set_attr(ids(ds), x_axis(ds), state(ds)) |>
    dataset_collapse()
  dataset_transfrom(filtered, state(ds), x_axis(ds))
}

#' tidy select for ID columns only. if strict, error when non-ID columns selected
select_ids <- function(ds, ...) {
  dots <- enquos(...)
  ids <- select(select(ds, all_of(id_cols(ds))), !!!dots) |> names()

  decomp <- to_decomposed(ds, strategy = selected_paths_builder(ids))
  selected <- dataset_get_composed(decomp, 1)
  dataset_transfrom(selected, state(ds))
}


intersect_with <- function(a, b) {
  merge_with(
    a,
    b,
    set_operation = "right",
    precedence = "left",
    strict = c("equal", "greater"),
    keep = TRUE
  )
}

#' mask - replace values in a with values from b where b has values
#' @export
mask_with <- function(a, b, framed = TRUE) {
  merged <- merge_with(
    a,
    b,
    set_operation = "or",
    precedence = "left",
    strict = c("equal", "greater"),
    keep = TRUE
  )

  if (framed) {
    frame <- to_wide(a) |> dataset_frame()
    merged |> intersect_with(frame)
  } else {
    merged
  }
}

#' fill - fill missing values in a with values from b
#' @export
fill_with <- function(a, b, framed = TRUE) {
  merged <- merg_helper(
    a,
    b,
    merge_func = merge_func(op = "or", prc = "left"),
    strict = c("equal", "greater"),
    keep = TRUE
  )

  if (framed) {
    frame <- to_wide(a) |> dataset_frame()
    merged |> intersect_with(frame)
  } else {
    merged
  }
}
