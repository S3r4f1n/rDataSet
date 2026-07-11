# Utility functions for dataset operations
library(dplyr)

# @todo test suite
#
dataset_diff <- function(a, b) {
  if (identical(a, b)) {
    return(NULL)
  }
  long_a <- to_long(a) %>% rename(left = value)
  long_b <- to_long(b) %>% rename(right = value)

  int <- full_join(
    long_a,
    long_b,
    by = intersect(names(long_a), names(long_b)),
    keep = FALSE
  )
  diff <- int %>%
    filter(xor(is.na(left), is.na(right)) | left != right)
  diff
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
