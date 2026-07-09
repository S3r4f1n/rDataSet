# Utility functions for dataset operations
library(dplyr)

dataset_diff <- function(a, b) {
  if (identical(a, b)) return(NULL)
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
ds_select <- function(ds, ..., strict = TRUE) {
  dots <- enquos(...)
  selection <- select(ds, !!!dots) |> names()
  ids <- intersect(selection, id_cols(ds))

  if (strict && !all(selection %in% id_cols(ds))) {
    stop("Only ID columns can be selected when strict = TRUE")
  }
  decomp <- to_decomposed(ds, strategy = selected_paths_builder(ids))
  selected <- dataset_get_composed(decomp, 1)
  dataset_transfrom(selected, state(ds))
}

#' summarise by groups, analogous to dplyr::summarise
ds_summarise <- function(ds, ..., by) {
  if (missing(by) || length(by) == 0) {
    stop("`by` must be a character vector specifying grouping columns")
  }
  idc <- id_cols(ds)
  if (!all(by %in% idc)) {
    stop("`by` must be a subset of the dataset's ID columns")
  }

  long <- to_long(ds)
  grouped <- dplyr::group_by(long, dplyr::across(dplyr::all_of(by)), variable)

  summarised <- dplyr::summarise(grouped, ..., .groups = "drop")

  summary_cols <- base::setdiff(names(summarised), c(by, "variable"))
  if (length(summary_cols) == 0) {
    stop("`...` must produce at least one summary column")
  }

  wide <- tidyr::pivot_wider(
    summarised,
    id_cols = dplyr::all_of(by),
    names_from  = variable,
    values_from = dplyr::all_of(summary_cols)
  )

  dataset_build(wide, by)
}
