require(dplyr)

# @todo handle non character ids
#
wide_to_long <- function(dataset) {
  if (state(dataset) != "wide") {
    stop(
      "dataset in wide form is expected but attr(dataset, 'dataset_state') is: ",
      state(dataset)
    )
  }

  if (is_empty_set(dataset)) {
    attr(dataset, "dataset_state") <- "long"
    return(dataset)
  }

  # conversion
  x_axis <- x_axis(dataset)

  long <- dataset %>%
    tidyr::pivot_longer(val_cols(dataset), values_transform = as.list) %>%
    rename_with(~x_axis, name)

  # setting metadata
  set_attr(long, ids(dataset), NULL, "long")
}

long_to_wide <- function(dataset, col = NULL) {
  if (state(dataset) != "long") {
    stop(
      "dataset in long form is expected but attr(dataset, 'dataset_state') is: ",
      state(dataset)
    )
  }

  if (is_empty_set(dataset)) {
    attr(dataset, "dataset_state") <- "wide"
    return(dataset)
  }

  ids <- ids(dataset)
  valc <- val_cols(dataset)
  x_axis <- if (is.null(col)) ids[length(ids)] else col

  if (length(valc) != 1) {
    stop(
      "a data set in long form is expected to only have one, vals column"
    )
  }

  # conversion
  wide <- dataset %>%
    tidyr::pivot_wider(
      names_from = all_of(x_axis),
      values_from = all_of(valc),
      values_fill = list(NA)
    ) %>%
    mutate(across(
      -all_of(setdiff(ids, x_axis)),
      ~ purrr::list_simplify(., strict = FALSE)
    ))

  # setting metadata
  set_attr(wide, ids, x_axis, "wide")
}
