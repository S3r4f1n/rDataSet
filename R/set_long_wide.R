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
    dataset <- bind_cols(
      dataset,
      tibble(variable = character(0), value = list())
    )
    return(set_attr(dataset, ids(dataset), NULL, state = "long"))
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
    dataset <- select(dataset, -all_of(c("variable", "value")))
    return(set_attr(dataset, ids(dataset), "variable", "wide"))
  }

  ids <- ids(dataset)
  valc <- val_cols(dataset)
  x_axis <- if (is.null(col)) ids[length(ids)] else col

  if (length(valc) != 1) {
    stop(
      paste0(
        "a data set in long form is expected to only have one, vals column but hast: ",
        paste(valc, collapse = ",")
      )
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

# returns a dataset with all true values
# used to get the "frame" of the wide format
dataset_frame <- function(a) {
  if (state(a) != "wide") {
    stop(paste0("Dataset must be in wide format. is: ", state(a)))
  }
  a |> mutate(across(val_cols(a), \(x) TRUE))
}
