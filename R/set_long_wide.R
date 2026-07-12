require(dplyr)

#' Widen to long conversion
#'
#' Converts a dataset from wide format (multiple value columns) to long
#' format (one row per value), preserving ID columns and the x-axis
#' information.
#'
#' @param dataset A dataset object in wide format.
#' @return A dataset object in long format.
#' @export
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

  x_axis <- x_axis(dataset)

  long <- dataset %>%
    tidyr::pivot_longer(val_cols(dataset), values_transform = as.list) %>%
    rename_with(~x_axis, name)

  set_attr(long, ids(dataset), NULL, "long")
}

#' Long to wide conversion
#'
#' Converts a dataset from long format (rows with values) back to wide
#' format where each combination of identifiers maps to one row.
#'
#' @param dataset A dataset object in long format.
#' @param col The column to use as the x-axis (value column names). If
#'   `NULL`, the last ID column is used.
#' @return A dataset object in wide format.
#' @export
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

  set_attr(wide, ids, x_axis, "wide")
}

#' Obtain the logical frame of a wide dataset
#'
#' Creates a copy of the dataset where every value is replaced by `TRUE`.
#' Useful for masking or preserving the original cell locations.
#'
#' @param a A dataset object in wide format.
#' @return A dataset with all values set to `TRUE`.
#' @export
dataset_frame <- function(a) {
  if (state(a) != "wide") {
    stop(paste0("Dataset must be in wide format. is: ", state(a)))
  }
  a |> mutate(across(val_cols(a), \(x) TRUE))
}
