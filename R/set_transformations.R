#' Transform a dataset into wide format
#'
#' Converts the current dataset (long or decomposed) to wide format.
#' Use `x_axis` to specify the column that should become value columns.
#'
#' @param ds A dataset object.
#' @param x_axis Optional column name to use as value axis. If `NULL`, the stored x-axis is used.
#' @return A dataset in wide format.
#' @export
to_wide <- function(ds, x_axis = NULL) {
  dataset_transfrom(ds, "wide", x_axis = x_axis)
}

#' Transform a dataset into long format
#'
#' Converts a wide or decomposed dataset to long format, with one row per
#' combination of IDs and value column.
#'
#' @param ds A dataset object.
#' @return A dataset in long format.
#' @export
to_long <- function(ds) {
  dataset_transfrom(ds, "long", )
}

#' Decompose a dataset into a list of data frames
#'
#' Splits a wide or long dataset into a list of child data frames along
#' hierarchical ID paths, using the supplied decomposition strategy.
#'
#' @param ds A dataset object.
#' @param strategy A function that returns a tibble of hierarchical paths
#'   (default: [hirarchical_paths()]).
#' @param x_axis Optional column name for the value axis; if `NULL`, the
#'   stored x-axis is used.
#' @return A list of data frames (each inherits class `"dataset"`)
#'   in decomposed format.
#' @export
to_decomposed <- function(ds, strategy = hirarchical_paths, x_axis = NULL) {
  dataset_transfrom(ds, "decomposed", x_axis, strategy)
}

#' Convert a dataset to a given state
#'
#' Low-level function that dispatches transformation logic based on current
#' and target state. Usually you should prefer the user-friendly wrappers
#' [to_wide()], [to_long()], and [to_decomposed()].
#'
#' @param ds A dataset object.
#' @param to Target state: one of `"wide"`, `"long"`, or `"decomposed"`.
#' @param x_axis Optional column name for the value axis (only used when
#'   converting to wide or decomposed).
#' @param decompose_strategy Strategy for decomposition (only used when
#'   converting to decomposed format).
#' @return A dataset object in the requested state.
#' @export
dataset_transfrom <- function(
  ds,
  to,
  x_axis = NULL,
  decompose_strategy = hirarchical_paths
) {
  from <- state(ds)

  if (from == to) {
    return(ds)
  }

  transition <- paste0(from, "_to_", to)

  result <- switch(
    transition,
    "wide_to_long" = wide_to_long(ds),
    "wide_to_decomposed" = dataset_decompose(ds, strategy = decompose_strategy),
    "long_to_wide" = long_to_wide(ds, col = x_axis),
    "long_to_decomposed" = dataset_decompose(
      long_to_wide(ds, col = x_axis),
      strategy = decompose_strategy
    ),
    "decomposed_to_wide" = dataset_compose(ds),
    "decomposed_to_long" = wide_to_long(dataset_compose(ds)),
    stop("Unsupported transformation from ", from, " to ", to)
  )

  return(result)
}
