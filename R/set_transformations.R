#' Transform a dataset into wide format
#'
#' Converts the current dataset (long or decomposed) to wide format.
#' Use `x_axis` to specify the column that should become value columns.
#'
#' This is a thin wrapper around [dataset_transfrom()] with `to = "wide"`.
#'
#' @param ds A dataset object.
#' @param x_axis Optional column name to use as value axis. If `NULL`, the stored x-axis is used.
#' @return A dataset in wide format.
#' @seealso [to_long()], [to_decomposed()], [dataset_transfrom()]
#' @examples
#' \dontrun{
#' library(rDataSet)
#' ds <- dataset_build(tibble::tibble(id = 1:2, value = c("a", "b")), ids = "id")
#' long <- to_long(ds)
#' wide <- to_wide(long)
#' }
#' @export
to_wide <- function(ds, x_axis = NULL) {
  dataset_transfrom(ds, "wide", x_axis = x_axis)
}

#' Transform a dataset into long format
#'
#' Converts a wide or decomposed dataset to long format, with one row per
#' combination of IDs and value column.
#'
#' This is a thin wrapper around [dataset_transfrom()] with `to = "long"`.
#'
#' @param ds A dataset object.
#' @return A dataset in long format.
#' @seealso [to_wide()], [to_decomposed()], [dataset_transfrom()]
#' @examples
#' \dontrun{
#' library(rDataSet)
#' ds <- dataset_build(tibble::tibble(id = 1:2, val1 = "a", val2 = "b"), ids = "id")
#' long <- to_long(ds)
#' }
#' @export
to_long <- function(ds) {
  dataset_transfrom(ds, "long", )
}

#' Decompose a dataset into a list of data frames
#'
#' Splits a wide or long dataset into a list of child data frames along
#' hierarchical ID paths, using the supplied decomposition strategy.
#'
#' This is a thin wrapper around [dataset_transfrom()] with `to = "decomposed"`.
#'
#' @param ds A dataset object.
#' @param strategy A function that returns a tibble of hierarchical paths
#'   (default: [hirarchical_paths()]).
#' @param x_axis Optional column name for the value axis; if `NULL`, the
#'   stored x-axis is used.
#' @return A list of data frames (each inherits class `"dataset"`)
#'   in decomposed format.
#' @seealso [hirarchical_paths()], [dataset_compose()], [dataset_transfrom()]
#' @examples
#' \dontrun{
#' library(rDataSet)
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:2, group = c("x","y"), value = 1:2),
#'   ids = c("id","group")
#' )
#' decomp <- to_decomposed(ds)
#' }
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
#' The function handles all six possible transitions between wide, long, and
#' decomposed formats. It automatically composes or decomposes datasets as
#' required, and can optionally set a new x-axis column when converting to
#' wide or decomposed format.
#'
#' @param ds A dataset object.
#' @param to Target state: one of `"wide"`, `"long"`, or `"decomposed"`.
#' @param x_axis Optional column name for the value axis (only used when
#'   converting to wide or decomposed).
#' @param decompose_strategy Strategy for decomposition (only used when
#'   converting to decomposed format).
#' @return A dataset object in the requested state.
#' @seealso [to_wide()], [to_long()], [to_decomposed()]
#' @examples
#' \dontrun{
#' library(rDataSet)
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:2, val1 = "a", val2 = "b"),
#'   ids = "id"
#' )
#' long <- dataset_transfrom(ds, "long")
#' wide <- dataset_transfrom(long, "wide")
#' }
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
