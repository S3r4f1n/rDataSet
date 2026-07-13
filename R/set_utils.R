require(dplyr)

#' Difference between two datasets
#'
#' Compares two datasets cell by cell and returns only rows where they differ.
#' Differences include rows present in only one dataset and rows where the
#' values differ.
#'
#' @param a A dataset object.
#' @param b A dataset object.
#' @param strict Character vector specifying allowed ID comparisons:
#'   `"equal"` (exact match), `"greater"` (IDs of `a` are superset), or
#'   `"less"` (IDs of `a` are subset). Default `"equal"`.
#' @return A dataset in wide format containing the differing rows.
#' @export
dataset_diff <- function(
  a,
  b,
  strict = "equal"
) {
  combine_datasets(a, b, strict = strict, "left", "right") %>%
    filter(xor(is.na(left), is.na(right)) | mapply(`!=`, left, right)) %>%
    set_attr(., ids(.), "source", state = "wide")
}

#' Filter a dataset
#'
#' Applies a conjunction of **dplyr** filter expressions to the dataset.
#' Operates on the internal long representation but returns the result in
#' the original state.
#'
#' @param ds A dataset object.
#' @param ... Filter expressions evaluated in the context of the long-form
#'   data.
#' @return A filtered dataset in the same state as `ds`.
#' @export
ds_filter <- function(ds, ...) {
  dots <- enquos(...)

  long <- to_long(ds)
  filtered <- filter(long, !!!dots) |>
    set_attr(ids(ds), x_axis(ds), state(ds)) |>
    dataset_collapse()
  dataset_transfrom(filtered, state(ds), x_axis(ds))
}

#' Select a subset of identifier columns
#'
#' Keeps only the rows associated with the specified identifier columns,
#' aggregating their values accordingly. Remaining identifiers are dropped.
#'
#' @param ds A dataset object.
#' @param ... Identifier column names, passed as unquoted expressions
#'   (tidy evaluation).
#' @return A dataset containing only the selected identifiers, in the same
#'   state as `ds`.
#' @export
select_ids <- function(ds, ..., ids = NULL) {
  if (!is.null(ids)) {
    if (is_dataset(ds)) {
      warning(
        "ids have been specified and datasets have been provided. only specified ids are used"
      )
    }
    ds <- dataset_build(ds, ids)
  }

  dots <- enquos(...)
  ids_selected <- select(select(ds, all_of(id_cols(ds))), !!!dots) |> names()

  decomp <- to_decomposed(ds, strategy = selected_paths_builder(ids_selected))
  selected <- dataset_get_composed(decomp, 1)
  dataset_transfrom(selected, state(ds))
}

#' Intersect with another dataset
#'
#' Keeps only the cells in `a` for which there is a corresponding non‑`NA`
#' cell in `b`. Uses left precedence and equal or greater ID requirement.
#'
#' @param a A dataset object.
#' @param b A dataset object.
#' @return A dataset containing intersection values from `a`.
#' @export
intersect_with <- function(a, b, ids = NULL) {
  merge_with(
    a,
    b,
    set_op = "intersect",
    prec = "left",
    strict = c("equal", "greater"),
    keep = FALSE,
    ids = ids
  )
}

#' Mask a dataset with another
#'
#' Keeps only the cells in `a` that are also present in `b`. Optionally
#' restricts the result to the original value locations of `a` (framed).
#'
#' @param a A dataset object (the data to be masked).
#' @param b A dataset object (the mask).
#' @param framed Logical. If `TRUE`, ensures the result only contains
#'   values for which `a` has entries. Default `TRUE`.
#' @return A masked dataset.
#' @export
mask_with <- function(a, b, framed = TRUE, ids = NULL) {
  merged <- merge_with(
    a,
    b,
    set_op = "union",
    prec = "right",
    strict = c("equal", "greater"),
    ids = ids
  )

  if (framed) {
    frame <- to_wide(a) |> dataset_frame()
    merged |> intersect_with(frame)
  } else {
    merged
  }
}

#' Fill missing values from another dataset
#'
#' Replaces `NA` cells in `a` with values from `b`. Optionally keeps the
#' original value locations (framed).
#'
#' @param a A dataset object (to be filled).
#' @param b A dataset object (source of fill values).
#' @param framed Logical. If `TRUE`, ensures the result only contains
#'   values for which `a` has entries. Default `TRUE`.
#' @return A filled dataset.
#' @export
fill_with <- function(a, b, framed = TRUE, ids = NULL) {
  merged <- merge_with(
    a,
    b,
    set_op = "union",
    prec = "left",
    strict = c("equal", "greater"),
    ids = ids
  )

  if (framed) {
    frame <- to_wide(a) |> dataset_frame()
    merged |> intersect_with(frame)
  } else {
    merged
  }
}
