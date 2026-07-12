library(dplyr)

#' Dataset Difference
#' @param a A dataset object.
#' @param b A dataset object.
#' @param strict Strictness of ID matching.
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

#' Filter dataset
#' @param ds A dataset object.
#' @param ... Filter conditions.
#' @export
ds_filter <- function(ds, ...) {
  dots <- enquos(...)

  long <- to_long(ds)
  filtered <- filter(long, !!!dots) |>
    set_attr(ids(ds), x_axis(ds), state(ds)) |>
    dataset_collapse()
  dataset_transfrom(filtered, state(ds), x_axis(ds))
}

#' Select ID columns
#' @param ds A dataset object.
#' @param ... ID columns to select.
#' @export
select_ids <- function(ds, ...) {
  dots <- enquos(...)
  ids <- select(select(ds, all_of(id_cols(ds))), !!!dots) |> names()

  decomp <- to_decomposed(ds, strategy = selected_paths_builder(ids))
  selected <- dataset_get_composed(decomp, 1)
  dataset_transfrom(selected, state(ds))
}

#' Intersect with another dataset
#' @param a A dataset object.
#' @param b A dataset object.
#' @export
intersect_with <- function(a, b) {
  merge_with(
    a,
    b,
    set_op = "intersect",
    prec = "left",
    strict = c("equal", "greater"),
    keep = FALSE
  )
}

#' Mask with another dataset
#' @param a A dataset object.
#' @param b A dataset object.
#' @param framed Whether to frame the result.
#' @export
mask_with <- function(a, b, framed = TRUE) {
  merged <- merge_with(
    a,
    b,
    set_op = "union",
    prec = "right",
    strict = c("equal", "greater"),
  )

  if (framed) {
    frame <- to_wide(a) |> dataset_frame()
    merged |> intersect_with(frame)
  } else {
    merged
  }
}

#' Fill with another dataset
#' @param a A dataset object.
#' @param b A dataset object.
#' @param framed Whether to frame the result.
#' @export
fill_with <- function(a, b, framed = TRUE) {
  merged <- merge_with(
    a,
    b,
    set_op = "union",
    prec = "left",
    strict = c("equal", "greater"),
  )

  if (framed) {
    frame <- to_wide(a) |> dataset_frame()
    merged |> intersect_with(frame)
  } else {
    merged
  }
}
