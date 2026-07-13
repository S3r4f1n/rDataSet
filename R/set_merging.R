require(dplyr)

replace_nulls_with_na <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  i <- which(lengths(x) == 0)
  if (length(i)) {
    x[i] <- list(NA)
  }
  x
}

#' Combine two datasets
#'
#' Joins the long forms of two datasets into a single intermediate table.
#'
#' @param a A dataset object.
#' @param b A dataset object.
#' @param strict Strictness of ID matching.
#' @param left_name Name for left values.
#' @param right_name Name for right values.
#' @return A dataframe in the internal `"scuffed_long"` format.
combine_datasets <- function(
  a,
  b,
  strict = c("equal"),
  left_name = "left",
  right_name = "right"
) {
  cmp <- compare_ids(ids(a), ids(b))

  if (!(cmp %in% strict)) {
    stop(
      "ids should be ",
      paste(strict, collapse = " or "),
      ". But is: ",
      cmp,
      "\nids a: ",
      paste(ids(a), collapse = ", "),
      "\nids b: ",
      paste(ids(b), collapse = ", ")
    )
  }

  long_a <- to_long(a) |>
    rename_with(\(x) left_name, all_of("value"))

  long_b <- to_long(b) |>
    rename_with(\(x) right_name, all_of("value"))

  out <- full_join(long_a, long_b, by = intersect(ids(a), ids(b))) |>
    mutate(
      !!left_name := replace_nulls_with_na(.data[[left_name]]),
      !!right_name := replace_nulls_with_na(.data[[right_name]])
    )

  new_ids <- union(ids(a), ids(b))

  set_attr(out, new_ids, x_axis = NULL, state = "scuffed_long")
}

#' Build a merging function
#'
#' Creates the value-level logic for a merge operation.
#'
#' @param op Operation name.
#' @param prc Precedence rule.
#' @return A function that takes two value vectors and returns the merged
#'   vector.
merge_func <- function(
  op = c("left", "right", "setdiff", "symdiff", "intersect", "union"),
  prc = c("left", "right")
) {
  op <- match.arg(op)
  prc <- match.arg(prc)

  prec_fn <- switch(
    prc,
    left = function(a, b) if_else(is.na(a), b, a),
    right = function(a, b) if_else(is.na(b), a, b)
  )

  switch(
    op,
    left = function(a, b) if_else(!is.na(a), prec_fn(a, b), NA),
    right = function(a, b) if_else(!is.na(b), prec_fn(a, b), NA),
    setdiff = function(a, b) if_else(!is.na(a) & is.na(b), prec_fn(a, b), NA),
    symdiff = function(a, b) {
      if_else(xor(is.na(a), is.na(b)), prec_fn(a, b), NA)
    },
    intersect = function(a, b) {
      if_else(!is.na(a) & !is.na(b), prec_fn(a, b), NA)
    },
    union = function(a, b) if_else(!is.na(a) | !is.na(b), prec_fn(a, b), NA)
  )
}

merg_helper <- function(
  a,
  b,
  merge_func,
  strict = c("equal"),
  keep = TRUE,
  left_name = "left_unlikely_long_name",
  right_name = "right_unlikely_long_name"
) {
  out <- combine_datasets(a, b, strict, left_name, right_name) |>
    mutate(value = merge_func(.data[[left_name]], .data[[right_name]])) |>
    select(-all_of(c(left_name, right_name)))

  out <- if (keep) {
    mutate(out, value = replace_nulls_with_na(out$value))
  } else {
    filter(out, !is.na(replace_nulls_with_na(out$value)))
  }

  out |>
    set_attr(setdiff(names(out), "value"), NULL, "long") |>
    dataset_transfrom(state(a), x_axis(a))
}

#' Merge two datasets with a set operation
#'
#' Combines two datasets using a flexible set operation (left, right,
#' setdiff, symdiff, intersect, union) and a chosen precedence rule. ID
#' matching strictness controls which datasets are permissible.
#'
#' @param a A dataset object.
#' @param b A dataset object.
#' @param set_op Set operation to apply: one of `"left"`, `"right"`,
#'   `"setdiff"`, `"symdiff"`, `"intersect"`, or `"union"`.
#' @param prec Precedence rule when ID columns differ: `"left"` gives
#'   priority to values from `a`, `"right"` to values from `b`.
#' @param strict Character vector specifying allowed ID comparisons:
#'   `"equal"`, `"greater"`, `"less"`. Default
#'   `c("equal","greater","less")` allows all three.
#' @param keep Logical. If `FALSE`, drops rows where the result value is
#'   `NA`. Default `FALSE`.
#' @return A dataset object resulting from the merge.
#' @export
merge_with <- function(
  a,
  b,
  set_op = c("left", "right", "setdiff", "symdiff", "intersect", "union"),
  prec = c("left", "right"),
  strict = c("equal", "greater", "less"),
  keep = FALSE,
  ids = NULL
) {
  # build datsets on the fly
  if (!is.null(ids)) {
    if (is_dataset(a) || is_dataset(b)) {
      warning(
        "ids have been specified and datasets have been provided. only specified ids are used"
      )
    }
    a <- dataset_build(a, ids)
    b <- dataset_build(b, ids)
  }

  merg_helper(
    a,
    b,
    merge_func = merge_func(set_op, prec),
    strict = strict,
    keep = keep
  )
}
