# after many attempts this seems to be an actually reasonable interface with merge_with and the
# internal structur seems to be testable and reliable. small enough for testing big enough to achieve more.
require(dplyr)

# NULL list elements appear for missing rows in full_join -> replace with NA
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

#' expects two datasets returns a dataset in long form with values from left and right
#' in respective columns. Specify the out column names with left_name, and right_name.
#' we could also see it as a dataet in wide format with a new colum e.g. source. this might
#' actually be nice for leapfrog trie join futur. but for now we consider it to be a fulty long
#' format as it will have two value columns. and no cloumn called value.
#' this should be small enough helper such that testing is possible
#' but usefull enough to dramatically simplyify other parts of the code
#' @internal
combine_datasets <- function(
  a,
  b,
  strict = c("equal"),
  left_name = "left",
  right_name = "right"
) {
  # The compare_ids() helper returns strings like "equal", "greater", "less", "missmatch"
  cmp <- compare_ids(ids(a), ids(b))

  if (!(cmp %in% strict)) {
    stop(
      "ids should be one of: ",
      paste(strict, collapse = ", "),
      " but is: ",
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

  # identify the column set that uniquely defines each row after the join
  new_ids <- union(ids(a), ids(b))

  set_attr(out, new_ids, x_axis = NULL, state = "scuffed_long")
}

#' Build a merging function that corresponds to the chosen set operation
#' and precedence rule.
#'
#' @param op       set operation name, one of "left", "right", "diff",
#'                 "xor", "and", "or".
#' @param prc      precedence rule, either "left" (prefer left when both
#'                 sides are present) or "right" (prefer right).
#'
#' @return A function `f(a, b)` that can be called inside `merg_helper`.
merge_func <- function(
  op = c("left", "right", "diff", "xor", "and", "or"),
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
    diff = function(a, b) if_else(!is.na(a) & is.na(b), prec_fn(a, b), NA),
    xor = function(a, b) if_else(xor(is.na(a), is.na(b)), prec_fn(a, b), NA),
    and = function(a, b) if_else(!is.na(a) & !is.na(b), prec_fn(a, b), NA),
    or = function(a, b) if_else(!is.na(a) | !is.na(b), prec_fn(a, b), NA)
  )
}

# this is a hollow helper, and other functions in here are more specific
# versions of this
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

#' merge with some options
#' @export
merge_with <- function(
  a,
  b,
  set_operation = c("left", "right", "diff", "xor", "and", "or"),
  precedence = c("left", "right"),
  strict = c("equal", "greater", "less"),
  keep = TRUE
) {
  merg_helper(
    a,
    b,
    merge_func = merge_func(set_operation, precedence),
    strict = strict,
    keep = keep
  )
}
