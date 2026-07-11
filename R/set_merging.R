# some restriction on the ids, e,g reduce the set in its reach down to another.
# ok this is a place where i get consistently confused. so it might be reasonable to write
# a theretic work on this issue. but i'm pretty sure this is true. by restricting ids and
# by using the conditionals on the merg function we do have all options of full join inner join and so on.
# i think the actual helpful mind frameworkt is the set ven diagram, with intersectiona nd so on. and we actualy
# get all the options when imagine the left right cols, we have only left, only right and intersection.
#
# idea for later, from the long format the set operations make a lot of snese. but in the wide format we might
# want to get a frame, e.g. a set which coverst the whole x-y-dimesnion to such that we can do a union and then
# reduce it back to the frame. like this we get a merge within the frae but beyond each of the sets.
#
# @todo test suite
require(dplyr)

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

  out <- full_join(long_a, long_b) |>
    mutate(
      !!left_name := replace_nulls_with_na(.data[[left_name]]),
      !!right_name := replace_nulls_with_na(.data[[right_name]])
    )

  # identify the column set that uniquely defines each row after the join
  new_ids <- setdiff(names(out), c(left_name, right_name))

  set_attr(out, new_ids, x_axis = NULL, state = "scuffed_long")
}

# thiese are some small helpers, which really should make code more readable
left_pred <- function(a, b) if_else(is.na(a), b, a)
right_pred <- function(a, b) if_else(is.na(b), a, b)

# Wrappers that enforce a “side” (left or right) and then apply a precedence function
set_left <- function(precedence) {
  function(a, b) if_else(is.na(a), NA, precedence(a, b))
}
set_right <- function(precedence) {
  function(a, b) if_else(is.na(b), NA, precedence(a, b))
}
set_or <- function(precedence) {
  function(a, b) {
    if_else(!is.na(a) | !is.na(b), precedence(a, b), NA)
  }
}
set_xor <- function(precedence) {
  function(a, b) {
    if_else(xor(is.na(a), is.na(b)), precedence(a, b), list(NA))
  }
}
set_and <- function(precedence) {
  function(a, b) {
    if_else(!is.na(a) & !is.na(b), precedence(a, b), list(NA))
  }
}
set_diff <- function(precedence) {
  function(a, b) {
    if_else(!is.na(a) & is.na(b), precedence(a, b), list(NA))
  }
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

  prec_fn <- switch(prc, left = left_pred, right = right_pred)

  set_op <- switch(
    op,
    left = set_left,
    right = set_right,
    diff = set_diff,
    xor = set_xor,
    and = set_and,
    or = set_or
  )

  set_op(prec_fn)
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
    mutate(!!value := merge_func(.data[[left_name]], .data[[right_name]])) |>
    select(-all_of(c(left_name, right_name)))

  out <- if (keep) out else filter(out, !is.na(out$value))

  out |>
    set_attr(setdiff(names(out), "value"), NULL, "long") |>
    dataset_transfrom(state(a), x_axis(a))
}

#' mask - replace values in 'a' with values from 'b' where 'b' has values
#' @export
mask_with <- function(a, b) {
  merg_helper(
    a,
    b,
    merge_func = right_pred,
    strict = c("equal", "greater"),
    keep = TRUE
  )
}

#' fill - fill missing values in 'a' with values from 'b'
#' @export
fill_with <- function(a, b) {
  merg_helper(
    a,
    b,
    merge_func = left_pred,
    strict = c("equal", "greater"),
    keep = TRUE
  )
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
