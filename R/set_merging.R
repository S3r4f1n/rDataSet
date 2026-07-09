# this is a hollow helper, and other functions in here are more specific
# versions of this
require(dplyr)
merg_helper <- function(a, b, join_func, merge_func, filter_func, strict_func) {
  if (!is.null(strict_func)) {
    ids_a <- ids(a)
    ids_b <- ids(b)
    add_a <- setdiff(ids_a, ids_b)
    add_b <- setdiff(ids_b, ids_a)
    strict_func(add_a, add_b)
  }

  long_a <- to_long(a) |> rename(left_super_long_unlikely_name = "value")
  long_b <- to_long(b) |> rename(right_super_long_unlikely_name = "value")

  out <- join_func(long_a, long_b) %>% # we need dplyr pipe here
    mutate(
      value = merge_func(
        left_super_long_unlikely_name,
        right_super_long_unlikely_name
      )
    ) |>
    select(-matches("_super_long_unlikely_name$")) |>
    filter_func()

  out |>
    set_attr(names(out), NULL, "long") |>
    dataset_transfrom(state(a), x_axis(a))
}

#' mask
mask_with <- function(a, b) {
  merg_helper(
    a,
    b,
    join_func = left_join,
    merge_func = function(a, b) if_else(is.na(b), a, b), # right
    filter_func = function(ds) ds,
    strict_func = function(add_a, add_b) {
      if (length(add_b) > 0) {
        stop(paste0("right can't have aditional ids. has: ", add_b))
      }
    }
  )
}

#' fill
fill_with <- function(a, b) {
  merg_helper(
    a,
    b,
    join_func = left_join,
    merge_func = function(a, b) if_else(is.na(a), b, a), # left
    filter_func = function(ds) ds,
    strict_func = function(add_a, add_b) {
      if (length(add_b) > 0) {
        stop(paste0("right can't have aditional ids. has: ", add_b))
      }
    }
  )
}

#' merge with some options
merge_by <- function(
  a,
  b,
  mode = c(
    "union_left",
    "intersect_left",
    "xor",
    "equal",
    "union_right",
    "intersect_right"
  ),
  collapse = TRUE,
  strict = TRUE
) {
  strict_func <- if (strict) {
    function(add_a, add_b) {
      if (length(a) > 0 || length(b) > 0) {
        stop(paste0(
          "ids missmatch:\nadditional a: ",
          add_a,
          "\nadditional b: ",
          add_b
        ))
      }
    }
  } else {
    NULL
  }

  filter_func <- if (collapse) {
    function(ds) filter(ds, is.na(ds$value))
  } else {
    function(ds) ds
  }

  mode <- match.arg(mode[[1]])
  merge_func <- switch(
    mode,
    union_right = function(a, b) if_else(is.na(b), a, b),
    union_left = function(a, b) if_else(is.na(a), b, a),
    equal = function(a, b) if_else(a == b, a, NA),
    xor = function(a, b) {
      if_else(xor(is.na(a), is.na(b)), if_else(is.na(a), b, a), NA)
    },
    intersect_right = function(a, b) if_else(is.na(a) & is.na(b), a, NA),
    intersect_left = function(a, b) if_else(is.na(a) & is.na(b), b, NA),
  )

  merg_helper(
    a,
    b,
    join_func = full_join,
    merge_func = merge_func,
    filter_func = filter_func,
    strict_func = strict_func
  )
}
