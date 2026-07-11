# this is a hollow helper, and other functions in here are more specific
# versions of this
require(dplyr)

# @todo test suite

merg_helper <- function(
  a,
  b,
  join_func,
  merge_func,
  filter_func,
  strict_func,
  default_value = NA
) {
  if (!is.null(strict_func)) {
    ids_a <- ids(a)
    ids_b <- ids(b)
    add_a <- setdiff(ids_a, ids_b)
    add_b <- setdiff(ids_b, ids_a)
    strict_func(add_a, add_b)
  }

  long_a <- to_long(a) |> rename(left_dataset_super_long_name_value = "value")
  long_b <- to_long(b) |> rename(right_dataset_super_long_name_value = "value")

  replace_nulls_with_na <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    # fast: only touches NULL elements
    i <- which(lengths(x) == 0) # NULLs are length 0
    if (length(i)) {
      x[i] <- list(NA)
    }
    x
  }

  out <- join_func(long_a, long_b) %>% # we need dplyr pipe here
    mutate(
      value = merge_func(
        replace_nulls_with_na(left_dataset_super_long_name_value),
        replace_nulls_with_na(right_dataset_super_long_name_value)
      )
    ) |>
    select(-matches("_dataset_super_long_name_value$")) |>
    filter_func()

  out |>
    set_attr(setdiff(names(out), "value"), NULL, "long") |>
    dataset_transfrom(state(a), x_axis(a))
}

#' mask - replace values in 'a' with values from 'b' where 'b' has values
mask_with <- function(a, b) {
  merg_helper(
    a,
    b,
    join_func = left_join,
    merge_func = function(a, b) if_else(is.na(b), a, b), # right
    filter_func = function(ds) ds,
    strict_func = function(add_a, add_b) {
      if (length(add_b) > 0) {
        stop(paste0(
          "right can't have additional ids. has: ",
          paste(add_b, collapse = ", ")
        ))
      }
    }
  )
}

#' fill - fill missing values in 'a' with values from 'b'
fill_with <- function(a, b) {
  merg_helper(
    a,
    b,
    join_func = left_join,
    merge_func = function(a, b) if_else(is.na(a), b, a), # left
    filter_func = function(ds) ds,
    strict_func = function(add_a, add_b) {
      if (length(add_b) > 0) {
        stop(paste0(
          "right can't have additional ids. has: ",
          paste(add_b, collapse = ", ")
        ))
      }
    }
  )
}

#' merge with some options
#' @todo work on the verbs here
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
      if (length(add_a) > 0 || length(add_b) > 0) {
        stop(paste0(
          "ids mismatch:\nadditional a: ",
          paste(add_a, collapse = ", "),
          "\nadditional b: ",
          paste(add_b, collapse = ", ")
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

  mode <- mode[[1]]
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

#' Combine datasets with custom merge function
combine_datasets <- function(a, b, merge_func, collapse = TRUE, strict = TRUE) {
  strict_func <- if (strict) {
    function(add_a, add_b) {
      if (length(add_a) > 0 || length(add_b) > 0) {
        stop(paste0(
          "ids mismatch:\nadditional a: ",
          paste(add_a, collapse = ", "),
          "\nadditional b: ",
          paste(add_b, collapse = ", ")
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

  merg_helper(
    a,
    b,
    join_func = full_join,
    merge_func = merge_func,
    filter_func = filter_func,
    strict_func = strict_func
  )
}
