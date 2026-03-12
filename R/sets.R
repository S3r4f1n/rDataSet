#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import purrr
NULL

#' Build a Dataset Object
#'
#' Creates a dataset object from a data frame by designating specific columns as ID columns.
#' ID columns must uniquely identify each row (one-to-one relation).
#'
#' @param df A data frame containing the data.
#' @param ids A character vector of column names to use as row identifiers.
#' @return A dataset object (S3 class "dataset") with the `dataset_ids` attribute set.
#' @export
#' @examples
#' df <- tibble::tibble(i = 1:5, value = c(10, NA, 30, NA, 50))
#' ds <- dataset_build(df, ids = "i")
dataset_build <- function(df, ids){
  if(!is.character(ids)) stop("ids must be vector of character")
  if(!is.data.frame(df)) stop("the df must be a dataframe")
  attr(df, "dataset_ids") <- ids

  class(df) <- c("dataset", class(df))
  dataset_integrity(df)
}

#' Validate Dataset Integrity
#'
#' Internal function to verify that a dataset object is valid.
#' Checks that the ID columns uniquely identify each row.
#'
#' @param dataset A dataset object to validate.
#' @return The input dataset (invisibly) if valid.
#' @details If the dataset is invalid.
#' @keywords internal
dataset_integrity <- function(dataset){
  if(!"dataset" %in% class(dataset)) stop("dataset must be of instance dataset")
  ids <- attr(dataset, "dataset_ids")
  dim <- dataset %>% nrow()
  dimB <- dataset %>%
    select(all_of(ids)) %>%
    unique() %>%
    nrow()

  if(dim != dimB) stop("the row ids do not uniqley identfy the rows")
  dataset
}

#' Set Difference
#'
#' Returns values from dataset `a` that are not present in dataset `b`.
#' A value is considered present if it is not `NA`.
#'
#' **Row/Column Handling:**
#' - **Rows:** All rows from `a` are retained (left join on ID columns)
#' - **Columns:** Only value columns that exist in both datasets are considered; ID columns must match
#'
#' **Value Operation:**
#' - For each cell at (row_id, col_id), the value from `a` is kept only if the corresponding cell in `b` is `NA`
#'
#' @param a A dataset object (minuend).
#' @param b A dataset object (subtrahend).
#' @return A dataset containing values from `a` where corresponding values in `b` are `NA`.
#' @details Error if ID columns of `a` and `b` do not match.
#' @keywords internal
dataset_minus <- function(a, b) {
  id_a <- attr(a, "dataset_ids")
  id_b <- attr(b, "dataset_ids")
  if(length(intersect(id_a, id_b)) != length(id_a)) stop("ids don't match", "\n id a: ", id_a, "\n id b: ", id_b)
  if(any(intersect(intersect(id_a, names(a)), names(b)) != id_a)) stop("some ids are missing in dataset. Ids: ", id_a)

  ids <- id_a
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- left_join(
    a %>% rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>% select(all_of(c(ids, common))) %>% rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids
  )
  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(is.na(y), x, NA)
      )
    ) %>%
    rename_with(function(x) sub("_dataset_a_ending$", "", x), matches("_dataset_a_ending$")) %>%
    select(-matches("_dataset_b_ending$"))
  
  class(out) <- c("dataset", class(out))
  dataset_integrity(out)
}

#' Set Intersection
#'
#' Returns values from dataset `a` that are also present in dataset `b`.
#' A value is considered present if it is not `NA`. Dataset `b` acts as a filter
#' determining which values from `a` appear in the result.
#'
#' **Row/Column Handling:**
#' - **Rows:** Only rows present in both `a` and `b` (inner join on ID columns)
#' - **Columns:** Only value columns that exist in both datasets; ID columns must match
#'
#' **Value Operation:**
#' - For each cell at (row_id, col_id), the value from `a` is kept only if the corresponding cell in `b` is not `NA`
#'
#' @param a A dataset object (left operand).
#' @param b A dataset object (right operand, used as filter).
#' @return A dataset containing values from `a` where corresponding values in `b` are not `NA`.
#' @details Error if ID columns of `a` and `b` do not match.
#' @keywords internal
dataset_intersect <- function(a, b) {
  id_a <- attr(a, "dataset_ids")
  id_b <- attr(b, "dataset_ids")
  if(length(intersect(id_a, id_b)) != length(id_a)) stop("ids don't match", "\n id a: ", id_a, "\n id b: ", id_b)
  if(any(intersect(intersect(id_a, names(a)), names(b)) != id_a)) stop("some ids are missing in dataset. Ids: ", id_a)

  ids <- id_a
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- inner_join(
    a %>% select(all_of(c(ids, common))) %>% rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>% select(all_of(c(ids, common))) %>% rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids
  )

  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(!is.na(y), x, NA)
      )
    ) %>%
    rename_with(function(x) sub("_dataset_a_ending$", "", x), matches("_dataset_a_ending$")) %>%
    select(-matches("_dataset_b_ending$"))
  
  class(out) <- c("dataset", class(out))
  dataset_integrity(out)
}

#' Set Union
#'
#' Returns all values from either dataset `a` or dataset `b`.
#' For cells present in both datasets, the value from `a` takes precedence.
#'
#' **Row/Column Handling:**
#' - **Rows:** All rows from both `a` and `b` (full join on ID columns)
#' - **Columns:** All value columns from both datasets; ID columns must match
#'
#' **Value Operation:**
#' - For each cell at (row_id, col_id), uses the value from `a` if present; otherwise falls back to `b`
#' - A value is considered present if it is not `NA`
#'
#' @param a A dataset object (left operand, takes precedence).
#' @param b A dataset object (right operand).
#' @return A dataset containing values from `a` where available, otherwise from `b`.
#' @details Error if ID columns of `a` and `b` do not match.
#' @keywords internal
dataset_union <- function(a, b) {
  id_a <- attr(a, "dataset_ids")
  id_b <- attr(b, "dataset_ids")
  if(length(intersect(id_a, id_b)) != length(id_a)) stop("ids don't match", "\n id a: ", id_a, "\n id b: ", id_b)
  if(any(intersect(intersect(id_a, names(a)), names(b)) != id_a)) stop("some ids are missing in dataset. Ids: ", id_a)

  ids <- id_a
  common <- setdiff(intersect(names(a), names(b)), ids)
  merged <- full_join(
    a %>% rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>% rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids
  )

  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(is.na(x), y, x)
      )
    ) %>%
    rename_with(function(x) sub("_dataset_a_ending$", "", x), matches("_dataset_a_ending$")) %>%
    select(-matches("_dataset_b_ending$"))
  
  class(out) <- c("dataset", class(out))
  dataset_integrity(out)
}

#' Set Equality
#'
#' Compares two datasets cell-by-cell and returns a dataset of logical values.
#'
#' **Row/Column Handling:**
#' - **Rows:** Must match exactly - both datasets must have the same rows (by ID)
#' - **Columns:** Must match exactly - both datasets must have the same value columns
#' - Throws an error if rows or columns do not align
#'
#' **Value Operation:**
#' For each cell at (row_id, col_id):
#' - Returns `TRUE` if both cells have equal non-`NA` values
#' - Returns `FALSE` if one cell is `NA` and the other is not, or if values differ
#' - Returns `NA` if both cells are `NA`
#'
#' @param a A dataset object (left operand).
#' @param b A dataset object (right operand).
#' @return A dataset of logical values indicating cell-wise equality.
#' @details Error if ID columns do not match, or if datasets have different dimensions.
#' @keywords internal
dataset_equality <- function(a, b) {
  id_a <- attr(a, "dataset_ids")
  id_b <- attr(b, "dataset_ids")
  if(length(intersect(id_a, id_b)) != length(id_a)) stop("ids don't match", "\n id a: ", id_a, "\n id b: ", id_b)
  if(any(intersect(intersect(id_a, names(a)), names(b)) != id_a)) stop("some ids are missing in dataset. Ids: ", id_a)

  ids <- id_a
  common <- setdiff(intersect(names(a), names(b)), ids)
  if(any(setdiff(names(a), ids) != common)) stop("cols don't align", "\n cols left: ", names(a), "\n cols right: ", names(b))
  if(any(setdiff(names(b), ids) != common)) stop("cols don't align", "\n cols left: ", names(a), "\n cols right: ", names(b))
  merged <- full_join(
    a %>% rename_with(function(x) paste0(x, "_dataset_a_ending"), all_of(common)),
    b %>% rename_with(function(x) paste0(x, "_dataset_b_ending"), all_of(common)),
    by = ids
  )

  if(nrow(merged) != nrow(a)) stop("rows don't algin. nrow(left) > nrow(right): ", nrow(a) > nrow(b))
  if(nrow(merged) != nrow(b)) stop("rows don't algin. nrow(left) > nrow(right): ", nrow(a) > nrow(b))

  out <- merged %>%
    mutate(
      purrr::map2_dfc(
        across(matches("_dataset_a_ending$")),
        across(matches("_dataset_b_ending$")),
        function(x, y) if_else(is.na(x) | is.na(y), if_else(is.na(x) & is.na(y), NA, FALSE), x == y)
      )
    ) %>%
    rename_with(function(x) sub("_dataset_a_ending$", "", x), matches("_dataset_a_ending$")) %>%
    select(-matches("_dataset_b_ending$"))
  
  class(out) <- c("dataset", class(out))
  dataset_integrity(out)
}

#' Extract Value Columns
#'
#' Returns all non-ID columns from a dataset.
#'
#' @param dataset A dataset object.
#' @return A data frame containing only the value columns (non-ID columns).
#' @export
#' @examples
#' ds <- dataset_build(tibble::tibble(i = 1:3, value = c(10, NA, 30)), ids = "i")
#' vals(ds)
vals <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(-all_of(ids))
}

#' Extract ID Columns
#'
#' Returns the ID columns from a dataset.
#'
#' @param dataset A dataset object.
#' @return A data frame containing only the ID columns.
#' @export
#' @examples
#' ds <- dataset_build(tibble::tibble(i = 1:3, value = c(10, NA, 30)), ids = "i")
#' ids(ds)
ids <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(all_of(ids))
}

#' @export
#' @rdname dataset_minus
"-.dataset" <- function(a, b) dataset_minus(a,b)

#' @export
#' @rdname dataset_intersect
">.dataset" <- function(a, b) dataset_intersect(a,b)

#' @export
#' @rdname dataset_union
"+.dataset" <- function(a, b) dataset_union(a,b)

#' @export
#' @rdname dataset_equality
"==.dataset" <- function(a, b) dataset_equality(a,b)
