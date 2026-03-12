#' @Document This is a package to treat Data Sets as mathematical sets. Or that is where the inspiration comes from.
#' The basic idea is to define a union, a setminus and a set intersection for data sets. 
#' Further we think of Data sets, as a Data Frame (Tibble), with an addition of id cols.
#' The id cols are similar to these you would expect in a sql datbase but with th further restriction
#' that set operations are only possible on one-to-one relations. This is a deliberate choice to reduce
#' surprising and confusing behaviour. If you want to expand a data set this should be done explicitly with e.g.
#' a join, and not implicitly in a set operation.
#'
#' The idea is that each value is identified with a row_id (select cols) and col_id (column name). And a vlue is either
#' present or abscent (is.na).
#' Lets say we have data sets A and B
#' the setminus A - B. Is the set A without the values which are also defined in B. E.g. if for the same row_id and col_id
#' values are present in bot data sets A and B. It wont be contained in the output. If it is only present in B. It won't
#' be in the output. Only if it is only in A it is in the output.
#' 
#' the set intersection A > B. Now only values which are present in both sets are in the output. More over values will be taken from A.
#' B in this case is just a filter which decides which values of A are in the output
#'
#' the set union A + B. Returns all the values which are either present in A or in B. For values which are present in both
#' the values are taken from A
#'
#' the set equality A == B. Is only defined on sets with the same ids. It throws an error if this missmatches. Further
#' it returns a dataset of turth values. For each cell. This can be used to find missmatches in data.
#'
#' ids(A) returns the id cols of A
#' vals(A) returns the value cols of A (everything else)
#' dataset_build(df, ids) creates a dataset with the data from the df and the id cols defined in ids (vec of names).

library(dplyr)

# this is heavly overbloated code, but it works and has little custom functions. Mostly the sanity checks are bloated
# and some merging, and some filtering, and some whatever. I guess a propper implementation should use a C or Rust backend
# tough i'm not sure if base R already gives the tools to do this basically at C time.
dataset_build <- function(df, ids){
  if(!is.character(ids)) stop("ids must be vector of character")
  if(!is.data.frame(df)) stop("the df must be a dataframe")
  attr(df, "dataset_ids") <- ids

  class(df) <- c("dataset", class(df))
  dataset_integrity(df)
}

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

vals <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(-all_of(ids))
}

ids <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(all_of(ids))
}

# updating the arithmetics
#' @export
"-.dataset" <- function(a, b) dataset_minus(a,b)
#' @export
">.dataset" <- function(a, b) dataset_intersect(a,b)
#' @export
"+.dataset" <- function(a, b) dataset_union(a,b)
#' @export
"==.dataset" <- function(a, b) dataset_equality(a,b)

# examples
# A <- dataset_build(tibble(i = 1:10, b = if_else(1:10 %% 2 == 0, NA, 1:10)), "i")
# B <- dataset_build(tibble(i = 1:10, b = na_if(1:10, 3)), "i")
# C <- dataset_build(tibble(i = 1:5, b = 10:6, c = 2), "i")

# A - B
