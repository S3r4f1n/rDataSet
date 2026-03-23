library(dplyr)
#' @todo
#' first do perfect support for wide long and compes decomposed form (these three formats must be loss free)
#' - fix edge cases
#' second implement the arithemtics using the transformations
#' third implement storing and loading of datasets.
#' - flat, is this even needed? it is basically the same as decomposed. but it might be considered, the decomposed long form :)
#'
#' Description
#' - wide / long (note that ids, and the x axis build the ids over a dataset in the wide form
#'   in the long form the ids are all cols but the last which is the value col)
#' - compose / decomposed, note that the composed form is exactly the wide format. We might call wide / composed format the default
#'   in the decomposed form we have a collection of dense? tables. constant values are factored out as much as possible
#' 
#' @keywords internal
"_PACKAGE"

#' Build a Dataset from a Data Frame
#'
#' Constructs a dataset object from a regular data frame by setting the
#' ID columns and initializing the dataset attributes.
#'
#' **Initialization:**
#' - Sets the `dataset_ids` attribute to specify which columns are ID columns
#' - Sets the `dataset_state` attribute to "wide"
#' - Adds "dataset" to the class vector
#' - Automatically collapses the dataset to remove empty rows/columns
#'
#' @param df A data frame to convert into a dataset.
#' @param ids A character vector specifying the names of ID columns.
#' @return A dataset object with the specified ID columns.
#' @details The ID columns must uniquely identify each row. The function
#'   validates this constraint via `dataset_integrity()` during collapse.
#' @keywords internal
dataset_build <- function(df, ids){
  if(!is.character(ids)) stop("ids must be vector of character")
  if(!is.data.frame(df)) stop("the df must be a dataframe")

  attr(df, "dataset_ids") <- ids
  attr(df, "dataset_x_axis") <- "variable" # normally variables are placed on the x axis, but could also be another id
  attr(df, "dataset_state") <- "wide"
  class(df) <- c("dataset", class(df))

  dataset <- df
  dataset_collapse(dataset)
}

#' Collapse Dataset to Remove Empty Rows and Columns
#'
#' Removes all rows and columns that contain only `NA` values, ensuring
#' the dataset contains only actual data. This is a normalization step
#' that maintains the mathematical set semantics.
#'
#' **Collapsing Process:**
#' - Identifies rows that have at least one non-NA value in value columns
#' - Removes columns that contain only NA values
#' - Decomposes and recomposes the dataset to ensure canonical form
#' - Validates the result with `dataset_integrity()`
#'
#' **Empty Set Handling:** If the dataset contains no values at all,
#' returns an empty data frame with no ID columns (representing ∅).
#'
#' @param dataset A dataset to collapse.
#' @return A collapsed dataset with all empty rows and columns removed.
#' @details This function ensures that datasets are in canonical form for
#'   set operations. Two datasets representing the same set will have
#'   identical collapsed forms.
#' @keywords internal
dataset_collapse <- function(dataset) {
  ids <- ids(dataset) %>% names()

  non_empty <- vals(dataset) %>% {!is.na(.)} %>% rowSums() %>% {. > 0} %>% which()

  # dropping empty cols and empty rows and ids
  out <- if(length(non_empty) > 0){
    dataset %>%
      slice(non_empty) %>%
      select(where(~ any(!is.na(.)))) %>%
      dataset_decompose() %>%
      dataset_compose()
  } else {
    tmp <- dataset %>% select(-everything()) %>% filter(FALSE) # empty set
    attr(tmp, "dataset_ids") <- NULL
    tmp
  }

  collapsed_set <- out
  dataset_integrity(collapsed_set)
}

#' Validate Dataset Integrity
#'
#' Checks that a dataset satisfies the fundamental integrity constraints
#' required for set operations.
#'
#' **Integrity Checks:**
#' - Verifies the object has "dataset" class
#' - Confirms `dataset_state` attribute is valid ("wide", "long", or "decomposed")
#' - Ensures ID columns uniquely identify each row (no duplicate ID combinations)
#'
#' @param collapsed_set A dataset to validate.
#' @return The input dataset (invisibly) if all checks pass.
#' @throws An error if any integrity check fails.
#' @details This function is called automatically after dataset operations
#'   to ensure the result is a valid dataset. The uniqueness check compares
#'   the total row count against the count of unique ID combinations.
#' @keywords internal
dataset_integrity <- function(collapsed_set){
  if(!"dataset" %in% class(collapsed_set)) stop("dataset must be of instance dataset")
  state <- attr(collapsed_set, "dataset_state")
  if(!state %in% c("wide", "long", "decomposed")) stop("each dataset is either in long or wide format but attr(dataset, 'dataset_state') is: ", attr(collapsed_set, "dataset_state"))

  ids <- attr(collapsed_set, "dataset_ids")
  dim <- collapsed_set %>% nrow()
  dimB <- collapsed_set %>%
    select(all_of(ids)) %>%
    unique() %>%
    nrow()

  if(dim != dimB) stop("the row ids do not uniqley identfy the rows")
  collapsed_set
}


#' Extract Value Columns from Dataset
#'
#' Returns all non-ID columns (value columns) from a dataset.
#'
#' @param dataset A dataset object.
#' @return A data frame containing only the value columns.
#' @details Value columns are those not specified as ID columns during
#'   dataset construction. These columns contain the actual data values
#'   that participate in set operations.
#' @keywords internal
vals <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(-all_of(ids))
}

#' Extract ID Columns from Dataset
#'
#' Returns all ID columns from a dataset.
#'
#' @param dataset A dataset object.
#' @return A data frame containing only the ID columns.
#' @details ID columns define the row identity and are used for joining
#'   and comparing datasets. They are specified during dataset construction
#'   via the `dataset_build()` function.
#' @keywords internal
ids <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(all_of(ids))
}

