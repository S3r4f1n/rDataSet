library(dplyr)
#' @keywords internal
"_PACKAGE"

dataset_build <- function(df, ids){
  if(!is.character(ids)) stop("ids must be vector of character")
  if(!is.data.frame(df)) stop("the df must be a dataframe")

  attr(df, "dataset_ids") <- ids
  attr(df, "dataset_state") <- "wide"
  class(df) <- c("dataset", class(df))

  dataset <- df
  dataset_collapse(dataset)
}

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


vals <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(-all_of(ids))
}

ids <- function(dataset){
  ids <- attr(dataset, "dataset_ids")
  dataset %>% select(all_of(ids))
}

