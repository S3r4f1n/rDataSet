require(dplyr)

# a dataset is a tibble with id cols
#
# internaly we store the formats, to implement format convertions
# long: ids | val colum
# wide: ids | val cols x id
# decomposed. list(decomposed)

dataset_build <- function(df, ids) {
  if (!is.character(ids)) {
    stop("ids must be vector of character")
  }
  if (!is.data.frame(df)) {
    stop("the df must be a dataframe")
  }

  set_attr(df, c(ids, "variable"), "variable", "wide", TRUE)
}

# helper to never forget setting some attributes
set_attr <- function(df, ids, x_axis, state, bloated = NULL) {
  attr(df, "dataset_ids") <- ids
  attr(df, "dataset_x_axis") <- x_axis
  attr(df, "dataset_state") <- state
  if (!is.null(bloated)) {
    attr(df, "dataset_bloated") <- bloated
  }
  class(df) <- c("dataset", class(df))

  df
}

empty_set <- function() {
  dataset_build(tibble(), character(0))
}

is_empty_set <- function(dataset) {
  nrow(dataset) == 0
}

x_axis <- function(dataset) {
  attr(dataset, "dataset_x_axis")
}

ids <- function(dataset) {
  attr(dataset, "dataset_ids")
}

state <- function(dataset) {
  attr(dataset, "dataset_state")
}

id_cols <- function(dataset) {
  setdiff(ids(dataset), x_axis(dataset))
}

val_cols <- function(dataset) {
  setdiff(names(dataset), id_cols(dataset))
}

empty_rows <- function(dataset) {
  valc <- val_cols(dataset)
  rowSums(is.na(dataset[valc])) > 0
}

empty_cols <- function(dataset) {
  valc <- val_cols(dataset)
  cols <- colSums(is.na(dataset[valc])) > 0
  c(rep(FALSE, id_cols()), cols)
}

empty_ids <- function(dataset) {
  # @todo actually complicated algo
}

dataset_collapse <- function(dataset) {
  df <- dataset[!empty_rows(dataset), !empty_cols(dataset)]
  attr(df, "dataset_bloated") <- FALSE
  df
}

id_integrity <- function(dataset) {
  idc <- id_cols(dataset)
  !any(duplicated(dataset[idc]))
}

print.dataset <- function(x, ...) {
  ids <- ids(x)
  val_cols <- val_cols(x)

  cat("Dataset\n")
  cat("IDs:", paste(ids, collapse = ", "), "\n")
  cat("Value columns:", paste(val_cols, collapse = ", "), "\n\n")

  NextMethod() # prints the tibble
  invisible(x)
}
