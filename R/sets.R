require(dplyr)

# @todo fix exports, add testing for wide / long, compose / decompose.
# @todo add some consolidating function in a xport.r ?
# @todo whatever we do with the arithmetic operations
# @todo whatever we do with serialize an deserialize (saving)

# a dataset is a tibble with id cols
#
# internaly we store the formats, to implement format convertions
# long: ids | val colum
# wide: ids | val cols x id
# decomposed. list(decomposed)

dataset_build <- function(df, ids) {
  if (!is.character(ids)) {
    stop(paste0("ids must be vector of character but is: ", typeof(ids)))
  }
  if (!is.data.frame(df)) {
    stop(paste0("the df must be a dataframe but is: ", class(ids)))
  }

  set_attr(df, c(ids, "variable"), "variable", "wide", TRUE) |>
    dataset_valid()
}

# helper to never forget setting some attributes
set_attr <- function(df, ids, x_axis, state, bloated = NULL) {
  attr(df, "dataset_ids") <- ids
  attr(df, "dataset_x_axis") <- x_axis
  attr(df, "dataset_state") <- state
  if (!is.null(bloated)) {
    # attr(df, "dataset_bloated") <- bloated # not used currently
  }
  class(df) <- union("dataset", class(df))

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
  rowSums(!is.na(dataset[valc])) == 0
}

empty_cols <- function(dataset) {
  valc <- val_cols(dataset)
  cols <- colSums(!is.na(dataset[valc])) == 0
  c(rep(FALSE, length(id_cols(dataset))), cols) # asumes ids come always first
}

empty_ids <- function(dataset) {
  # @todo actually complicated algo
}

dataset_collapse <- function(dataset) {
  df <- dataset[!empty_rows(dataset), ]
  df <- df[, !empty_cols(df)]
  set_attr(df, ids(dataset), x_axis(dataset), state(dataset), bloated = FALSE)
}

id_integrity <- function(dataset) {
  idc <- id_cols(dataset)
  !any(duplicated(dataset[idc]))
}

dataset_valid <- function(ds) {
  state <- state(ds)
  if (!state %in% c("wide", "long", "decomposed")) {
    stop("error invalid state: ", state)
  }
  if (!id_integrity(ds)) {
    stop("ids do not uniquely identify rows")
  }
  ds
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
