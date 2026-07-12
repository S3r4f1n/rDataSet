require(dplyr)
require(cli)


# a dataset is a tibble with id cols
#
# internaly we store the formats, to implement format convertions
# long: ids | val colum
# wide: ids | val cols x id
# decomposed. list(decomposed)

# @export
dataset_build <- function(df, ids) {
  if (!is.character(ids)) {
    stop(paste0("ids must be vector of character but is: ", typeof(ids)))
  }
  if (!is.data.frame(df)) {
    stop(paste0("the df must be a dataframe but is: ", class(ids)))
  }

  set_attr(df, c(ids, "variable"), "variable", "wide") |>
    dataset_valid()
}

# helper to never forget setting some attributes
set_attr <- function(df, ids, x_axis, state) {
  attr(df, "dataset_ids") <- ids
  attr(df, "dataset_x_axis") <- x_axis
  attr(df, "dataset_state") <- state
  class(df) <- union("dataset", class(df))

  df
}

# @export
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


dataset_collapse <- function(dataset) {
  df <- dataset[!empty_rows(dataset), ]
  df <- df[, !empty_cols(df)]
  set_attr(df, ids(dataset), x_axis(dataset), state(dataset))
}

id_integrity <- function(dataset) {
  idc <- id_cols(dataset)
  !any(duplicated(dataset[idc]))
}

compare_ids <- function(a, b) {
  m <- function(a) if (a > 0) "more" else "zero"
  m_a <- setdiff(a, b) |> length() |> m()
  m_b <- setdiff(b, a) |> length() |> m()
  switch(
    paste0(m_a, "_", m_b),
    more_more = "missmatch",
    more_zero = "greater",
    zero_more = "less",
    zero_zero = "equal",
  )
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

# @export
print.dataset <- function(x, ...) {
  cat("Dataset - State: ", cli::col_blue(state(x)), ", IDs | Values\n")

  idc <- cli::col_blue(paste(id_cols(x), collapse = ", "))
  valc <- cli::col_green(paste(val_cols(x), collapse = ", "))
  xaxi <- cli::col_red(paste(x_axis(x), collapse = ", "))

  cat("Cols: ", idc, " | ", valc, ", x-Axis: ", xaxi, "\n\n", sep = "")

  NextMethod() # prints the tibble
  invisible(x)
}
