# do things. not sure what i need here to be honest, maybe go further away and jsut call it diff
# for difference

library(dplyr)
#
dataset_diff <- function(a, b) {
  suppressWarnings(if (a == b) return(NULL))

  long_a <- to_long(a) %>% rename(left = value)
  long_b <- to_long(b) %>% rename(right = value)

  int <- full_join(
    long_a,
    long_b,
    by = intersect(names(long_a), names(long_b)),
    keep = FALSE
  )
  diff <- int %>%
    filter(xor(is.na(left), is.na(right)) | left != right)

  diff
}

# this is most likely useless no?
# i guess we can filter on all values and
# retain ids
ds_filter <- function(ds, ...) {
  dots <- enquos(...)

  long <- to_long(ds)
  filtered <- filter(long, !!!dots) |>
    set_attr(ids(ds), x_axis(ds), state(ds)) |>
    dataset_collapse()
  dataset_transfrom(filtered, state(ds), x_axis(ds))
}

#' use tidy select for tibble based selection of ids. only selection on ids is allowed
#' all value cols which are functional dependent on the ids are retained.
#' other value cols are dropped.
ds_select <- function(ds, ..., strict = TRUE) {
  dots <- enquos(...)
  selection <- select(ds, !!!dots) |> names() # tidy selection
  ids <- intersect(selection, id_cols(ds)) # make this a failing check instead of a fix if flag strict is set to true
  decomp <- to_decomposed(ds, strategy = selected_paths_builder(ids))
  selected <- dataset_get_composed(decomp, 1)
  dataset_transfrom(selected, state(ds))
}

# analougus to summarise?
# @todo
ds_summarise <- function(ds, ..., by) {}
