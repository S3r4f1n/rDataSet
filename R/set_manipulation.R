library(dplyr)

dataset_filter <- function(data, ...) {
  dots <- enquos(...)

  long <- dataset_to_long(data)
  filtered <- filter(long, !!!dots)
  dataset_to_wide(filtered)
}

dataset_to_long <- function(dataset) {
  ids <- ids(dataset) %>% names()
  if(attr(dataset, "dataset_state") != "wide") stop("dataset in wide form is expected but attr(dataset, 'dataset_state') is: ", attr(dataset, "dataset_state"))
  attr(dataset, "dataset_state") <- "long"

  long <- dataset %>%
    tidyr::pivot_longer(-all_of(ids), values_transform = as.character) %>%
    rename(variable = name) %>%
    dataset_build(ids = c(ids, "variable"))

  dataset_integrity(long)
}

dataset_to_wide <- function(dataset) {
  ids <- ids(dataset) %>% names()
  vals <- vals(dataset) %>% names()

  if(attr(dataset, "dataset_state") != "long") stop("dataset in long form is expected but attr(dataset, 'dataset_state') is: ", attr(dataset, "dataset_state"))
  if(any(vals != "value")) stop("a data set in long form is expected to only have one, vals column named value")
  if(!"variable" %in% ids) stop("a data set in long form is expected to have a id column which is called variable")

  attr(dataset, "dataset_state") <- "wide"

  wide <- dataset %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    dataset_build(ids = setdiff(c(ids), "variable"))

  dataset_collapse(wide)
}
