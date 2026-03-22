library(dplyr)


fancy_transpose <- function(df, ids){
  id_path <- df %>%
    select(all_of(ids)) %>%
    purrr::reduce(\(x,y) paste0(x, ".", y))

  if(length(unique(id_path)) < length(id_path)) stop("id col must uniqley identify each row")

  df %>%
    select(-all_of(ids)) %>%
    purrr::transpose() %>%
    setNames(id_path)
}

fancy_transpose_undo <- function(named_list){
  # todo
  # this is a loss free reverse, well ordering can be lost
}

dataset_flatten <- function(dataset){
  df_list <- dataset_decompose(dataset)

  flat <- purrr::map(df_list, function(child){
    id_child <- names(ids(child))
    fancy_transpose(child, id_child)
  }) %>%
    purrr::reduce(c)

  flat[order(names(flat))]
}

dataset_flatten_undo <- function(flat) {
  # todo
  # this is a loss free reverse, well ordering can be lost
}

dataset_save <- function(dataset, path) {
  flat <- dataset_flatten(dataset)
  yaml::as.yaml(flat) %>%
    stringr::str_split("\n") %>%
    writeLines(path)
}

dataset_load <- function(path) {
  flat <- yaml::as.yaml(path)
  dataset_flatten_undo(flat)
}
