library(dplyr)

fancy_transpose <- function(df, ids, infix = ".", postfix = ""){
  id_path <- df %>%
    select(all_of(ids)) %>%
    purrr::reduce(\(x,y) paste0(x, infix, y)) %>%
    paste0(., postfix)

  if(length(unique(id_path)) < length(id_path)) stop("id col must uniqley identify each row")

  df %>%
    select(-all_of(ids)) %>%
    purrr::transpose() %>%
    setNames(id_path)
}

# todo
# this is a loss free reverse, well ordering can be lost
fancy_transpose_undo <- function(named_list){
}

# key points of implementation
# 1. build hiracical decomposition (given two id sets one must contain the other, build this from ordinal prsedence, e.g. left to right)
# 2. we can drop the id relation because of this, but we lose the simetry of the decomposition
# 3. specify flattening id (flatten by this, otherwhise by x_axis as this is contained in every table)
# 
# example: 
# [id.id]
# x_axis = val
# x_axis = val
# [id.id.x_axis]
# code = val
# code = val
# code = val
# 
dataset_flatten <- function(dataset, flatten_by){
  df_list <- dataset_hirarchical_decompose(dataset)
  decomposed_vals <- df_list

  flat <- purrr::map(decomposed_vals, function(child){
    id_child <- names(ids(child))
    if(flatten_by %in% id_child) {
      fancy_transpose(
        child %>% dataset_to_long() %>% dataset_to_wide(flatten_by),
        c(setdiff(id_child, flatten_by),  attr(child, "dataset_x_axis")),
        postfix = ":"
      )
    } else {
      fancy_transpose(child, id_child)
    }
  }) %>%
    purrr::reduce(c)

  flat_order <- flat[order(names(flat))]
  ids <- c(setdiff(c(names(ids(dataset)), attr(dataset, "dataset_x_axis")), flatten_by), flatten_by)

  c(
    dataset_ids = list(ids),
    flat_order
  )

}

# todo
# this is a loss free reverse, well ordering can be lost
dataset_flatten_undo <- function(flat) {
}

dataset_save <- function(dataset, path, flatten_by = NULL) {
  flatten_by <- if(is.null(flatten_by)) attr(dataset, "dataset_x_axis") else flatten_by
  flat <- dataset_flatten(dataset, flatten_by)
  jsonlite::write_json(flat, path)
}

dataset_load <- function(path) {
  flat <- jsonlite::read_json(path)
  dataset_compose(flat)
}
