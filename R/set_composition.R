library(dplyr)

dataset_decompose <- function(dataset) {
  if(attr(dataset, "dataset_state") != "wide") stop("dataset in wide form is expected but attr(dataset, 'dataset_state') is: ", attr(dataset, "dataset_state"))
  ids <- names(ids(dataset))
  vals <- names(vals(dataset))
  id_plan <- map(seq_along(ids), ~ combn(ids, .x, simplify = FALSE)) %>% flatten()
  id_paths <- map(id_plan, ~ purrr::reduce(dataset %>% select(all_of(.x)), paste0))

  # we rank id paths by
  # 1. fewest distinct values
  # 2. number of ids
  op <- tibble(id_plan, id_paths) %>% # this is the golden plan
    mutate(
      n_distinct = map(id_paths, n_distinct) %>% unlist(),
      n_ids = map(id_plan, length) %>% unlist()
    ) %>%
    arrange(n_distinct, n_ids)

  # heavy lifting
  dependence_table <-  dataset_functional_dependence(dataset, op)
  indices <- max.col(dependence_table, ties.method = "first")
  # refers to the highest ranked id path which fully describes the data

  # we build the collection of ids and vars, each row will be a dataframe in the output
  rd_plan <- tibble(row = seq_along(indices), indices = max.col(dependence_table, ties.method = "first")) %>%
    mutate(
      variable = rownames(dependence_table)[row],
      ids = op$id_plan[indices]
      ) %>%
    summarise(
      ids = head(ids, 1),
      vars = list(variable),
      .by = indices
    )

  # we save the id relation separatly to be loss less on joins
  used_ids <- rd_plan %>% pull(ids) %>% unlist() %>% unique()
  id_relation <- ids(dataset)[used_ids]

  # simple helper function
  reduce_dataset <- function(dataset, ids, vals) {
    out <- dataset %>%
      slice_head(n =  1, by = all_of(ids)) %>%
      select(all_of(c(ids, vals)))
    attr(out, "dataset_ids") <- ids
    attr(out, "dataset_state") <- "wide"
    class(out) <- c("dataset", class(out))
    out
  }

  # reducing the dataset into its components
  decomposed_vals <- purrr::map2(rd_plan$ids, rd_plan$vars, \(ids, vals) {
    reduce_dataset(dataset, ids, vals)
  })

  decomposition <- c(list(id_relation), decomposed_vals)
  attr(decomposition, "dataset_state") <- "decomposed"

  decomposition
}

# build fd table
# x axis id and id combinations
# y axis values
# cells if the id coll can copmletely discibe the value it is set to true
# this is the golden middle where the heavy lifting is done
# @internal
dataset_functional_dependence <- function(dataset, op) {
  vals <- names(vals(dataset))
  is_const_within <- function(var, id_vec) {
    tapply(var, id_vec, \(x) length(unique(x)) <= 1) |> all()
  }

  out <- lapply(op$id_paths, function(id_path) {
    sapply(dataset[vals], function(var) is_const_within(var, id_path))
  })

  names(out) <- purrr::map(op$id_plan, ~ purrr::reduce(.x, paste0)) %>% unlist()
  out %>% as.data.frame() %>% as.matrix() 
}

dataset_compose <- function(list_df) {
  if(attr(list_df, "dataset_state") != "decomposed") stop("dataset in composed form is expected but attr(dataset, 'dataset_state') is: ", attr(list_df, "dataset_state"))

  # Join all data frames using natural join (join on all common columns)
  out <- purrr::reduce(list_df, ~ {
    common_cols <- intersect(names(.x), names(.y))
    inner_join(.x, .y, by = common_cols)
  })

  ids <- purrr::map(list_df, ~ attr(., "dataset_ids")) %>% unlist() %>% unique()
  attr(out, "dataset_ids") <- ids
  attr(out, "dataset_state") <- "wide"
  out
}
