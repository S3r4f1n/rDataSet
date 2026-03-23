library(dplyr)

# similar to dataset_decompose but with different strategy
dataset_hirarchical_decompose <- function(dataset) {
  if(attr(dataset, "dataset_state") != "wide") stop("dataset in wide form is expected but attr(dataset, 'dataset_state') is: ", attr(dataset, "dataset_state"))
  ids <- names(ids(dataset))

  # empty case
  if(length(ids) == 0) {
    decomposed <- list(dataset_empty())
    attr(decomposed, "dataset_ids") <- NULL
    attr(decomposed, "dataset_state") <- "decomposed"
    return(decomposed)
  }

  vals <- names(vals(dataset))
  id_plan <- purrr::accumulate(ids, c)
  id_paths <- purrr::accumulate(ids(dataset), paste0)

  # we rank id paths by simple hirarchy
  op <- tibble(id_plan, id_paths)

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

  decomposition <- decomposed_vals
  attr(decomposition, "dataset_state") <- "decomposed"
  attr(decomposition, "dataset_ids") <- rd_plan$ids %>% unlist() %>% unique()
  class(decomposition) <- c("dataset", class(decomposition))

  decomposition
}
