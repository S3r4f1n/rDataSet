library(dplyr)
# i did the ids updates here and i guess there is something wrong with it
dataset_decompose <- function(dataset, dependence = NULL) {

  if(attr(dataset, "dataset_state") != "wide") stop("dataset in wide form is expected but attr(dataset, 'dataset_state') is: ", attr(dataset, "dataset_state"))
  ids <- ids(dataset) %>% names()
  vals <- vals(dataset) %>% names()

  dp <- if(length(dependence) == 0) dataset_functional_dependence(dataset) else dependence
  plan <- max.col(dp, ties.method = "first")

  plan_b <- tibble(
    variable = row.names(dp),
    level = plan
  ) %>%
    summarise(vars = list(variable), .by = level)

  out <- purrr::map2(plan_b$level, plan_b$vars, \(level, vars) {
    curr_id <- ids[1:level]
    sel <- dataset %>% select(all_of(c(curr_id, vars)))
    out <- slice_head(sel, n = 1, by = all_of(curr_id))
    attr(out, "dataset_ids") <- curr_id
    attr(out, "dataset_state") <- "decomposed"
    class(out) <- c("dataset", class(out))
    out
  })

  attr(out, "dataset_state") <- "decomposed"
  class(out) <- c("dataset", class(out))

  out
}

dataset_functional_dependence <- function(dataset) {
  dataset_integrity(dataset)
  ids <- attr(dataset, "dataset_ids")
  other <- setdiff(names(dataset), ids)
  ds_sorted <- dataset[do.call(order, dataset[ids]), ]

  id_paths <- ids %>%
    purrr::map(~ ds_sorted[[.x]]) %>%
    purrr::accumulate(~ paste(.x, .y, sep = "_"))

  is_const_within <- function(var, id_vec) {
    same_id <- id_vec[-1] == id_vec[-length(id_vec)]
    same_id[is.na(same_id)] <- FALSE

    v1 <- var[-length(var)]
    v2 <- var[-1]

    value_changed <- (v1 != v2) | (is.na(v1) != is.na(v2))
    value_changed[is.na(value_changed)] <- TRUE

    !any(same_id & value_changed)
  }

  out <- lapply(id_paths, function(id_path) {
    sapply(ds_sorted[other], function(var) is_const_within(var, id_path))
  })

  names(out) <- ids
  out %>% as.data.frame() %>% as.matrix()
}

dataset_compose <- function(list_df) {
  if(attr(list_df, "dataset_state") != "decomposed") stop("dataset in composed form is expected but attr(dataset, 'dataset_state') is: ", attr(list_df, "dataset_state"))
  out <- purrr::reduce(list_df, inner_join)
  ids <- purrr::map(list_df, ~ attr(., "dataset_ids")) %>% unlist() %>% unique()
  attr(out, "dataset_ids") <- ids
  attr(out, "dataset_state") <- "wide"
  out
}
