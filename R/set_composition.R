require(dplyr)

# joining a list of datasets
dataset_compose <- function(list_df) {
  if (state(list_df) != "decomposed") {
    stop(
      "dataset in composed form is expected but attr(dataset, 'dataset_state') is: ",
      state(list_df)
    )
  }

  # empty case
  if (is_empty_set(list_df[[1]])) {
    return(list_df[[1]])
  }

  # Join all data frames using natural join (join on all common columns)
  out <- purrr::reduce(
    list_df,
    ~ {
      common_cols <- intersect(names(.x), names(.y))
      inner_join(.x, .y, by = common_cols)
    }
  )

  set_attr(out, ids(list_df), x_axis(list_df), "wide")
}

# splitting a dataset into a list of joinable datasets.
dataset_decompose <- function(dataset, strategy = hirarchical_paths) {
  if (state(dataset) != "wide") {
    stop(
      "dataset in wide form is expected but attr(dataset, 'dataset_state') is: ",
      state(dataset)
    )
  }

  # empty case
  if (is_empty_set(dataset)) {
    decomposed <- list(dataset)
    return(set_attr(decomposed, NULL, NULL, "decomposed"))
  }

  # defines order in which ids and combination of
  # ids are checkd on their functional dependence
  paths <- strategy(dataset)

  valc <- val_cols(dataset)
  mask <- names(dataset) %in% valc
  rd_plan <- list()
  col_names <- names(dataset)
  for (i in seq_len(nrow(paths))) {
    cols <- df_functional_dependence(unlist(paths[i, "paths"]), dataset[, mask])
    rd_plan[[i]] <- list(
      ids = unlist(paths[i, "ids"]),
      valc = unlist(col_names[mask][cols])
    )
    mask[mask] <- !cols # update mask
  }

  x_axis_t <- x_axis(dataset)
  # reducing the dataset into smaller "functional" dependent tables
  decomposition <- purrr::map(rd_plan, \(plan) {
    slice_dataset(dataset, plan$ids, plan$valc, x_axis_t)
  })

  set_attr(decomposition, ids(dataset), x_axis_t, "decomposed")
}


# reduce a dataset to the sepcified id cols and val cols
slice_dataset <- function(dataset, id_cols, val_cols, x_axis) {
  out <- dataset %>%
    dplyr::slice_head(n = 1, by = all_of(unname(id_cols))) %>%
    select(all_of(unname(c(id_cols, val_cols))))
  set_attr(out, c(id_cols, x_axis), x_axis, "wide")
}


# for a given vecotr find the functional dependent cols
# returns vec of bools
df_functional_dependence <- function(v, df) {
  preloaded_functional_dependence <- function(b) {
    tapply(b, v, \(x) length(unique(x)) <= 1) |> all()
  }

  purrr::map(df, preloaded_functional_dependence) |> as.logical()
}

dataset_get_composed <- function(ds, index) {
  state <- state(ds)
  if (state != "decomposed") {
    stop(paste0("State should be decomposed! state: ", state))
  }
  if (index < 1 || index > length(ds)) {
    stop(paste0("Index should be in 1:", length(ds), "! index: ", index))
  }
  out <- ds[[index]]
  ids <- c(intersect(names(out), ids(ds)), x_axis(ds))
  set_attr(out, ids, x_axis(ds), "wide")
}

# strategy hirarchical / in order of ids
hirarchical_paths <- function(dataset) {
  idc <- id_cols(dataset)
  id_ids <- purrr::accumulate(idc, c)
  paths <- purrr::accumulate(dataset[idc], paste0)
  tibble(ids = id_ids, paths = paths)
}

#' this creates all combinations of id paths and arranges them
#' by the number of distinct values in a path (lower to more) and
#' by the number of ids cols in a path. The higher up in the list
#' the earlier it is used for the decomposition. later paths might
#' not be used at all. exponential time in number of id columns. But these
#' should be generaly only a few (up to 10 different ids)
efficient_paths <- function(dataset) {
  idc <- id_cols(dataset)

  id_plan <- purrr::map(seq_along(idc), ~ combn(idc, .x, simplify = FALSE)) %>%
    purrr::flatten()
  id_paths <- purrr::map(
    id_plan,
    ~ purrr::reduce(dataset %>% select(all_of(.x)), paste0)
  )

  tibble(id_plan, id_paths) %>% # this is the golden plan
    mutate(
      n_distinct = purrr::map(id_paths, n_distinct) %>% unlist(),
      n_ids = purrr::map(id_plan, length) %>% unlist()
    ) %>%
    arrange(n_distinct, n_ids) %>%
    select(ids = id_plan, paths = id_paths)
}

# ok this is a functional mess but works
# first add ids, and then you get a function
# which expects dataset selected_paths_builder(ids)(dataset)
selected_paths_builder <- function(ids) {
  function(dataset) {
    idc <- id_cols(dataset)

    id_plan <- list(intersect(ids, idc), setdiff(idc, ids), idc)
    id_paths <- purrr::map(
      id_plan,
      ~ purrr::reduce(dataset %>% select(all_of(.x)), paste0)
    )

    tibble(ids = id_plan, paths = id_paths)
  }
}
