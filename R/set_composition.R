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

#' Hierarchical decomposition paths
#'
#' Creates a plan for decomposing a dataset by accumulating ID columns
#' in the order they appear. The resulting paths are used by
#' [dataset_decompose()] to split the dataset into smaller tables.
#'
#' @param dataset A dataset object in wide format.
#' @return A tibble with two columns: `ids` (list of character vectors)
#'   and `paths` (list of character vectors). Each row corresponds to a
#'   level of the hierarchy.
#' @seealso [efficient_paths()], [selected_paths_builder()], [dataset_decompose()]
#' @examples
#' \dontrun{
#' library(rDataSet)
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:2, group = c("x","y"), value = 1:2),
#'   ids = c("id","group")
#' )
#' hirarchical_paths(ds)
#' }
#' @export
hirarchical_paths <- function(dataset) {
  idc <- id_cols(dataset)
  id_ids <- purrr::accumulate(idc, c)
  paths <- purrr::accumulate(dataset[idc], paste0)
  tibble(ids = id_ids, paths = paths)
}

#' Efficient decomposition paths
#'
#' Generates all possible combinations of ID columns and orders them
#' by the number of distinct values (fewer first) and the number of
#' ID columns. This strategy can produce a more compact decomposition
#' but runs in exponential time with respect to the number of ID columns.
#' Use only when the number of ID columns is small (typically ≤ 10).
#'
#' @param dataset A dataset object in wide format.
#' @return A tibble with columns `ids` (list of character vectors) and
#'   `paths` (list of character vectors), sorted by increasing
#'   distinctness and ID count.
#' @seealso [hirarchical_paths()], [selected_paths_builder()], [dataset_decompose()]
#' @examples
#' \dontrun{
#' library(rDataSet)
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:2, group = c("x","y"), value = 1:2),
#'   ids = c("id","group")
#' )
#' efficient_paths(ds)
#' }
#' @export
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

#' Build a custom decomposition strategy
#'
#' Returns a function that, when applied to a dataset, creates a
#' decomposition plan where the specified IDs are used first,
#' followed by the remaining IDs, and finally all IDs together.
#' This allows you to prioritise certain ID columns during
#' decomposition.
#'
#' @param ids Character vector of ID column names to prioritise.
#' @return A function that takes a dataset and returns a tibble
#'   with columns `ids` and `paths`, suitable for use as the
#'   `strategy` argument of [dataset_decompose()].
#' @seealso [hirarchical_paths()], [efficient_paths()], [dataset_decompose()]
#' @examples
#' \dontrun{
#' library(rDataSet)
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:2, group = c("x","y"), value = 1:2),
#'   ids = c("id","group")
#' )
#' strat <- selected_paths_builder("group")
#' strat(ds)
#' }
#' @export
selected_paths_builder <- function(ids) {
  function(dataset) {
    idc <- id_cols(dataset)

    id_plan <- list(intersect(ids, idc), setdiff(idc, ids), idc)
    id_plan <- id_plan[lengths(id_plan) > 0]
    id_paths <- purrr::map(
      id_plan,
      ~ purrr::reduce(dataset %>% select(all_of(.x)), paste0)
    )

    tibble(ids = id_plan, paths = id_paths)
  }
}
