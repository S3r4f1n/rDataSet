library(dplyr)

#' Decompose Dataset into Hierarchical Components
#'
#' Decomposes a wide-format dataset into a list of smaller datasets based on
#' functional dependencies between ID columns and value columns. Each component
#' dataset contains variables that are functionally dependent on a specific
#' level of the ID hierarchy.
#'
#' **Decomposition Process:**
#' - Analyzes functional dependencies between ID columns and value columns
#' - Groups value columns by the level of ID hierarchy they depend on
#' - Creates separate datasets for each dependency level
#' - Each component retains only the ID columns necessary for that level
#'
#' **State Transition:** `"wide"` → `"decomposed"` (returns a list of datasets)
#'
#' @param dataset A dataset in wide format.
#' @param dependence Optional pre-computed functional dependence matrix. If `NULL`,
#'   it is computed automatically via `dataset_functional_dependence()`.
#' @return A list of datasets in "decomposed" state, where each element represents
#'   a dependency level.
#' @details Requires the input dataset to be in "wide" state. The decomposition
#'   enables more efficient storage and operations by separating variables that
#'   depend on different levels of the ID hierarchy.
#' @keywords internal
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

#' Compute Functional Dependence Matrix
#'
#' Analyzes the functional dependencies between ID columns and value columns
#' in a dataset. Determines which value columns are functionally dependent
#' on which level of the ID hierarchy.
#'
#' **Algorithm:**
#' - Sorts the dataset by ID columns
#' - For each ID level, checks if value columns remain constant within groups
#'   defined by that ID level
#' - Returns a boolean matrix where `TRUE` indicates functional dependence
#'
#' **Dependence Criterion:** A value column is functionally dependent on an ID
#' level if, within each group of rows sharing the same ID prefix, the value
#' column does not change (all non-NA values are identical).
#'
#' @param dataset A dataset to analyze for functional dependencies.
#' @return A logical matrix with value columns as rows and ID columns as columns.
#'   `matrix[i, j] = TRUE` means value column `i` is functionally dependent on
#'   ID column `j` (and transitively on all preceding ID columns).
#' @details The function uses row-level comparison to detect when values change
#'   within ID groups. NA values are handled specially - a change from NA to
#'   non-NA (or vice versa) is considered a value change.
#' @keywords internal
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

#' Compose Decomposed Datasets into Wide Format
#'
#' Reconstructs a wide-format dataset from a list of decomposed component datasets.
#' Performs inner joins across all components to restore the original structure.
#'
#' **Composition Process:**
#' - Inner joins all component datasets together
#' - Merges ID columns from all components (taking unique set)
#' - Restores the wide format with all value columns
#'
#' **State Transition:** `"decomposed"` → `"wide"`
#'
#' This is the inverse operation of `dataset_decompose()`.
#'
#' @param list_df A list of datasets in "decomposed" state (as returned by
#'   `dataset_decompose()`).
#' @return A single dataset in "wide" format with all components joined together.
#' @details Requires all input datasets to be in "decomposed" state. Uses inner
#'   join semantics, so only rows present in all components will be retained.
#' @keywords internal
dataset_compose <- function(list_df) {
  if(attr(list_df, "dataset_state") != "decomposed") stop("dataset in composed form is expected but attr(dataset, 'dataset_state') is: ", attr(list_df, "dataset_state"))
  out <- purrr::reduce(list_df, inner_join)
  ids <- purrr::map(list_df, ~ attr(., "dataset_ids")) %>% unlist() %>% unique()
  attr(out, "dataset_ids") <- ids
  attr(out, "dataset_state") <- "wide"
  out
}
