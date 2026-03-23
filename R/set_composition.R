library(dplyr)

#' @todo make this more general and formal, rmove the hirarchcal part?
#' and work with a dummy id for level 0? Currently order of ids matter, this i convenient in some cases
#' but flawed in other. Make it independent of the order of ids.
#' independent vars, single dependence, multi dependence, what if multiple ids can completly cover the variance
#' use the id with the lowest unique values (n_distinct)
#'
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

dataset_clean_decompose <- function(dataset) {
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
  dependence_table <-  dataset_clean_functional_dependence(dataset, op)
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
    attr(out, "dataset_state") <- "decomposed"
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
  # @todo rework the is_const_within function to work on unsorted tables
dataset_clean_functional_dependence <- function(dataset, op) {
  vals <- names(vals(dataset))
  is_const_within <- function(var, id_vec) {
    tapply(var, id_vec, \(x) length(unique(x)) <= 1) |> all()
  }
  # we can no longer rely on beeing sorted is_const_within must be refactored
  out <- lapply(op$id_paths, function(id_path) {
    sapply(dataset[vals], function(var) is_const_within(var, id_path))
  })

  names(out) <- purrr::map(op$id_plan, ~ purrr::reduce(.x, paste0)) %>% unlist()
  out %>% as.data.frame() %>% as.matrix() 
}


# filter trivial independence

# this would lead to two completly independent tables.
# we lose the infromation connecting a and b
# we need to keep the hirarchy for in the decompositions...
# we need to be smarter about the selection of id_paths, smallest isn't sufficient
# the subset of ids must be connected as well. meaning the id paths are hyper edges over
# ids we choose a subset of hyper adges which give a connected hypr tree
# additionally wa can choose maybe a minimum spanning hyper tree. where the weight
# is computed as a combination of n_distinct values per edge. one dge covering all would
# likely always be minimal in that sense. i guess we want to the add up over the variables
# e.g. if we can factor some out we are happy but we'll likely need the full edge always
# this should be fine as we keep all relations id relations in tact and still we reduce some
# varaibles.
#
# so what we want is a set of edges, which connects all ids, but also is minimal in the sense
# of n_distinct * edge count. for each variable, we need one edge (place to store our information)
# so if we would choose the completely covering edge n times we would have a solution
# (our initial state) but also the maximum amount of n_distinct * edges.
# we can reduce this connections by choosing more suitable edges.
#
# so we need to keep to constraints going.
#
# 1. Graph is connected
# 2. edges are relaxed where possible.
#
# in this example the best would be to use
# id a for var one
# and id a,b for var two
#
# if we only would have var 1 we still would need to store a,b
# as other wise we loose the relation between the ids. if we say we
# only have ids which are necesarry in the sense they can't be fully covered by an id
# with less distinct values. more desirable.
#
# now this would lead in a case where any id can fully cover the sets the best option to
# the id with the fewest distinct values.
# 
# we want to select some values from that dependence table. mostly the hmm
# we want to select the ids such that we have small tables, e.g. for each variable we chosse the id with the lowest
# distinct values
# we need to guarantee coveredness and want to optimize n_distinct.
# ok after thinking about this for a long time  i guess the best approach is to
# select the first ids which cover the whole graph
# and additionaly store a id relation.
# this guarantees a loss less join and can reduce information
# down into smaller tables.
# this is of limited use to optimize performance at the current my current  understanding
# but maybe still helpful to create set minus.
#
# so we can actually use the easy optimization above and additionaly store the complete id relation.
# now there could be heavily redundant information in the ids but we won't care about that
# or we could actualy filter the complete id col to reduce to ids which are actually
# needed to cover the variables
