#' I think it would be greast to create a new data type for this as well
#' or at least build upan the tibble and not on the dataset
#' things to consider, goruping, multi groups?
#' this could be stored as json or toml. We have groupt value, dicts, and values.
#' a dedicated function which allows to intuivley set these would be great.
#' limit on nesting depth? for stasrters yes if we have good ideas no?
#' 
#' group {
#'   group level attributes
#'   varname = val
#'   pairs
#'   varname{
#'     val = val
#'     val = val
#'     val = val
#'   }
#'   lists
#'   varname [val, val, val, val,]
#' }
#' 
#' meta
#' named_list(named_list(named_list()))
#' 
#' Convert Dataset to Nested List Structure
#'
#' Converts a dataset object into a nested list structure suitable for serialization.
#' The structure separates group-level metadata from individual-level data.
#'
#' @param dataset A dataset object to convert.
#' @param .by Optional character vector of column names to group by. If NULL, no grouping is applied.
#' @return A named list with group-level information and a collection of individual records.
#' @export
#' @examples
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:3, group = c("A", "A", "B"), value = c(10, 20, 30)),
#'   ids = "id"
#' )
#' dataset_to_list(ds, .by = "group")


library(dplyr)
# this is a helper to build a collapse plan for the dataframe
dataset_collapse_map <- function(dataset) {
  dataset_integrity(dataset)
  ids <- attr(dataset, "dataset_ids")
  other <- setdiff(names(dataset), ids)
  ds_sorted <- dataset[do.call(order, dataset[ids]), ]

  id_paths <- ids %>% 
    purrr::map(~ ds_sorted[[.x]]) %>% 
    purrr::accumulate(~ paste(.x, .y, sep = "_"))

  global_id <- rep(TRUE, nrow(ds_sorted))

  is_const_within <- function(var, id_vec) {
    same_id <- id_vec[-1] == id_vec[-length(id_vec)]
    same_id[is.na(same_id)] <- FALSE

    v1 <- var[-length(var)]
    v2 <- var[-1]

    value_changed <- (v1 != v2) | (is.na(v1) != is.na(v2))
    value_changed[is.na(value_changed)] <- TRUE 

    !any(same_id & value_changed)
  }

  constancy_matrix <- cbind(
    top_level = sapply(ds_sorted[other], is_const_within, global_id), # top level col
    sapply(id_paths[-length(id_paths)], function(id_path) { # inner cols
      sapply(ds_sorted[other], is_const_within, id_path)
    }),
    setNames(rep(TRUE, length(other)), ids[length(ids)])[[1]] # little hack to get named col
  )

  first_const_idx <- max.col(constancy_matrix, ties.method = "first")

  list(
    ids = ids,
    variables = other,
    mapping = first_const_idx
  )
}

# this collapses the dataframe into a tree like structure
nest <- function(data, ids, collapse_map) {
  curr_id_name <- ids[1]
  curr_id <- factor(data[[curr_id_name]], levels = unique(data[[curr_id_name]]))
  collapse <- collapse_map %>% filter(group_level == curr_id_name) %>% pull(variable)

  collapse <- if (length(collapse) > 0) {
    select(data, all_of(collapse)) %>%
      summarise(across(everything(), ~ head(., 1))) %>%
      as.list()
    } else {
      NULL
    }

  inner <- data %>%
    select(-all_of(c(names(collapse), curr_id_name)))

  out <- if (length(ids) > 1){
    purrr::map(split(inner, curr_id), ~ nest(., ids[-1], collapse_map))
  } else {
    inner %>% purrr::transpose()
  }
  
  c(collapse, setNames(out, levels(curr_id)))
}

# this collapses the dataset into a ready made structur to be written by json lite or toml
dataset_nest <- function(dataset, collapse_map = NULL) {
  collapse_map <- if(length(collapse_map) == 0) dataset_collapse_map(dataset) else collapse_map

  ids <- collapse_map$ids
  variables <- collapse_map$variables
  mapping <- collapse_map$mapping

  level_names <- c("top_level", ids)
  title_names <- c(ids, NA_character_) 

  prepped_mapping <- tibble(
    group_level = title_names[mapping],
    variable = variables,
    inside_group = level_names[mapping]
  )

  ids <- attr(dataset, "dataset_ids")
  metadata <- list(
    ids = ids,
    variables = setdiff(names(dataset), ids),
    structure = prepped_mapping %>%
      summarise(vars = list(variable), .by = inside_group) %>%
      {setNames(.$vars, .$inside_group)}
  )
  data <- nest(dataset, ids, prepped_mapping)
  list(metadata = metadata, data = data)
}

# testing 
ds <- dataset_build(tibble::tibble(
      id = 1:4,
      lab = "hi",
      lab2 = "ho",
      group = c("B", "B", "A", "B"),
      group1 = c("B", "B", "A", "B"),
      int_val = c(1L, 2L, NA, 4L),
      num_val = c(1.5, NA, 3.7, 4.2),
      char_val = c("a", NA, "c", "d"),
      log_val = c(TRUE, FALSE, NA, TRUE)
    ),
    ids = c("group", "id")
    )
df <- ds

data <- ds
ids = c("group", "id")
collapse_map <- tibble(
  id = c("", "group", "group", "", "id", "", "", "", ""),
  col = c("id", "lab", "lab2", "group", "group1", "int_val", "num_val", "char_val", "log_val")
)
dataset_nest(ds) %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
