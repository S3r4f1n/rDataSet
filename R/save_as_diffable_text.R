library(dplyr)
#' Dataset to Tree Structure Conversion
#'
#' This module provides functionality to convert a dataset into a hierarchical tree structure
#' based on its ID columns. The resulting nested list structure is suitable for serialization
#' to formats like JSON or TOML.
#'
#' The tree structure organizes data by nesting according to the ID column hierarchy,
#' with variables that are constant within each group level stored as group-level attributes.
#'
#' Example structure:
#' \preformatted{
#' {
#'   metadata: { ids, variables, structure },
#'   data: {
#'     group_level_value: {
#'       constant_vars = values,
#'       nested_group: {
#'         ...
#'       }
#'     }
#'   }
#' }
#' }
#'
#' @section Functions:
#' - `dataset_collapse_map()`: Analyzes which variables are constant within each ID level
#' - `dataset_nest()`: Converts dataset to nested tree structure
#'
#' @name dataset_tree
#' @keywords internal
NULL

#' Build Collapse Map for Dataset
#'
#' Analyzes a dataset to determine which variables remain constant within each ID level.
#' This mapping is used to separate group-level attributes from individual-level data
#' when building the tree structure.
#'
#' @param dataset A dataset object to analyze.
#' @return A list containing:
#'   \item{ids}{The ID column names}
#'   \item{variables}{The value column names}
#'   \item{mapping}{Integer vector indicating the deepest ID level at which each variable is constant}
#' @export
dataset_collapse_map <- function(dataset) {
  dataset_integrity(dataset)
  ids <- attr(dataset, "dataset_ids")
  other <- setdiff(names(dataset), ids)
  ds_sorted <- dataset[do.call(order, dataset[ids]), ]

  # Build ID paths for each level (e.g., "A", "A_B" for nested IDs)
  id_paths <- ids %>%
    purrr::map(~ ds_sorted[[.x]]) %>%
    purrr::accumulate(~ paste(.x, .y, sep = "_"))

  global_id <- rep(TRUE, nrow(ds_sorted))

  # Check if a variable remains constant within each group defined by id_vec
  is_const_within <- function(var, id_vec) {
    same_id <- id_vec[-1] == id_vec[-length(id_vec)]
    same_id[is.na(same_id)] <- FALSE

    v1 <- var[-length(var)]
    v2 <- var[-1]

    value_changed <- (v1 != v2) | (is.na(v1) != is.na(v2))
    value_changed[is.na(value_changed)] <- TRUE

    !any(same_id & value_changed)
  }

  # Build constancy matrix: rows=variables, cols=ID levels
  constancy_matrix <- cbind(
    top_level = sapply(ds_sorted[other], is_const_within, global_id),
    sapply(id_paths[-length(id_paths)], function(id_path) {
      sapply(ds_sorted[other], is_const_within, id_path)
    }),
    setNames(rep(TRUE, length(other)), ids[length(ids)])[[1]]
  )

  # For each variable, find the deepest ID level where it's constant
  first_const_idx <- max.col(constancy_matrix, ties.method = "first")

  list(
    ids = ids,
    variables = other,
    mapping = first_const_idx
  )
}

#' Recursively Nest Dataset by ID Columns
#'
#' Internal recursive function that builds the nested tree structure.
#' At each level, it extracts variables that are constant within the current ID group
#' and recursively processes remaining data for deeper nesting.
#'
#' @param data Data frame to nest
#' @param ids Remaining ID column names to process
#' @param collapse_map Tibble mapping variables to their group levels
#' @return A nested list structure
#' @keywords internal
nest <- function(data, ids, collapse_map) {
  curr_id_name <- ids[1]
  curr_id <- factor(data[[curr_id_name]], levels = unique(data[[curr_id_name]]))

  # Extract variables that are constant at this ID level
  collapse <- collapse_map %>% filter(group_level == curr_id_name) %>% pull(variable)

  collapse <- if (length(collapse) > 0) {
    select(data, all_of(collapse)) %>%
      summarise(across(everything(), ~ head(., 1))) %>%
      as.list()
    } else {
      NULL
    }

  # Remove already-processed columns for inner recursion
  inner <- data %>%
    select(-all_of(c(names(collapse), curr_id_name)))

  # Recurse into deeper ID levels or transpose leaf data
  out <- if (length(ids) > 1){
    purrr::map(split(inner, curr_id), ~ nest(., ids[-1], collapse_map))
  } else {
    inner %>% purrr::transpose()
  }

  c(collapse, setNames(out, levels(curr_id)))
}

#' Convert Dataset to Nested Tree Structure
#'
#' Converts a dataset into a hierarchical tree structure based on its ID columns.
#' The output separates metadata about the structure from the actual nested data.
#'
#' The resulting structure is suitable for serialization to JSON, TOML, or other
#' hierarchical formats. Variables that are constant within ID groups are stored
#' as attributes at the appropriate nesting level.
#'
#' @param dataset A dataset object to convert.
#' @param collapse_map Optional pre-computed collapse map from [dataset_collapse_map()].
#'   If NULL, it will be computed automatically.
#' @return A list with two components:
#'   \item{metadata}{List containing ids, variables, and structure mapping}
#'   \item{data}{The nested tree structure}
#' @export
#' @examples
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:4, group = c("A", "A", "B", "B"), value = c(10, 20, 30, 40)),
#'   ids = "group"
#' )
#' result <- dataset_nest(ds)
#' # result$metadata contains structure info
#' # result$data contains nested tree
dataset_nest <- function(dataset, collapse_map = NULL) {
  # Compute collapse map if not provided
  collapse_map <- if(length(collapse_map) == 0) dataset_collapse_map(dataset) else collapse_map

  ids <- collapse_map$ids
  variables <- collapse_map$variables
  mapping <- collapse_map$mapping

  # Build level and title names for the structure mapping
  level_names <- c("top_level", ids)
  title_names <- c(ids, NA_character_)

  # Create prepped mapping: which variables belong to which group level
  prepped_mapping <- tibble(
    group_level = title_names[mapping],
    variable = variables,
    inside_group = level_names[mapping]
  )

  ids <- attr(dataset, "dataset_ids")

  # Build metadata describing the tree structure
  metadata <- list(
    ids = ids,
    variables = setdiff(names(dataset), ids),
    structure = prepped_mapping %>%
      summarise(vars = list(variable), .by = inside_group) %>%
      {setNames(.$vars, .$inside_group)}
  )

  # Build the nested data tree
  data <- nest(dataset, ids, prepped_mapping)
  list(metadata = metadata, data = data)
}

#' Infer R type from JSON-parsed values
#'
#' Attempts to infer the appropriate R type for a vector of values that may
#' contain NULL (representing NA) or the string "NA". Uses aggressive type inference:
#' - If all non-NULL values are logical (TRUE/FALSE), return logical
#' - If all non-NULL values are whole numbers, return integer
#' - If all non-NULL values are numeric, return double
#' - Otherwise return character
#'
#' @param values Vector of values (may contain NULL for NA, or string "NA")
#' @return Vector with inferred type
#' @keywords internal
infer_type <- function(values) {
  # Helper to check if a value represents NA/NULL
  is_na_value <- function(x) {
    is.na(x) || is.null(x) || identical(x, "NA")
  }

  # Replace NULL and "NA" strings with NA
  values <- purrr::map(values, function(x) {
    if (is_na_value(x)) NA else x
  })

  # Check if all values are NA
  if (all(purrr::map_lgl(values, is.na))) {
    return(rep(NA, length(values)))
  }

  # Extract non-NA values for type checking
  non_na <- purrr::discard(values, is.na)

  if (length(non_na) == 0) {
    return(rep(NA, length(values)))
  }

  # Check for logical type (must be TRUE/FALSE, not 0/1)
  if (all(purrr::map_lgl(non_na, ~ is.logical(.) || identical(., TRUE) || identical(., FALSE)))) {
    return(purrr::map_lgl(values, ~ if (is.na(.)) NA else as.logical(.)))
  }

  # Check for numeric types
  if (all(purrr::map_lgl(non_na, is.numeric))) {
    # Check if all values are whole numbers (integers)
    numeric_vals <- purrr::map_dbl(non_na, as.numeric)
    if (all(numeric_vals == floor(numeric_vals))) {
      return(purrr::map_int(values, ~ if (is.na(.)) NA else as.integer(.)))
    } else {
      return(purrr::map_dbl(values, ~ if (is.na(.)) NA else as.numeric(.)))
    }
  }

  # Default to character
  purrr::map_chr(values, ~ if (is.na(.)) NA else as.character(.))
}

#' Recursively Unnest Data Tree to Data Frame
#'
#' Internal recursive function that converts a nested data tree back to a flat
#' data frame. It traverses the nested structure and reconstructs rows by
#' combining ID values from the hierarchy with leaf-level variable values.
#'
#' @param data Nested data tree to unnest
#' @param ids ID column names in order of nesting
#' @param variables Value column names
#' @param current_ids Current ID values from parent levels (named list)
#' @return A data frame with reconstructed rows
#' @keywords internal
unnest_tree <- function(data, ids, variables, current_ids = list()) {
  if (length(ids) == 0) {
    # Leaf level: extract variable values and normalize types
    row_data <- current_ids
    for (var in variables) {
      val <- data[[var]]
      if (is.null(val)) {
        row_data[[var]] <- NA
      } else {
        # Normalize single values using type inference
        row_data[[var]] <- infer_type(list(val))[[1]]
      }
    }
    return(tibble::as_tibble(row_data))
  }

  curr_id_name <- ids[1]
  remaining_ids <- ids[-1]

  # Separate constant variables from nested entries
  var_names <- intersect(names(data), variables)
  entry_names <- setdiff(names(data), variables)

  # Process each entry recursively
  rows <- purrr::map2_df(
    entry_names,
    data[entry_names],
    function(id_val, entry_data) {
      new_ids <- c(current_ids, setNames(list(id_val), curr_id_name))
      unnest_tree(entry_data, remaining_ids, variables, new_ids)
    }
  )

  # Add constant variables to all rows
  for (var in var_names) {
    val <- data[[var]]
    # Convert single values to vector matching row count
    if (is.null(val)) {
      rows[[var]] <- NA
    } else if (length(val) == 1) {
      rows[[var]] <- val
    } else {
      # Vector value - use type inference
      rows[[var]] <- infer_type(as.list(val))
    }
  }

  rows
}

#' Convert Nested Tree Structure Back to Dataset
#'
#' Reverses the operation of [dataset_nest()], converting a nested tree structure
#' (either directly from `dataset_nest()` or from JSON/TOML parsing) back into
#' a flat dataset object.
#'
#' @param nested_data A nested tree structure with `metadata` and `data` components.
#' @return A dataset object with the original structure restored.
#' @export
#' @examples
#' ds <- dataset_build(
#'   tibble::tibble(id = 1:3, group = c("A", "A", "B"), value = c(10, 20, 30)),
#'   ids = "group"
#' )
#' nested <- dataset_nest(ds)
#' restored <- dataset_unnest(nested)
dataset_unnest <- function(nested_data) {
  # Validate input structure
  if (!is.list(nested_data) || !all(c("metadata", "data") %in% names(nested_data))) {
    stop("Input must contain both 'metadata' and 'data' components")
  }

  metadata <- nested_data$metadata
  data <- nested_data$data

  # Validate metadata
  if (is.null(metadata$ids)) {
    stop("metadata must contain 'ids' field")
  }

  # Flatten metadata fields (JSON parsing may create nested lists)
  ids <- if (is.list(metadata$ids)) unlist(metadata$ids) else metadata$ids
  variables <- if (is.list(metadata$variables)) unlist(metadata$variables) else metadata$variables

  # Convert JSON-parsed data (which may have NULL for missing values) to proper R NA
  convert_null_to_na <- function(x) {
    if (is.list(x)) {
      purrr::map(x, convert_null_to_na)
    } else if (is.null(x)) {
      NA
    } else {
      x
    }
  }

  data <- convert_null_to_na(data)

  # Recursively unnest the tree structure
  df <- unnest_tree(data, ids, variables)

  # Ensure columns are in the correct order: IDs first, then variables
  all_cols <- c(ids, variables)
  df <- df[, all_cols]

  # Apply type inference to all columns
  # ID columns: try to infer numeric type from string names
  for (id_col in ids) {
    vals <- df[[id_col]]
    if (is.character(vals)) {
      # Try to infer if character IDs are actually numeric
      suppressWarnings({
        numeric_vals <- as.numeric(vals)
        if (!any(is.na(numeric_vals)) && all(numeric_vals == floor(numeric_vals))) {
          # All values are whole numbers - convert to integer
          df[[id_col]] <- as.integer(numeric_vals)
        }
      })
    }
  }

  # Value columns: use type inference
  for (var in variables) {
    vals <- df[[var]]
    # Use infer_type to ensure proper typing
    df[[var]] <- infer_type(as.list(vals))
  }

  # Convert to dataset
  dataset_build(df, ids = ids)
}
