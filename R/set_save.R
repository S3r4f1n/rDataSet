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

fancy_transpose_undo <- function(named_list, ids, infix = ".", postfix = ""){
  if(length(named_list) == 0) {
    return(data.frame())
  }

  # Extract names and remove postfix
  names_vec <- names(named_list)
  if(postfix != "") {
    names_vec <- sub(paste0(postfix, "$"), "", names_vec)
  }

  # Split names by infix to get ID values
  id_parts <- strsplit(names_vec, infix, fixed = TRUE)

  # Build ID columns
  id_df <- purrr::map(seq_along(ids), function(i) {
    purrr::map_chr(id_parts, ~ .x[i])
  }) %>%
    setNames(ids) %>%
    as.data.frame()

  # Collect all unique value column names across all elements
  all_val_cols <- unique(unlist(purrr::map(named_list, names)))

  # Build value columns - each element of the list may have different columns
  val_df <- purrr::map(all_val_cols, function(col) {
    purrr::map_chr(named_list, ~ {
      val <- .x[[col]]
      if(is.null(val)) NA_character_ else as.character(val)
    })
  }) %>%
    setNames(all_val_cols) %>%
    as.data.frame()

  # Reset row names and combine
  rownames(id_df) <- NULL
  rownames(val_df) <- NULL
  cbind(id_df, val_df)
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

#' Reverse dataset_flatten: Reconstruct Dataset from Flat List
#'
#' Reconstructs a dataset from a flat list created by `dataset_flatten()`.
#' First reconstructs the hierarchical decomposition, then composes it back
#' into a dataset.
#'
#' @param flat A flat list created by `dataset_flatten()`.
#' @return A dataset object.
#' @keywords internal
dataset_flatten_undo <- function(flat) {
  # Extract the dataset_ids (first element)
  ids <- flat$dataset_ids
  flat_no_ids <- flat[-1]

  if(length(flat_no_ids) == 0) {
    return(dataset_empty())
  }

  # The ids vector is: c(original_ids, x_axis, flatten_by) where flatten_by is last
  flatten_by <- ids[length(ids)]
  base_ids <- ids[1:(length(ids) - 2)]

  # Group entries by their component structure
  # Entries with the same ID columns should be in the same component
  # - Non-flattened entries with same base_ids go together
  # - Flattened entries with same (base_ids, x_axis_name) go together

  # Collect all rows for each component type
  # Key by: paste(comp_ids, collapse=",") for non-flattened
  # Key by: paste(comp_ids, collapse=",") + x_axis_name for flattened
  components <- list()

  for(entry_name in names(flat_no_ids)) {
    entry_data <- flat_no_ids[[entry_name]]
    has_postfix <- grepl(":$", entry_name)

    if(has_postfix) {
      # Flattened entry
      name_clean <- sub(":$", "", entry_name)
      parts <- strsplit(name_clean, ".", fixed = TRUE)[[1]]
      n_base_ids <- length(base_ids)
      id_vals <- parts[1:n_base_ids]
      x_axis_name <- parts[n_base_ids + 1]
      comp_ids <- c(base_ids, flatten_by)

      # Key includes x_axis_name to separate different value columns
      comp_key <- paste(c("flat", comp_ids, x_axis_name), collapse = "|")

      # Build rows for this entry
      row_data <- purrr::map(names(entry_data), function(flatten_val) {
        val <- entry_data[[flatten_val]]
        tibble::tibble(
          !!!setNames(list(id_vals), base_ids),
          !!flatten_by := flatten_val,
          !!x_axis_name := val
        )
      })
      df <- dplyr::bind_rows(row_data)

      # Add to component
      if(is.null(components[[comp_key]])) {
        components[[comp_key]] <- list(df = df, comp_ids = comp_ids)
      } else {
        components[[comp_key]]$df <- dplyr::bind_rows(components[[comp_key]]$df, df)
      }

    } else {
      # Non-flattened entry
      parts <- strsplit(entry_name, ".", fixed = TRUE)[[1]]
      id_vals <- parts
      comp_ids <- base_ids
      comp_key <- paste(c("nonflat", comp_ids), collapse = "|")

      # Build row for this entry
      row_data <- tibble::tibble(
        !!!setNames(list(id_vals), comp_ids),
        !!!entry_data
      )

      # Add to component
      if(is.null(components[[comp_key]])) {
        components[[comp_key]] <- list(df = row_data, comp_ids = comp_ids)
      } else {
        components[[comp_key]]$df <- dplyr::bind_rows(components[[comp_key]]$df, row_data)
      }
    }
  }

  # Convert components to datasets
  decomposed <- purrr::map(components, function(comp) {
    df <- comp$df
    comp_ids <- comp$comp_ids
    attr(df, "dataset_ids") <- comp_ids
    attr(df, "dataset_x_axis") <- "variable"
    attr(df, "dataset_state") <- "wide"
    class(df) <- c("dataset", class(df))
    df
  })

  # Set up the decomposed structure
  attr(decomposed, "dataset_state") <- "decomposed"
  attr(decomposed, "dataset_ids") <- base_ids
  class(decomposed) <- c("dataset", class(decomposed))

  # Compose back into a single dataset
  dataset_compose(decomposed)
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


# example usage for flattening:
if(FALSE) {
ds <- dataset_build(
  tibble(
    varname = c("age", "age", "gender", "gender"),
    lab = c("Alter", "Alter", "Geschlecht", "Geschlecht"),
    code = c(1, 2, 1, 2),
    labs = list(5, 6, "weiblich", "männlich")
  ),
  ids = c("varname", "code")
)

# Flatten by "code" and convert to JSON
ds %>% dataset_flatten("code") %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

# Flatten by "varname" and convert to JSON
ds %>% dataset_flatten("varname") %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

# Round-trip example: flatten, save, load, and reconstruct
flat <- ds %>% dataset_flatten("code")
ds_reconstructed <- dataset_flatten_undo(flat)
ds == ds_reconstructed  # Should return TRUE
}
