library(dplyr)
## we have had many iterrations of this now.
# i think with this approach of using decomposed dataframes is the correct way
# to build the nested printable data
# we still need to think about wheter we want to store ids as list names
# and when unnesting how we get back the original col names
# basic idea is to have the detailed tables inserted in the more general ones
#
# 
nest_decomposed_list <- function(df_list) {
  purrr::reduce(rev(df_list), function(child, parent) {

    common_keys <- intersect(names(ids(parent)), names(ids(child)))

    if (length(common_keys) == 0) {
      stop("No common keys found between consecutive dataframes.")
    }

    nested_child <- child %>%
      group_by(across(all_of(common_keys))) %>%
      tidyr::nest() %>%
      ungroup()

    parent %>%
      left_join(nested_child)
  })
}

# some nameing issues
flatten_nested_df <- function(nested_df) {
  # Identify which columns are lists (the nested parts)
  list_cols <- names(nested_df)[purrr::map_lgl(nested_df, is.list)]
  
  # If no list columns remain, we are done
  if (length(list_cols) == 0) {
    return(nested_df)
  }
  
  # Unnest the first list column found
  # keep_empty = TRUE ensures we don't lose parent rows that have no children
  flat_df <- nested_df %>%
    tidyr::unnest(cols = all_of(list_cols[1]), keep_empty = TRUE, names_sep = "_")
  
  # Recurse until all levels are flat
  flatten_nested_df(flat_df)
}

ds
ds %>%
  dataset_decompose() %>%
  nest_decomposed_list() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

# this is an updated version which still fails
# maybe add an example and let qwen figure this out

nest_decomposed_list <- function(df_list) {
  # Fold from bottom to top
  reduce(rev(seq_along(df_list))[-1], function(acc_idx, parent_idx) {
    
    child  <- df_list[[parent_idx + 1]]
    parent <- df_list[[parent_idx]]
    
    # Identify the Foreign Key (linking A and B) 
    # and the Primary Key (the unique ID of B)
    common_keys <- intersect(colnames(parent), colnames(child))
    child_pk    <- setdiff(colnames(child), colnames(parent))[1]
    
    # 1. Nest the child data
    nested_child <- child %>%
      group_by(across(all_of(common_keys))) %>%
      nest() %>%
      mutate(data = map(data, function(d) {
        # Transform the nested rows into a named list using the ID column
        # This makes the ID the "Key" in the JSON object
        stats::setNames(split(d, seq(nrow(d))), d[[child_pk]])
      })) %>%
      # Rename the nested column to the ID column name for the JSON key
      rename(!!child_pk := data) %>%
      ungroup()
    
    # 2. Update the parent in the list
    df_list[[parent_idx]] <<- parent %>% 
      left_join(nested_child, by = common_keys)
    
    return(parent_idx)
  })
  
  # Final step: Make the top-level parent a named list by ITS id as well
  top_df <- df_list[[1]]
  top_pk <- colnames(top_df)[1] # Assuming first col is the top-level ID
  
  stats::setNames(split(top_df, seq(nrow(top_df))), top_df[[top_pk]])
}

flatten_nested_df <- function(nested_obj) {
  # Convert the named list back to a data frame if it's the top level
  if (is.list(nested_obj) && !is.data.frame(nested_obj)) {
    nested_obj <- bind_rows(nested_obj)
  }
  
  # Find list columns
  list_cols <- names(nested_obj)[map_lgl(nested_obj, is.list)]
  
  if (length(list_cols) == 0) return(nested_obj)
  
  # Unnest the named lists back into rows
  flat_df <- nested_obj %>%
    mutate(across(all_of(list_cols[1]), ~ map(.x, bind_rows))) %>%
    tidyr::unnest(cols = all_of(list_cols[1]), keep_empty = TRUE)
  
  flatten_nested_df(flat_df)
}
