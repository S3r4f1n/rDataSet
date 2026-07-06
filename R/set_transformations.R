dataset_transfrom <- function(ds, to, x_axis = NULL, decompose_strategy = hirarchical_paths) {
  from <- state(ds) # wide, long, decomposed
  
  if (from == to) {
    return(ds)
  }
  
  # Handle transformations from wide to other states
  if (from == "wide" && to == "long") {
    return(wide_to_long(ds))
  }
  
  if (from == "wide" && to == "decomposed") {
    return(dataset_decompose(ds, strategy = decompose_strategy))
  }
  
  # Handle transformations from long to other states
  if (from == "long" && to == "wide") {
    return(long_to_wide(ds, col = x_axis))
  }
  
  if (from == "long" && to == "decomposed") {
    # Transform long to wide then decompose
    wide_ds <- long_to_wide(ds, col = x_axis)
    return(dataset_decompose(wide_ds, strategy = decompose_strategy))
  }
  
  # Handle transformations from decomposed to other states
  if (from == "decomposed" && to == "wide") {
    # Need to compose the decomposed dataset back to wide
    return(dataset_compose(ds))
  }
  
  if (from == "decomposed" && to == "long") {
    # Transform decomposed to wide then to long
    wide_ds <- dataset_compose(ds)
    return(wide_to_long(wide_ds))
  }
  
  # If we get here, the transformation isn't supported
  stop("Unsupported transformation from ", from, " to ", to)
}
