dataset_transfrom <- function(ds, to, x_axis = NULL, decompose_strategy = hirarchical_paths) {
  from <- state(ds)
  
  if (from == to) {
    return(ds)
  }
  
  transition <- paste0(from, "_to_", to)
  
  result <- switch(transition,
    "wide_to_long" = wide_to_long(ds),
    "wide_to_decomposed" = dataset_decompose(ds, strategy = decompose_strategy),
    "long_to_wide" = long_to_wide(ds, col = x_axis),
    "long_to_decomposed" = dataset_decompose(long_to_wide(ds, col = x_axis), strategy = decompose_strategy),
    "decomposed_to_wide" = dataset_compose(ds),
    "decomposed_to_long" = wide_to_long(dataset_compose(ds)),
    stop("Unsupported transformation from ", from, " to ", to)
  )
  
  return(result)
}
