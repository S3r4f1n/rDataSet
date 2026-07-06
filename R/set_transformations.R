

# implement ai!
dataset_transfrom <- function(ds, to, x_axis = NULL, decompose_strategy = hirarchical_paths) {
  from <- state(ds) # wide, long, decomposed
  switch case
    wide to wide return ds
    wide to long return wide_to_long(ds)
    long to wide return long_to_wide(ds, col = x_axis)
    ...
    wide to decomposed return dataset_decompose(ds, strategy = decompose_strategy)
    long to decomposed return ds |> long_to_wide() |> (_, strategy = decompose_strategy)
}
