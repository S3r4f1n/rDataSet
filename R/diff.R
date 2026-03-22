# do things. not sure what i need here to be honest, maybe go further away and jsut call it diff
# for difference

library(dplyr)
dataset_diff <- function(a, b) {
  dataset_integrity(a)
  dataset_integrity(b)
  ids <- ids(a) %>% names()

  left_new <- a - b
  right_new <- b - a

  left_int <- a > b
  right_int <- b > a


  left_long <- left_int %>% tidyr::pivot_longer(-all_of(ids), values_transform = as.character) %>% dataset_build(ids = c(ids, "name"))
  right_long <- right_int %>% tidyr::pivot_longer(-all_of(ids), values_transform = as.character) %>% dataset_build(ids = c(ids, "name"))

  eq <- left_long == right_long
  
  missmatches <- inner_join(left_long %>% slice(which(!eq$value)) %>% rename(left = value), right_long %>% rename(right = value), by = c(ids, "name")) %>% rename(variable = name)
  list(
    left_new = left_new,
    right_new = right_new,
    missmatches = missmatches
  )
}

