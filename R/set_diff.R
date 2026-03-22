# do things. not sure what i need here to be honest, maybe go further away and jsut call it diff
# for difference

library(dplyr)
dataset_diff <- function(a, b) {
  dataset_integrity(a)
  dataset_integrity(b)

  suppressWarnings(if(a == b) return(NULL))

  long_a <- dataset_to_long(a) %>% rename(left = value)
  long_b <- dataset_to_long(b) %>% rename(right = value)

  int <- full_join(long_a, long_b)
  diff <- int %>%
    filter(xor(is.na(left), is.na(right)) | left != right)

  diff
}

