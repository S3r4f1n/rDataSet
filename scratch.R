ds <- dataset_build(
  tibble(
    id = 1:4%%2,
    lab = "Hi",
    code = 1:4, 
    labs = 2:5
  ),
  ids = c("id", "code")
)

b <- dataset_build(
  tibble(
    id = 1:4%%2,
    lab = "Hi",
    code = 1:4, 
    labs = 2:5%%4
  ),
  ids = c("id", "code")
)

tmp <- tempfile(fileext = ".toml")

dataset_save(ds, tmp)

tomledit::as_toml(dataset_nest(ds))
tomledit::from_toml()

readLines(tmp)
