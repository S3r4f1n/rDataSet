ds <- dataset_build(
  tibble(
    id = 1:4%%2,
    lab = "Hi",
    code = c("HI", "HI", "HO", "HO"), 
    labs = 2:5
  ),
  ids = c("id", "code")
)

ds <- dataset_build(
  tibble(
    varname = c("age", "age", "gender", "gender"),
    lab = c("Alter", "Alter", "Geschlecht", "Geschlecht"),
    code = c(1, 2, 1, 2),
    labs = list(5, 6, "weiblich", "männlich")
  ),
  ids = c("varname", "code")
)

ds == ds %>%
  dataset_decompose() %>%
  dataset_compose()
  .[[3]] %>%
  dataset_to_long()

# some bug here
ds %>%
  dataset_to_long() %>%
  dataset_to_wide("id") %>%
  # attr("dataset_x_axis")

  dataset_to_long() %>%
  dataset_to_wide("variable")


df <-   tibble(
    id = 1:4%%2,
    lab = "Hi",
    code = 1:4, 
    labs = 2:5
  )

ids <- c("id", "code")

ds <- dataset_build(
  tibble(
    id = 1:4%%2,
    lab = "Hi",
    code = 1:4, 
    labs = 2:5%%4
  ),
  ids = c("id", "code")
)

b
ds
dataset_diff(ds, b)

tmp <- tempfile(fileext = ".toml")

dataset_save(ds, tmp)

tomledit::as_toml(dataset_nest(ds))
tomledit::from_toml()

readLines(tmp)

ds %>%
  dataset_filter(value == "Hi")

(ds == b) %>%
  dataset_filter(value == FALSE)

ds %>%
  dataset_to_long %>%
  dataset_to_wide %>%
  dataset_to_wide


a <- ds

a - a

dataset_diff(A, B)

# these are fun
ds %>% dataset_to_long() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

# these are fun but not that convincing
ds %>% dataset_to_long() %>%
  dataset_to_wide("code") %>%
  dataset_decompose() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)


# this gets second place and is loss free. but i guess we can optimize it
ds %>% dataset_decompose() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

# this gets closest to what i want to have but has loss...
ds %>% dataset_flatten() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

# i think it is nice that ordering properties are stored in the attributes
# but we need to treat them more sacredly if we really want to do this
# i also think flattening should only ba applied to the last, variable in the
# hirarcy as otherwise it can be impossible to decipher to which id a name belongs
# currently it is clearly identifiable with the infix and postfix.
ds %>%
  {x <- .; attr(x, "dataset_ids") <- c("code", "varname"); x} %>%
  dataset_flatten() %>%
  dataset_flatten_undo()
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)


ds %>% dataset_flatten("varname") %>%
  dataset_flatten_undo()

ds %>% dataset_hirarchical_decompose()
ds %>% dataset_decompose()

a <- A
b <- B


