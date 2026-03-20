

#' Save and Load Datasets
#'
#' Wrapper functions around [dataset_nest()] and [dataset_unnest()] to make
#' saving and loading datasets more comfortable. Supports JSON and TOML formats.
#'
#' @name dataset_save
#' @keywords internal
NULL

#' Save Dataset to File
#'
#' Saves a dataset to a file in JSON or TOML format.
#'
#' @param dataset A dataset object to save.
#' @param path The file path where to save the dataset.
#' @param type The file type, either "json" or "toml". If NULL, inferred from path extension.
#' @return The nested structure that was saved (invisibly).
#' @export
#' @examples
#' \dontrun{
#' ds <- dataset_build(tibble::tibble(id = 1:3, value = c(10, 20, 30)), ids = "id")
#' dataset_save(ds, "output.json", type = "json")
#' dataset_save(ds, "output.toml", type = "toml")
#' }
dataset_save <- function(dataset, path, type = NULL) {
  if (!"dataset" %in% class(dataset)) stop("dataset must be of instance dataset") 

  if (is.null(type)) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("json", "ndjson")) {
      type <- "json"
    } else if (ext == "toml") {
      type <- "toml"
    } else {
      stop("Could not infer file type from path extension. Please specify type = 'json' or type = 'toml'")
    }
  }

  if (!type %in% c("json", "toml")) stop("Unsupported type '", type, "'. Supported types are: 'json', 'toml'")

  nested <- dataset_nest(dataset)

  if (type == "json") {
    jsonlite::write_json(nested, path, pretty = TRUE, auto_unbox = TRUE, na = "null")

  } else if (type == "toml") {
    if (!requireNamespace("tomledit", quietly = TRUE)) {
      stop("TOML writing requires the 'tomledit' package. Install with: install.packages('tomledit')")
    }
    tomledit::write_toml(nested, path)
  }

  invisible(nested)
}

#' Load Dataset from File
#'
#' Reads a dataset from a JSON or TOML file.
#'
#' @param path The file path from which to load the dataset.
#' @param type The file type, either "json" or "toml". If NULL, inferred from path extension.
#' @return A dataset object.
#' @export
#' @examples
#' \dontrun{
#' ds <- dataset_read("output.json")
#' ds <- dataset_read("output.toml")
#' }
dataset_read <- function(path, type = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)

  if (is.null(type)) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("json", "ndjson")) {
      type <- "json"
    } else if (ext == "toml") {
      type <- "toml"
    } else {
      stop("Could not infer file type from path extension. Please specify type = 'json' or type = 'toml'")
    }
  }

  if (!type %in% c("json", "toml")) stop("Unsupported type '", type, "'. Supported types are: 'json', 'toml'")

  if (type == "json") {
    nested <- jsonlite::read_json(path)
  } else if (type == "toml") {
    if (!requireNamespace("tomledit", quietly = TRUE)) {
      stop("TOML support requires the 'tomledit' package. Install with: install.packages('tomledit')")
    }
    nested <- tomledit::read_toml(path)
  }

  dataset_unnest(nested)
}
