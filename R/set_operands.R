#' @export
#' @rdname dataset_minus
"-.dataset" <- function(a, b) dataset_minus(a,b)

#' @export
#' @rdname dataset_intersect
">.dataset" <- function(a, b) dataset_intersect(a,b)

#' @export
#' @rdname dataset_union
"+.dataset" <- function(a, b) dataset_union(a,b)

#' @export
#' @rdname dataset_equality
"==.dataset" <- function(a, b) dataset_equality(a,b)

# this currently breaks the lib. as some of the datasets are filtered with
# dataset filter logic. i think it is a o k to not do this.
# 
# #' @export
# #' @rdname dataset_filter
# #' @method dplyr::filter dataset
# "filter.dataset" <- function(.data, ...) dataset_filter(.data, ...)
