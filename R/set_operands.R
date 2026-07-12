#' @export
#' @rdname dataset_minus
"-.dataset" <- function(a, b) dataset_minus(a, b)

#' @export
#' @rdname dataset_intersect
">.dataset" <- function(a, b) dataset_intersect(a, b)

#' @export
#' @rdname dataset_union
"+.dataset" <- function(a, b) dataset_union(a, b)

#' @export
#' @rdname dataset_equality
"==.dataset" <- function(a, b) dataset_equality(a, b)
