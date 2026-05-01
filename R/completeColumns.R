
#' Identify Columns Without Missing Values
#'
#' This function checks each element of a data frame or list-like object
#' for missing values (`NA`) and identifies those that are completely
#' observed, i.e., contain no missing entries.
#'
#' Depending on the argument \code{which}, the function either returns
#' the names of such elements or a logical vector indicating completeness
#' for each element.
#'
#' @param x A data.frame or list-like object whose elements are checked
#'   for missing values.
#' @param which Logical. If \code{TRUE} (default), the function returns
#'   the names of elements without missing values. If \code{FALSE}, a
#'   logical vector is returned, where each element corresponds to an
#'   element in \code{x} and indicates whether it is complete.
#'
#' @details
#' An element is considered *complete* if it contains zero missing values.
#' Internally, the function uses \code{\link{anyNA}} to detect missing values.
#'
#' @return
#' If \code{which = TRUE}, a character vector with the names of all
#' complete elements.
#'
#' If \code{which = FALSE}, a logical vector of length \code{length(x)},
#' where \code{TRUE} indicates that the corresponding element contains
#' no missing values.
#'
#' @seealso \code{\link{anyNA}}, \code{\link{is.na}},
#'   \code{\link{na.omit}}, \code{\link{complete.cases}}
#'
#' @examples
#' # Names of columns without missing values
#' completeColumns(d.pizza)
#'
#' # Logical vector indicating completeness
#' completeColumns(d.pizza, which = FALSE)
#'


#' @family data.inspection
#' @concept data-inspection
#' @concept missing-data
#'
#'
#' @export
completeColumns <- function(x, which = TRUE) {
  
  if (!is.list(x))
    stop("'x' must be a data.frame or list-like object")
  
  ok <- !vapply(x, anyNA, logical(1))
  
  nm <- names(x)
  if (is.null(nm)) nm <- as.character(seq_along(x))
  
  if (which) nm[ok] else ok
}

