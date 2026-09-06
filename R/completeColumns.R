
#' Identify Columns Without Missing Values
#'
#' This function checks each element of a data frame or list-like object
#' for missing values (`NA`) and identifies those that are completely
#' observed, i.e., contain no missing entries.
#'
#' An element is considered *complete* if it contains zero missing
#' values. Internally, the function uses [anyNA()] to detect
#' missing values.
#'
#' @param x a data.frame or list-like object whose elements are checked
#'   for missing values.
#' @param output character string specifying the output representation.
#'   One of `"names"` (return the names of the complete elements,
#'   the default) or `"logical"` (return a logical vector indicating
#'   completeness for each element).
#'
#' @return
#' if `output = "names"`, a character vector with the names of all
#' complete elements.
#'
#' If `output = "logical"`, a logical vector of length
#' `length(x)`, where `TRUE` indicates that the corresponding
#' element contains no missing values.
#'
#' @seealso [anyNA()], [is.na()],
#'   [na.omit()], [complete.cases()]
#'
#' @examples
#' # Names of columns without missing values
#' completeColumns(airquality)
#'
#' # Logical vector indicating completeness
#' completeColumns(airquality, output = "logical")
#'
#' @family data.missing
#' @concept missing-value
#' @concept data-inspection
#' @export
completeColumns <- function(x, output = c("names", "logical")) {

  if (!is.list(x))
    stop("'x' must be a data.frame or list-like object")

  output <- match.arg(output)

  ok <- !vapply(x, anyNA, logical(1))

  nm <- names(x)
  if (is.null(nm)) nm <- as.character(seq_along(x))

  switch(output,
         names   = nm[ok],
         logical = ok)
}
