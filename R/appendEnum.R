
#' Add an Enumeration Column
#'
#' Prepends (or inserts) a column of enumeration labels -- lowercase or
#' uppercase Roman numerals, or Arabic numbers.
#'
#' @param x a data.frame or matrix (vectors are coerced via \code{matrix()}).
#' @param type enumeration style: \code{"roman-lcase"}, \code{"roman-ucase"}
#'   or \code{"arabic"}.
#' @param suffix text appended to each enumeration label.
#' @param startWith first enumeration index.
#' @param after position after which the column is inserted (see
#'   \code{\link{appendX}}); default \code{0L} prepends it.
#' @param colName optional name for the new column; \code{NULL} (default)
#'   leaves it unnamed for matrices, while for data frames a default name
#'   (\code{"V1"}) is used.
#'
#' @return \code{x} with an additional enumeration column.
#'
#' @examples
#' d <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' appendEnum(d)
#'
#' appendEnum(d, type = "arabic", suffix = ") ")
#'
#' # insert after the first column instead of prepending
#' appendEnum(d, after = 1L, colName = "no")
#'
#' @seealso \code{\link{append}}
#' 
#' @family data.append
#' @concept table
#' @concept append
#' @export
appendEnum <- function(x, type = c("roman-lcase", "roman-ucase", "arabic"),
                       suffix = ". ", startWith = 1L, after = 0L,
                       colName = NULL) {

  if (!is.matrix(x) && !is.data.frame(x))
    x <- matrix(x)

  type <- match.arg(type)
  i <- seq_len(nrow(x)) + (startWith - 1L)

  labels <- switch(type,
                   "roman-lcase" = paste0(tolower(as.roman(i)), suffix),
                   "roman-ucase" = paste0(as.roman(i), suffix),
                   "arabic"      = paste0(i, suffix))

  appendX(x, labels, after = after, newNames = colName)
}
