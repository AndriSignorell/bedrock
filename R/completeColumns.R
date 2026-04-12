
#' Identify Columns Without Missing Values
#'
#' This function checks each column of a data frame for missing values
#' (`NA`) and identifies those columns that are completely observed,
#' i.e., contain no missing entries.
#'
#' Depending on the argument \code{which}, the function either returns
#' the names of such columns or a logical vector indicating completeness
#' for each column.
#'
#' @param x A \code{data.frame} containing the data to be checked.
#' @param which Logical. If \code{TRUE} (default), the function returns
#'   the names of columns without missing values. If \code{FALSE}, a
#'   logical vector is returned, where each element corresponds to a
#'   column in \code{x} and indicates whether that column is complete.
#'
#' @details
#' A column is considered *complete* if it contains zero missing values.
#' Internally, the function counts the number of \code{NA}s per column
#' using \code{\link{is.na}} and selects those with a count of zero.
#'
#' @return
#' If \code{which = TRUE}: a character vector with the names of all
#' complete columns.
#'
#' If \code{which = FALSE}: a logical vector of length \code{ncol(x)},
#' where \code{TRUE} indicates that the corresponding column contains
#' no missing values.
#'
#' @seealso \code{\link{is.na}}, \code{\link{na.omit}},
#'   \code{\link{complete.cases}}
#'
#' @examples
#' # Names of columns without missing values
#' completeColumns(d.pizza)
#'
#' # Logical vector indicating completeness
#' completeColumns(d.pizza, which = FALSE)
#'


#' @export
completeColumns <- function(x, which=TRUE){
  if(which)
    names(Filter(isZero, sapply(x, function(z) sum(is.na(z)))))
  else 
    sapply(x, function(z) sum(is.na(z)))==FALSE
}

