
#' Extract Dichotomous (Binary) Variables
#'
#' Identifies and extracts dichotomous (binary) variables from a data frame
#' or matrix using \code{isDichotomous()}.
#'
#' @param x A data frame or matrix.
#' @param strict Logical; if \code{TRUE}, only variables with exactly two
#'   distinct values are considered dichotomous. If \code{FALSE}, variables
#'   with one or two distinct values are allowed. Default is \code{FALSE}.
#' @param na.rm Logical; should missing values be ignored when checking
#'   for dichotomous variables? Default is \code{FALSE}.
#' @param return Character string specifying the return format:
#'   \itemize{
#'     \item \code{"data"}: subset of \code{x} containing only dichotomous variables (default)
#'     \item \code{"names"}: names of dichotomous variables
#'     \item \code{"index"}: column indices of dichotomous variables
#'     \item \code{"logical"}: logical vector indicating dichotomous variables
#'   }
#'
#' @return Depending on \code{return}:
#' \itemize{
#'   \item \code{"data"}: data frame or matrix
#'   \item \code{"names"}: character vector
#'   \item \code{"index"}: integer vector
#'   \item \code{"logical"}: logical vector
#' }
#'
#' @details
#' Variables with only missing values are not considered dichotomous when
#' \code{na.rm = FALSE}. When \code{na.rm = TRUE}, such variables are treated
#' as empty vectors and are considered dichotomous only if \code{strict = FALSE}.
#'
#' @seealso \code{\link{isDichotomous}}
#'
#' @examples
#' dat <- data.frame(
#'   a = c(0, 1, 1, 0),
#'   b = c(1, 2, 3, 4),
#'   c = c(TRUE, FALSE, TRUE, TRUE),
#'   d = c(NA, NA, NA, NA)
#' )
#'
#' flags(dat)
#'
#' # Effect of na.rm:
#' flags(dat, na.rm = TRUE)
#'


#' @family data.inspection
#' @concept data-inspection
#' @concept data-manipulation
#' @concept factor-handling
#'
#'
#' @export
flags <- function(x,
                  strict = FALSE,
                  na.rm = FALSE,
                  return = c("data", "names", "index", "logical")) {
  
  output <- match.arg(return)
  
  if (!is.data.frame(x) && !is.matrix(x))
    stop("x must be a data.frame or matrix")
  
  # Column-wise dichotomous check
  if (is.matrix(x)) {
    is_flag <- apply(x, 2, isDichotomous,
                     strict = strict, na.rm = na.rm)
  } else {
    is_flag <- vapply(
      x,
      isDichotomous,
      logical(1L),
      strict = strict,
      na.rm = na.rm
    )
  }
  
  out <- switch(output,
                data    = x[, is_flag, drop = FALSE],
                names   = colnames(x)[is_flag],
                index   = which(is_flag),
                logical = is_flag
  )
  
  return(out)
}


