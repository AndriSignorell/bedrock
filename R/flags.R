
#' Extract Dichotomous (Binary) Variables
#'
#' Identify and extract dichotomous (binary) variables from a data frame
#' or matrix using \code{isDichotomous()}.
#'
#' @param x A data frame or matrix.
#'
#' @param strict Logical.
#'
#'   If \code{TRUE}, only variables with exactly two
#'   distinct values are considered dichotomous.
#'
#'   If \code{FALSE}, variables with one or two
#'   distinct values are allowed.
#'
#'   Default is \code{FALSE}.
#'
#' @param na.rm Logical.
#'
#'   Should missing values be ignored when checking
#'   for dichotomous variables?
#'
#'   Default is \code{FALSE}.
#'
#' @param output Character string specifying the output representation.
#'
#'   One of:
#'
#'   \describe{
#'     \item{\code{\"data\"}}{
#'       Subset of \code{x} containing only dichotomous variables.
#'     }
#'
#'     \item{\code{\"names\"}}{
#'       Names of dichotomous variables.
#'     }
#'
#'     \item{\code{\"index\"}}{
#'       Column indices of dichotomous variables.
#'     }
#'
#'     \item{\code{\"logical\"}}{
#'       Logical vector indicating dichotomous variables.
#'     }
#'   }
#'
#'   Default is \code{\"data\"}.
#'
#' @return
#' Depending on \code{output}:
#'
#' \itemize{
#'   \item \code{\"data\"}: data frame or matrix
#'   \item \code{\"names\"}: character vector
#'   \item \code{\"index\"}: integer vector
#'   \item \code{\"logical\"}: logical vector
#' }
#'
#' @details
#' Variables with only missing values are not considered dichotomous
#' when \code{na.rm = FALSE}.
#'
#' When \code{na.rm = TRUE}, such variables are treated as empty vectors
#' and are considered dichotomous only if \code{strict = FALSE}.
#'
#' Internally, variables with indeterminate dichotomous status
#' (i.e. \code{NA} returned by \code{isDichotomous()})
#' are treated as non-dichotomous for filtering purposes.
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
#' # effect of na.rm
#' flags(dat, na.rm = TRUE)
#'
#' # return variable names
#' flags(dat, output = "names")
#'
#' # return column indices
#' flags(dat, output = "index")
#'
#' @family data.inspection
#' @concept data-inspection
#' @concept data-manipulation
#' @concept factor-handling
#'


#' @export
flags <- function(
    x,
    strict = FALSE,
    na.rm = FALSE,
    output = "data"
) {
  
  output <- match.arg(
    output,
    choices = c("data", "names", "index", "logical")
  )
  
  if (!is.data.frame(x) && !is.matrix(x))
    stop("Argument 'x' must be a data.frame or matrix.")
  
  isFlag <- vapply(
    seq_len(ncol(x)),
    function(i)
      isDichotomous(
        x[, i],
        strict = strict,
        na.rm = na.rm
      ),
    logical(1L)
  )
  
  names(isFlag) <- colnames(x)
  
  # Variables with indeterminate dichotomous status
  # are treated as non-dichotomous for filtering
  isFlag[is.na(isFlag)] <- FALSE
  
  switch(
    output,
    
    data = x[, isFlag, drop = FALSE],
    
    names = names(isFlag)[isFlag],
    
    index = which(isFlag),
    
    logical = isFlag
  )
}

