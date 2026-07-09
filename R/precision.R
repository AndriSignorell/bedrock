
#' Precision, Decimal Places and Fractional Part of a Numeric Value
#'
#' \code{nDec()} returns the number of decimals.\cr
#' \code{prec()} returns the precision of a number x, i.e. the smallest positional
#'               value of its last significant digit (e.g. 0.001 for 3.142).\cr
#' \code{frac()} returns the fractional part of a numeric value.\cr
#' \code{maxDigits()} returns the maximal number of decimal digits in \code{x}.
#'
#' @name precision
#'
#' @param x the numeric value (or a vector of numerics), whose fractional part
#' is to be calculated.
#' @param dpwr power of 10 for a factor z, the fractional part will be
#' multiplied with. The result will be returned rounded to integer. Defaults to
#' \code{NA} and will then be ignored.
#'
#' @return
#' \itemize{
#'   \item \code{nDec()}: an integer vector of the same length as \code{x};
#'     \code{NA} elements yield \code{NA}.
#'   \item \code{prec()}: a single numeric value, the finest precision found
#'     across all (non-missing) elements of \code{x}. Returns 1 if all values
#'     are zero and \code{NA} if no non-missing values are left.
#'   \item \code{frac()}: a numeric vector of the same length as \code{x}.
#'   \item \code{maxDigits()}: a single integer value.
#' }
#'
#' @seealso \code{\link{format.info}}, \code{\link{as.integer}},
#' \code{\link{trunc}}
#' @keywords arith
#' @examples
#'
#' x <- rnorm(5)*100
#' x
#' frac(x)
#'
#' # multiply by 10^4
#' frac(x, dpwr=4)
#'
#' maxDigits(c(1.25, 1.8, 12.0, 1.00000))
#'
#' x <- c("0.0000", "0", "159.283", "1.45e+10", "1.4599E+10" )
#' nDec(x)
#' prec(as.numeric(x))
#'
#' @family math.utils
#' @concept number-formatting
#' @export
nDec <- function(x) {

  # the decimal separator as used by as.character()/format()
  decSep <- gsub("1", "", format(1.1))

  # returns the number of decimal places in a number x

  # possible alternative:  format.info
  #   [1] ... width (in characters) used by format(x),
  #   [2] ... number of digits after decimal point.
  #   [3] ... exponential representation

  isNAIn <- is.na(x)

  if (!inherits(x, "character"))
    x <- as.character(x)

  res <- rep(0L, length(x))

  # remove exponents, if any
  x <- gsub(pattern = "[eE].+$", replacement = "", x = x)

  # locate the separator locale-consistently (a hardcoded "." would fail
  # for OutDec = ",")
  hasSep <- grep(decSep, x, fixed = TRUE)
  res[hasSep] <- nchar(x[hasSep]) -
    regexpr(decSep, x[hasSep], fixed = TRUE)

  res[isNAIn] <- NA_integer_

  return(res)

}


#' @rdname precision
#' @export
prec <- function(x) {

  # Function to return the most precise
  # digit from a vector of real numbers
  # Keep dividing by powers of 10 (pos and neg from trunc(log(max(x)) down)
  # until the fractional portion is zero, then we have the highest precision
  # digit in terms of a integer power of 10.

  # Thanks to Thomas Lumley for help with machine precision

  # the sign carries no precision information; log10 requires positives
  x <- abs(x[!is.na(x)])

  if (length(x) == 0L)
    return(NA_real_)

  if (max(x) == 0)
    return(1)

  init <- trunc(log10(max(x))) + 1
  zero <- 0
  y <- 1
  while (any(y > zero)) {
    init <- init - 1
    x1 <- x * 10^(-init)
    y <- x1 - trunc(x1)
    zero <- max(x1) * .Machine$double.eps
  }
  10^init

}


#' @rdname precision
#' @export
frac <- function(x, dpwr = NA) {  # fractional part
  res <- abs(x) %% 1
  # Alternative: res <- abs(x-trunc(x))
  if (!is.na(dpwr)) res <- round(10^dpwr * res)
  res
}


#' @rdname precision
#' @export
maxDigits <- function(x) {
  # maximal number of decimal digits;
  # note that as.character() honours getOption("OutDec")
  z <- na.omit(unlist(
    lapply(strsplit(as.character(x),
                    split = getOption("OutDec"), fixed = TRUE),
           "[", 2)))
  if (length(z) == 0)
    res <- 0L
  else
    res <- max(nchar(z))

  return(res)

}
