
#' Precision, Decimal Places and Fractional Part of a Numeric Value
#'
#' Four small utilities for the written form of a number, as opposed to its
#' value.
#'
#' `nDec()` returns the number of decimal places of every element.\cr
#' `maxDec()` returns the largest of those numbers.\cr
#' `prec()` returns the precision, the smallest positional value of the last
#'               significant digit found in `x` (e.g. 0.001 for 3.142).\cr
#' `frac()` returns the fractional part.
#'
#' @name precision
#'
#' @details
#' `nDec()` and `maxDec()` count what is printed: the input is converted
#' with [as.character()], an exponent is discarded, and the digits behind the
#' last decimal separator are counted. A number that R chooses to print in
#' scientific notation therefore has no decimal places, `nDec(1e-300)` is `0`,
#' and trailing zeros of a numeric are gone before counting, as `1.50` and
#' `1.5` are the same number. Pass the values as character strings to count
#' them as written.
#'
#' Where R switches to scientific notation is R's decision, not this
#' function's, and it has moved between versions: up to R 4.2 [as.character()]
#' followed `options(scipen=)`, since R 4.3 it writes the shortest
#' representation that reads back as the same number. A value near that switch,
#' such as `0.00001`, may therefore count five decimals or none, depending on
#' the R version. Pass it as a character string to fix the count.
#'
#' Both a period and a comma are accepted as the decimal separator of a
#' character input, the last one in the string deciding, so that a thousands
#' separator does not distort the count. Numeric input always arrives with a
#' period, whatever `getOption("OutDec")` says.
#'
#' `maxDec(x)` is the maximum of `nDec(x)`, missing values removed, and `0`
#' when nothing is left to count.
#'
#' `prec()` works on the value rather than on its written form and reports the
#' position of the last significant digit across the whole vector, not one
#' value per element. For input that is exact in decimal it is
#' `10^-maxDec(x)`.
#'
#' `frac()` discards the sign, the fractional part of `-1.25` being `0.25`, as
#' the sign belongs to the integer part of the number. To read the decimals as
#' an integer, scale and round the result, `round(1e4 * frac(x))` for the first
#' four of them.
#'
#' @param x a numeric vector, or a character vector of numbers as written.
#'
#' @return
#' \itemize{
#'   \item `nDec()`: an integer vector of the same length as `x`;
#'     `NA` elements yield `NA`.
#'   \item `maxDec()`: a single integer value, `0` if `x` has no
#'     non-missing element with decimals.
#'   \item `prec()`: a single numeric value, the finest precision found
#'     across all (non-missing) elements of `x`. Returns 1 if all values
#'     are zero and `NA` if no non-missing values are left.
#'   \item `frac()`: a numeric vector of the same length as `x`.
#' }
#'
#' @examples
#'
#' x <- rnorm(5)*100
#' x
#' frac(x)
#'
#' # the first four decimal digits, as an integer
#' round(1e4 * frac(x))
#'
#' # the sign belongs to the integer part
#' frac(c(-1.25, 1.25))
#' ## [1] 0.25 0.25
#'
#' nDec(c(1.25, 1.8, 12.0, 1.00000))
#' ## [1] 2 1 0 0
#'
#' # the same numbers, summarised
#' maxDec(c(1.25, 1.8, 12.0, 1.00000))
#' ## [1] 2
#'
#' x <- c("0.0000", "0", "159.283", "1.45e+10", "1.4599E+10" )
#' nDec(x)
#' prec(as.numeric(x))
#'
#' # trailing zeros survive in a character input, but not in a numeric one
#' nDec("1.500")
#' ## [1] 3
#' nDec(1.500)
#' ## [1] 1
#'
#' @seealso [format.info()], [as.integer()], [trunc()]
#'
#' @family math.precision
#' @concept precision
#' @concept numerical-methods
#' @export
nDec <- function(x) {

  # possible alternative:  format.info
  #   [1] ... width (in characters) used by format(x),
  #   [2] ... number of digits after decimal point.
  #   [3] ... exponential representation

  isNAIn <- is.na(x)

  if (!inherits(x, "character"))
    x <- as.character(x)

  # remove exponents, if any
  x <- gsub(pattern = "[eE].+$", replacement = "", x = x)

  # as.character() writes a period whatever getOption("OutDec") says, while a
  # character input may carry either separator; the last one is the decimal
  # one, so a thousands separator does not distort the count
  pos <- regexpr("[.,][^.,]*$", x)

  res <- integer(length(x))
  # regexpr() returns NA for an NA element, which must not reach the subscript
  hasSep <- !is.na(pos) & pos > 0L
  res[hasSep] <- nchar(x[hasSep]) - pos[hasSep]

  res[isNAIn] <- NA_integer_

  res

}


#' @rdname precision
#' @export
maxDec <- function(x) {

  z <- nDec(x)
  z <- z[!is.na(z)]

  if (length(z) == 0L)
    0L
  else
    max(z)

}


#' @rdname precision
#' @export
prec <- function(x) {

  # Keep dividing by powers of 10, starting above the leading digit, until
  # no fractional part is left; that power is the position of the last
  # significant digit.

  # Thanks to Thomas Lumley for help with machine precision

  # a vector of NAs is logical, and it has a documented result here
  if (!(is.numeric(x) || all(is.na(x))))
    stop("'x' must be numeric")

  # the sign carries no precision information; log10 requires positives
  x <- abs(x[!is.na(x)])

  if (length(x) == 0L)
    return(NA_real_)

  if (max(x) == 0)
    return(1)

  pow <- trunc(log10(max(x))) + 1
  tol <- 0
  fracPart <- 1

  while (any(fracPart > tol)) {
    pow <- pow - 1
    scaled <- x * 10^(-pow)
    fracPart <- scaled - trunc(scaled)
    # what is left below the representation error of the largest element is
    # noise, not precision
    tol <- max(scaled) * .Machine$double.eps
  }

  10^pow

}


#' @rdname precision
#' @export
frac <- function(x) {

  # a vector of NAs is logical, and NAs travel through untouched
  if (!(is.numeric(x) || all(is.na(x))))
    stop("'x' must be numeric")

  # the sign belongs to the integer part
  abs(x) %% 1

}
