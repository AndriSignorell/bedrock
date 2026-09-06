
#' Extract Dichotomous (Binary) Variables
#'
#' Identify and extract dichotomous (binary) variables from a data frame
#' or matrix using `isDichotomous()`.
#'
#' Variables with only missing values are not considered dichotomous
#' when `na.rm = FALSE`.
#'
#' When `na.rm = TRUE`, such variables are treated as empty vectors
#' and are considered dichotomous only if `strict = FALSE`.
#'
#' Internally, variables with indeterminate dichotomous status
#' (i.e. `NA` returned by `isDichotomous()`)
#' are treated as non-dichotomous for filtering purposes.
#'
#' @param x a data frame or matrix.
#' @param strict logical. If `TRUE`, only variables with exactly two
#'   distinct values are considered dichotomous. If `FALSE` (default),
#'   variables with one or two distinct values are allowed.
#' @param na.rm logical. Should missing values be ignored when checking
#'   for dichotomous variables? Default is `FALSE`.
#' @param output character string specifying the output representation.
#'   One of `"data"` (subset of `x` containing only dichotomous
#'   variables, the default), `"names"` (names of dichotomous
#'   variables), `"index"` (column indices) or `"logical"`
#'   (logical vector indicating dichotomous variables).
#'
#' @return
#' depending on `output`:
#'
#' \itemize{
#'   \item `"data"`: data frame or matrix.
#'   \item `"names"`: character vector.
#'   \item `"index"`: integer vector.
#'   \item `"logical"`: logical vector.
#' }
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
#' @family data.predicate
#' @concept binary
#' @concept data-inspection
#' @export
flags <- function(x, strict = FALSE, na.rm = FALSE,
                  output = c("data", "names", "index", "logical")) {

  output <- match.arg(output)

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
