
#' Resolve Contingency Table
#'
#' Converts either a contingency table or two classification variables
#' into a standardized contingency-table representation.
#'
#' If \code{x} is a matrix, it is interpreted as a contingency table.
#' Otherwise \code{x} and \code{y} are converted to factors and a table
#' is constructed after removing incomplete observations.
#'
#' @param x a contingency table, factor or vector.
#' @param y an optional factor or vector.
#' Ignored when \code{x} is a matrix.
#' @param square logical indicating whether a square contingency table
#' is required.
#' @param integerCounts logical; if \code{TRUE} (default) a warning is
#' issued when the table contains non-integer counts.
#' @param data.name optional character string used as the \code{data.name}
#' entry of the result. If \code{NULL} (default), it is derived from
#' \code{deparse(substitute(x))} (and \code{y}). This only reflects the
#' variable names as seen by \code{resolveContingency()} itself: functions
#' that call \code{resolveContingency()} internally should build their own
#' \code{data.name} via \code{deparse(substitute())} at their own call site
#' and pass it through here, otherwise the reported name will be the formal
#' argument names of the calling function (e.g. \code{"x and y"}) rather
#' than the names the end user actually typed.
#'
#' @return A list containing:
#' \describe{
#'   \item{table}{contingency table}
#'   \item{n}{total sample size}
#'   \item{r}{number of rows}
#'   \item{c}{number of columns}
#'   \item{k}{number of rows (alias; convenient for square tables)}
#'   \item{data.name}{name of the data}
#' }
#'
#' @family data.utils
#' @concept formula
#' @concept frequency-table
#' @export
resolveContingency <- function(
    x,
    y = NULL,
    square = FALSE,
    integerCounts = TRUE,
    data.name = NULL
) {
  if (is.matrix(x)) {
    
    # --- numeric check must come before any arithmetic on x ---------------
    if (!is.numeric(x))
      stop("'x' must be a numeric matrix")
    
    if (any(x < 0, na.rm = TRUE) || any(!is.finite(x)))
      stop("all entries of 'x' must be nonnegative and finite")
    
    if (integerCounts && any(x != round(x)))
      warning("'x' contains non-integer counts", call. = FALSE)
    
    DNAME <- if (is.null(data.name)) deparse1(substitute(x)) else data.name
    tab <- x
    
  } else {
    
    # --- NULL check must come before length comparison --------------------
    if (is.null(y))
      stop("if 'x' is not a matrix, 'y' must be given")
    
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    
    DNAME <- if (is.null(data.name)) {
      paste(
        deparse1(substitute(x)),
        "and",
        deparse1(substitute(y))
      )
    } else data.name
    
    ok <- complete.cases(x, y)
    x <- factor(x[ok])
    y <- factor(y[ok])
    
    if (nlevels(x) < 2L || nlevels(y) < 2L)
      stop("'x' and 'y' must each have at least 2 levels")
    
    tab <- table(x, y)
  }
  
  if (square) {
    if (nrow(tab) != ncol(tab))
      stop("'x' must be a square contingency table")
    if (nrow(tab) < 2L)
      stop("'x' must be square with at least two rows and columns")
  }
  
  list(
    table     = tab,
    n         = sum(tab),
    r         = nrow(tab),
    c         = ncol(tab),
    k         = nrow(tab),   # alias; convenient for square tables
    data.name = DNAME
  )
}


