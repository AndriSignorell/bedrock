
#' Resolve a Contingency Table
#'
#' Brings a two-way classification into one canonical shape, no matter whether
#' it arrives as a ready-made contingency table or as two classification
#' variables. The function validates the counts, drops the incomplete
#' observations and reports the table together with its dimensions, so that
#' association measures, tests of independence and agreement statistics can
#' share one entry point instead of each repeating the same preparation.
#'
#' Any two-dimensional object is taken as a contingency table and used as it
#' is, which covers a matrix as well as a [table()] or [xtabs()] object; a data
#' frame of counts is coerced with [as.matrix()]. Its entries must be numeric,
#' non-negative and finite; non-integer counts are reported with a warning
#' unless `integerCounts` is set to `FALSE`, as they occur legitimately in
#' weighted or expected tables. An array of any other number of dimensions is
#' an error, rather than being flattened into a classification variable.
#'
#' Two classification variables are cross-tabulated instead. Observations
#' missing in either variable are dropped, both variables are then coerced to
#' factors, which drops the levels that no longer occur, and at least two
#' levels must remain on each side.
#'
#' Whichever way the table arrives, it must have at least two rows and two
#' columns: a one-way table carries no association to measure and is rejected
#' rather than passed on to a caller that cannot use it.
#'
#' `square` is meant for the statistics that compare two ratings of the same
#' items, such as the tests of marginal homogeneity or the agreement measures.
#' It guarantees that the table has as many columns as rows, and nothing
#' beyond that: whether the two axes really carry the same categories cannot be
#' checked on a table that may have no `dimnames` at all, and remains the
#' responsibility of the caller.
#'
#' @param x a contingency table or matrix of counts, or a factor or vector of
#'   classifications.
#' @param y an optional factor or vector of classifications, of the same length
#'   as `x`. Required unless `x` is a table, ignored when it is.
#' @param square logical, whether a square contingency table is required,
#'   defaults to `FALSE`.
#' @param integerCounts logical, whether non-integer counts should be reported
#'   with a warning, defaults to `TRUE`.
#' @param dataName optional character string used as the `dataName` entry of
#'   the result. If `NULL` (default), it is derived from the unevaluated
#'   arguments. That name only reflects what `resolveContingency()` itself
#'   sees: a function calling it internally should build its own name from
#'   [substitute()] at its own call site and pass it through here, as it would
#'   otherwise report its own formal argument names, typically `"x and y"`,
#'   instead of the names the end user typed.
#'
#' @return a list containing:
#' \describe{
#'   \item{table}{the contingency table.}
#'   \item{n}{the total sample size, the sum of all counts.}
#'   \item{r}{integer, the number of rows.}
#'   \item{c}{integer, the number of columns.}
#'   \item{dataName}{character description of the input, for use as the
#'     `data.name` of an `htest` object.}
#' }
#'
#' @examples
#' # from an existing contingency table
#' tab <- matrix(c(10, 5, 3, 12), nrow = 2,
#'               dimnames = list(c("A", "B"), c("yes", "no")))
#' str(resolveContingency(tab))
#'
#' # from two classification variables
#' set.seed(1)
#' x <- sample(c("low", "high"), 100, replace = TRUE)
#' y <- sample(c("yes", "no"), 100, replace = TRUE)
#' resolveContingency(x, y)$table
#'
#' # a caller passes the name it sees at its own call site
#' myTest <- function(x, y) {
#'   r <- resolveContingency(x, y,
#'                           dataName = paste(deparse1(substitute(x)), "and",
#'                                            deparse1(substitute(y))))
#'   r$dataName
#' }
#' myTest(x, y)
#' ## [1] "x and y"
#'
#' @seealso [table()], [resolveGroups()], [resolveFormula()]
#'
#' @family data.resolve
#' @concept data-resolution
#' @concept table
#'
#' @export
resolveContingency <- function(
    x,
    y = NULL,
    square = FALSE,
    integerCounts = TRUE,
    dataName = NULL
) {
  
  checkFlag(square)
  checkFlag(integerCounts)
  
  if (!is.null(dataName))
    checkString(dataName)
  
  # a two-dimensional object is a table, everything else a classification
  if (!is.null(dim(x))) {
    
    # --- the name must be taken before x is reassigned, as substitute()
    # --- returns the value once x is no longer a promise ------------------
    dname <- if (is.null(dataName)) deparse1(substitute(x)) else dataName
    
    if (length(dim(x)) != 2L)
      stop("'x' must be a two-dimensional table")
    
    if (is.data.frame(x))
      x <- as.matrix(x)
    
    # --- numeric check must come before any arithmetic on x ---------------
    if (!is.numeric(x))
      stop("'x' must be a numeric matrix")
    
    if (any(x < 0, na.rm = TRUE) || any(!is.finite(x)))
      stop("all entries of 'x' must be nonnegative and finite")
    
    if (integerCounts && any(x != round(x)))
      warning("'x' contains non-integer counts", call. = FALSE)
    
    tab <- x
    
  } else {
    
    # --- NULL check must come before length comparison --------------------
    if (is.null(y))
      stop("if 'x' is not a table, 'y' must be given")
    
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    
    dname <- if (is.null(dataName)) {
      paste(
        deparse1(substitute(x)),
        "and",
        deparse1(substitute(y))
      )
    } else dataName
    
    ok <- complete.cases(x, y)
    x <- factor(x[ok])
    y <- factor(y[ok])
    
    if (nlevels(x) < 2L || nlevels(y) < 2L)
      stop("'x' and 'y' must each have at least 2 levels")
    
    tab <- table(x, y)
  }
  
  if (nrow(tab) < 2L || ncol(tab) < 2L)
    stop("contingency table must have at least two rows and columns")
  
  if (square && nrow(tab) != ncol(tab))
    stop("'x' must be a square contingency table")
  
  list(
    table    = tab,
    n        = sum(tab),
    r        = nrow(tab),
    c        = ncol(tab),
    dataName = dname
  )
}
