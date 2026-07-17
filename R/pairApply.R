
#' Pairwise Calculations
#'
#' Implements a logic to run pairwise calculations on the columns of a
#' data.frame or a matrix.
#'
#' This code is based on the logic of \code{cor()} and extended for asymmetric
#' functions. Cell \code{[i, j]} of the result contains
#' \code{FUN(x[[i]], x[[j]], ...)}, so the first argument of \code{FUN}
#' corresponds to the row variable and the second to the column variable.
#'
#' @param x a list, a data.frame or a matrix with columns to be processed
#' pairwise.
#' @param FUN a function (or the name of a function) to be calculated. It is
#' assumed, that the first 2 arguments denominate x and y, and that it
#' returns a single numeric value.
#' @param \dots the dots are passed to FUN.
#' @param symmetric logical. Does the function yield the same result for FUN(x,
#' y) and FUN(y, x)? \cr If \code{TRUE} just the lower triangular matrix is
#' calculated and mirrored. Default is FALSE.
#'
#' @return a matrix with the results of FUN.
#'
#' @seealso [base::outer()], [stats::pairwise.table]
#' @keywords manip
#' @examples
#'
#' # build a dataset
#' set.seed(1)
#' d.sub <- transform(
#'   data.frame(
#'     X1 = rnorm(n <- 300),
#'     X3 = rnorm(n)),
#'   X2 = 0.8*X1 + rnorm(n),
#'   X4 = 0.5*X3 + rnorm(n)
#'   )
#'
#' pairApply(d.sub, FUN = cor, method="spearman")
#'
#' # user defined functions are ok as well
#' pairApply(d.sub,
#'   FUN = function(x,y)
#'     wilcox.test(as.numeric(x), as.numeric(y))$p.value, symmetric=TRUE)
#'
#' @family combinatorics
#' @concept combinatorics
#' @concept programming
#' @export
pairApply <- function(x, FUN = NULL, ..., symmetric = FALSE) {

  # match.fun resolves both function objects and function names,
  # replacing the former eval(parse(...)) construction
  FUN <- match.fun(FUN)

  if (is.matrix(x)) x <- as.data.frame(x)
  x <- as.list(x)

  n <- length(x)
  pp <- matrix(NA_real_, n, n, dimnames = list(names(x), names(x)))

  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      pp[i, j] <- FUN(x[[i]], x[[j]], ...)
    }
  }

  if (symmetric) {
    pp[upper.tri(pp)] <- t(pp)[upper.tri(pp)]
  } else {
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (j > i) {
          pp[i, j] <- FUN(x[[i]], x[[j]], ...)
        }
      }
    }
  }

  return(pp)
}
