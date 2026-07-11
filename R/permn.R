
#' Set of Permutations
#'
#' Returns all distinct permutations of a vector. Repeated values in `x`
#' are treated as indistinguishable, so duplicated permutations are not returned.
#'
#' @param x atomic vector. Missing values are not supported.
#' @param sortResults logical scalar. If `TRUE`, the result matrix is sorted
#'   using [sortX()]. Default is `FALSE`.
#'
#' @return A matrix containing all distinct permutations of `x`, one
#'   permutation per row.
#'
#' @examples
#' permn(letters[2:5])
#' permn(2:5)
#'
#' # repeated elements are handled as indistinguishable
#' permn(c("a", "b", "c", "a"))
#'
#' @seealso [utils::combn()], [base::factorial()]
#'
#' @family combinatorics
#' @concept combinatorics
#' @concept number-theory
#' @export
permn <- function(x, sortResults = FALSE) {
  
  if (!is.atomic(x))
    stop("'x' must be an atomic vector")
  
  if (is.factor(x))
    stop("factors are not supported; use as.character(x) or as.numeric(x)")
  
  if (!is.logical(sortResults) || length(sortResults) != 1L || is.na(sortResults))
    stop("'sortResults' must be TRUE or FALSE")
  
  if (length(x) == 0L)
    return(matrix(x, nrow = 1L, ncol = 0L))
  
  if (anyNA(x))
    stop("missing values are not supported")
  
  tbl  <- table(x)
  cnts <- as.integer(tbl)
  n    <- sum(cnts)
  
  log_nperm <- lfactorial(n) - sum(lfactorial(cnts))
  if (log_nperm > log(.Machine$integer.max))
    stop(sprintf("too many permutations (%s); reduce input size",
                 format(round(exp(log_nperm)), big.mark = "'")))
  
  vals <- names(tbl)

  res <- .permnInner(vals, cnts)
  
  # map character keys back to original type via first occurrence in x
  lookup <- x[match(vals, as.character(x))]
  res    <- matrix(lookup[match(res, vals)], nrow = nrow(res))

  if (sortResults) res <- sortX(res)
  
  res
}



# == internal helper functions =================================================

.permnInner <- function(vals, cnts) {
  n <- sum(cnts)
  k <- length(vals)
  
  if (k == 0L)
    return(matrix(character(0), nrow = 1L, ncol = 0L))
  
  if (k == 1L)
    return(matrix(rep(vals, cnts), nrow = 1L))
  
  if (k == 2L) {
    combs  <- combn(n, cnts[1L])
    result <- matrix(rep(vals[2L], n), nrow = ncol(combs), ncol = n)
    idx    <- cbind(as.vector(col(combs)), as.vector(combs))
    result[idx] <- vals[1L]
    return(result)
  }
  
  fvr_perms  <- .permnInner(c("1", "0"), c(cnts[1L], n - cnts[1L]))
  rest_perms <- .permnInner(vals[-1L], cnts[-1L])
  
  nr_fvr  <- nrow(fvr_perms)
  nr_rest <- nrow(rest_perms)
  result  <- matrix(vals[1L], nrow = nr_fvr * nr_rest, ncol = n)
  
  for (i in seq_len(nr_fvr)) {
    rest_pos <- which(fvr_perms[i, ] == "0")
    rows     <- ((i - 1L) * nr_rest + 1L):(i * nr_rest)
    result[rows, rest_pos] <- rest_perms
  }
  result
}

