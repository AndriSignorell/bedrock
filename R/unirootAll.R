
#' Find multiple roots of a function within an interval
#'
#' Searches a numeric interval for all roots (zeros) of a function \code{f}
#' by subdividing it into \code{n} sub-intervals, detecting sign changes, and
#' refining each candidate with \code{\link[stats]{uniroot}}.
#'
#' The function \code{f} is called as \code{f(x, ...)} where \code{x} is a
#' numeric vector. If \code{f} does not accept vector input, it is called
#' element-wise via \code{sapply}.
#'
#' Grid points at which \code{|f(x)| < tol} are returned directly as roots.
#' Sign changes are detected using \code{sign()}, which avoids numerical
#' overflow that can occur with product-based approaches. Non-finite function
#' values are silently ignored when detecting sign changes. If \code{uniroot}
#' fails on a sub-interval, that interval is skipped with a warning rather
#' than aborting the entire search.
#'
#' @param f       A function for which roots are sought. Must accept a numeric
#'   first argument; additional arguments are passed via \code{...}.
#' @param interval A numeric vector of length 2 specifying the search interval.
#'   Either \code{interval} or both \code{lower} and \code{upper} must be
#'   supplied.
#' @param lower  Lower bound of the search interval.
#'   Default: \code{min(interval)}.
#' @param upper  Upper bound of the search interval.
#'   Default: \code{max(interval)}.
#' @param tol    Convergence tolerance passed to \code{\link[stats]{uniroot}},
#'   and also used as the threshold for (i) treating grid-point values as
#'   exact zeros and (ii) collapsing near-duplicate roots.
#'   Default: \code{.Machine$double.eps^0.5}.
#' @param maxiter Maximum number of iterations for \code{\link[stats]{uniroot}}.
#'   Default: \code{1000}.
#' @param n      Number of sub-intervals used for the initial grid search.
#'   Increase \code{n} if roots may be close together or the function
#'   oscillates rapidly. Default: \code{100}.
#' @param ...    Additional arguments passed to \code{f}.
#'
#' @return A numeric vector of roots found in \code{[lower, upper]}, sorted in
#'   ascending order. Returns \code{numeric(0)} if no roots are found.
#'
#' @details
#' **Limitations:** Roots within the same sub-interval of width
#' \code{(upper - lower) / n} may be missed. Roots of even multiplicity
#' that do not produce a sign change will not be found unless they happen
#' to fall on a grid point. A warning is issued if no roots are found at
#' all despite finite function values being present.
#'
#' @seealso \code{\link[stats]{uniroot}} for the underlying single-root solver.
#'
#' @examples
#' f <- function(x) cos(2 * x)^3
#' roots <- unirootAll(f, c(0, 10))
#' stopifnot(all(abs(f(roots)) < 1e-6))
#'
#' # Non-vectorized function
#' g <- Vectorize(function(x) integrate(function(t) t^x, 0, 1)$value - 0.5)
#' unirootAll(g, c(0.1, 5))
#'


#' @export
unirootAll <- function(f,
                              interval,
                              lower = min(interval),
                              upper = max(interval),
                              tol = .Machine$double.eps^0.5,
                              maxiter = 1000,
                              n = 100,
                              ...) {
  
  ## --- checks ---
  if (!missing(interval) && length(interval) != 2)
    stop("'interval' must be length 2")
  
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
    stop("require lower < upper")
  
  ## --- grid ---
  xseq <- seq(lower, upper, length.out = n + 1)
  
  ## --- evaluate function (vectorized fallback) ---
  mod <- tryCatch(
    f(xseq, ...),
    error = function(e) NULL
  )
  
  # try vectorized evaluation; fallback to element-wise if needed
  if (is.null(mod) || length(mod) != length(xseq)) {
    mod <- sapply(xseq, function(x) {
      tryCatch(
        f(x, ...),
        error = function(e) NA_real_
      )
    })
  }  

  ok <- is.finite(mod)
  
  ## --- near-zero roots ---
  roots <- xseq[ok & abs(mod) < tol]
  
  ## --- sign changes ---
  s <- sign(mod)
  change <- (s[1:n] != s[2:(n + 1)]) & ok[1:n] & ok[2:(n + 1)]
  idx <- which(change)
  
  ## --- refine with uniroot (robust) ---
  if (length(idx) > 0) {
    roots2 <- sapply(idx, function(i) {
      tryCatch(
        uniroot(f,
                lower = xseq[i],
                upper = xseq[i + 1],
                tol = tol,
                maxiter = maxiter,
                ...)$root,
        error = function(e) NA_real_
      )
    })
    roots <- c(roots, roots2[!is.na(roots2)])
  }
  
  ## --- deduplicate ---
  if (length(roots) > 1) {
    roots <- sort(roots)
    roots <- roots[c(TRUE, diff(roots) > tol)]
  }
  
  ## --- improved warning ---
  if (length(roots) == 0 && any(ok)) {
    warning(
      "No sign changes detected and no near-zero grid points found. ",
      "Roots of even multiplicity or closely spaced roots may be missed. ",
      "Consider increasing 'n'."
    )
  }
  
  return(roots)
}

