
#' Resolve Grouped Data
#'
#' Standardizes grouped data supplied either as a numeric vector with
#' grouping variable or as a list of group-specific vectors.
#'
#' Missing observations are removed and grouping information is returned
#' in a consistent format suitable for hypothesis tests, summaries,
#' effect-size calculations and plotting functions.
#'
#' @param x a numeric vector of observations, or a list of numeric vectors.
#' @param g a grouping variable of the same length as \code{x}.
#'   Ignored when \code{x} is a list.
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{numeric response vector}
#'   \item{g}{grouping factor}
#'   \item{n}{total number of observations}
#'   \item{k}{number of groups}
#'   \item{group.sizes}{group sample sizes}
#'   \item{group.names}{group labels}
#'   \item{data.name}{character description of the input}
#' }
#'
#' @family data.utils
#' @concept grouped-data
#' @concept data-manipulation
#'


#' @export
resolveGroups <- function(x, g) {
  
  if (is.list(x)) {
    
    if (length(x) < 2L)
      stop("'x' must contain at least two groups")
    
    if (!missing(g))
      warning("'x' is a list, so ignoring argument 'g'")
    
    dname <- deparse1(substitute(x))
    
    x <- lapply(
      x,
      function(z) z[complete.cases(z)]
    )
    
    if (!all(vapply(x, is.numeric, logical(1L))))
      warning(
        "some elements of 'x' are not numeric and will be coerced to numeric"
      )
    
    group.sizes <- lengths(x)
    
    if (any(group.sizes == 0L))
      stop("all groups must contain observations")
    
    g <- factor(
      rep.int(seq_along(x), group.sizes)
    )
    
    if (!is.null(names(x)))
      levels(g) <- names(x)
    
    x <- unlist(x, use.names = FALSE)
    
  } else {
    
    if (missing(g))
      stop("'g' is missing")
    
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    
    dname <- paste(
      deparse1(substitute(x)),
      "and",
      deparse1(substitute(g))
    )
    
    ok <- complete.cases(x, g)
    
    x <- x[ok]
    g <- factor(g[ok])
    
    n <- length(x)
    
    if (n < 2L)
      stop("not enough observations")
    
    if (nlevels(g) < 2L)
      stop("all observations are in the same group")
    
    group.sizes <- table(g)
  }
  
  n <- length(x)
  
  if (n < 2L)
    stop("not enough observations")
  
  list(
    x = x,
    g = g,
    n = n,
    k = nlevels(g),
    group.sizes = group.sizes,
    group.names = levels(g),
    data.name = dname
  )
  
}

