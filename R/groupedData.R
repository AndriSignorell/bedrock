
#' Construct Grouped Data
#'
#' Converts either a numeric vector and grouping variable, or a list of
#' group-specific vectors, into a standardized grouped-data representation.
#'
#' This helper is intended for functions operating on grouped observations,
#' such as rank-based tests, grouped summaries, effect-size calculations,
#' or plotting functions.
#'
#' Missing observations are removed using \code{\link{complete.cases}}.
#'
#' @param x a numeric vector of observations, or a list of numeric vectors.
#'   If a list is supplied, each element represents one group.
#' @param g a grouping variable of the same length as \code{x}.
#'   Ignored when \code{x} is a list.
#'
#' @return A list with components:
#' \describe{
#'   \item{x}{numeric response vector}
#'   \item{g}{grouping factor}
#'   \item{n}{total number of observations}
#'   \item{k}{number of groups}
#'   \item{group.sizes}{number of observations per group}
#'   \item{group.names}{group labels}
#' }
#'
#' @details
#' If \code{x} is a list:
#' \itemize{
#'   \item each element is treated as one group,
#'   \item missing values are removed within groups,
#'   \item a grouping factor is created automatically.
#' }
#'
#' If \code{x} is a vector:
#' \itemize{
#'   \item \code{x} and \code{g} must have identical lengths,
#'   \item incomplete observations are removed,
#'   \item \code{g} is coerced to a factor.
#' }
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 6)
#' g <- c("A", "A", "B", "B", "C", "C")
#'
#' groupedData(x, g)
#'
#' groupedData(list(
#'   A = c(1, 2),
#'   B = c(3, 4),
#'   C = c(5, 6)
#' ))
#'
#' @family data.utils
#' @concept data-manipulation
#' @concept grouped-data
#'


#' @export
groupedData <- function(x, g) {
  
  if (is.list(x)) {
    
    if (length(x) < 2L)
      stop("'x' must be a list with at least 2 elements")
    
    if (!missing(g))
      warning("'x' is a list, so ignoring argument 'g'")
    
    x <- lapply(x, function(z)
      z[complete.cases(z)])
    
    if (!all(vapply(x, is.numeric, logical(1L))))
      warning(
        "some elements of 'x' are not numeric and will be coerced to numeric"
      )
    
    group.sizes <- lengths(x)
    
    if (any(group.sizes == 0L))
      stop("all groups must contain data")
    
    g <- factor(rep.int(seq_along(x), group.sizes))
    
    if (!is.null(names(x)))
      levels(g) <- names(x)
    
    x <- unlist(x, use.names = FALSE)
    
  } else {
    
    if (missing(g))
      stop("'g' is missing")
    
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    
    ok <- complete.cases(x, g)
    
    x <- x[ok]
    g <- factor(g[ok])
    
    if (nlevels(g) < 2L)
      stop("all observations are in the same group")
    
    group.sizes <- table(g)
  }
  
  list(
    x = x,
    g = g,
    n = length(x),
    k = nlevels(g),
    group.sizes = group.sizes,
    group.names = levels(g)
  )
  
}


