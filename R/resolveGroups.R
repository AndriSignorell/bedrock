
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
#' @param groups a grouping variable of the same length as \code{x}.
#'   Ignored when \code{x} is a list.
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{numeric response vector}
#'   \item{groups}{grouping factor}
#'   \item{n}{total number of observations}
#'   \item{k}{number of groups}
#'   \item{group.sizes}{group sample sizes}
#'   \item{group.names}{group labels}
#'   \item{data.name}{character description of the input}
#' }
#'



#' @family data.utils  
#' @concept formula  
#' @concept modelling
#'
#'
#' @export
resolveGroups <- function(x, groups) {
  
  if (is.list(x)) {
    
    if (length(x) < 2L)
      stop("'x' must contain at least two groups")
    
    if (!missing(groups))
      warning("'x' is a list, so ignoring argument 'groups'")
    
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
    
    groups <- factor(
      rep.int(seq_along(x), group.sizes)
    )
    
    if (!is.null(names(x)))
      levels(groups) <- names(x)
    
    x <- unlist(x, use.names = FALSE)
    
  } else {
    
    if (missing(groups))
      stop("'groups' is missing")
    
    if (length(x) != length(groups))
      stop("'x' and 'groups' must have the same length")
    
    dname <- paste(
      deparse1(substitute(x)),
      "and",
      deparse1(substitute(groups))
    )
    
    ok <- complete.cases(x, groups)
    
    x <- x[ok]
    groups <- factor(groups[ok])
    
    n <- length(x)
    
    if (n < 2L)
      stop("not enough observations")
    
    if (nlevels(groups) < 2L)
      stop("all observations are in the same group")
    
    group.sizes <- table(groups)
  }
  
  n <- length(x)
  
  if (n < 2L)
    stop("not enough observations")
  
  list(
    x = x,
    groups = groups,
    n = n,
    k = nlevels(groups),
    group.sizes = group.sizes,
    group.names = levels(groups),
    data.name = dname
  )
  
}

