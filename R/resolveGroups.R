
#' Resolve Grouped Data
#'
#' Brings grouped data into one canonical shape, no matter which of the two
#' usual interfaces the caller was given: a response vector together with a
#' grouping variable, or a list holding one vector per group. The function
#' validates the input, drops missing values, builds the grouping factor and
#' returns the commonly needed group information, providing a shared entry
#' point for hypothesis tests, summaries, effect-size calculations and plotting
#' functions.
#'
#' The two input forms are treated as equivalent. For a list, every element is
#' taken as one group and `groups` is ignored with a warning. For a vector,
#' `groups` is coerced to a factor after the incomplete observations have been
#' removed, so that empty levels are dropped. The levels of an input that
#' already is a factor keep their order, everything else is ordered as
#' [factor()] produces it.
#'
#' The names of a list become the group labels and their order becomes the
#' order of the levels. As these labels end up in printed results, they must be
#' complete and unique; a partially or ambiguously named list is an error
#' rather than being silently renamed. A list without any names is labelled
#' `"1"`, `"2"`, and so on.
#'
#' A data frame is a list of columns and is resolved as one, which covers the
#' common case of one group per column. Its column names are complete and
#' unique by construction and are used as the group labels. Note that the
#' groups of a data frame all have the same length before the missing values
#' are removed, so a ragged design has to be padded with `NA` or passed as a
#' plain list.
#'
#' Missing values are removed in both cases, but along different rules. From a
#' list every `NA` and `NaN` in a group is dropped, from a vector those
#' observations are dropped where either the response or the grouping variable
#' is missing. What remains must leave at least two groups, each of them
#' non-empty.
#'
#' The returned `dataName` is built from the unevaluated arguments and is meant
#' to be passed on to the `data.name` element of an `htest` object. It reads as
#' `"x and g"` for a vector with a grouping variable and as the deparsed
#' expression itself for a list.
#'
#' @param x a numeric vector of observations, or a list of numeric vectors,
#'   one per group. A data frame is a list and is accepted as one, every
#'   column being taken as a group.
#' @param groups a grouping variable, a vector of the same length as `x`,
#'   coerced to a factor. Ignored with a warning when `x` is a list.
#'
#' @return a list containing:
#' \describe{
#'   \item{x}{numeric vector of the observations, missing values removed. For
#'     a list input the groups follow each other in the order of the list.}
#'   \item{groups}{factor of the same length as `x` holding the group
#'     membership.}
#'   \item{n}{integer, the total number of observations.}
#'   \item{k}{integer, the number of groups.}
#'   \item{groupSizes}{named integer vector of the group sample sizes, in the
#'     order of the levels.}
#'   \item{groupNames}{character vector of the group labels, the levels of
#'     `groups`.}
#'   \item{dataName}{character description of the input, for use as the
#'     `data.name` of an `htest` object.}
#' }
#'
#' @examples
#' # vector + grouping variable
#' set.seed(1)
#' x <- rnorm(30)
#' g <- rep(c("a", "b", "c"), each = 10)
#' str(resolveGroups(x, g))
#'
#' # list of group-specific vectors, the names become the labels
#' resolveGroups(list(a = rnorm(10), b = rnorm(12), c = rnorm(8)))[c("k", "groupSizes")]
#'
#' # both interfaces lead to the same result
#' identical(resolveGroups(x, g)$groupSizes,
#'           resolveGroups(split(x, g))$groupSizes)
#'
#' # a data frame is resolved column by column
#' resolveGroups(data.frame(ctrl = c(1, 2, 3), treat = c(4, 5, NA)))$groupSizes
#'
#' @family data.resolve
#' @concept data-resolution
#' @concept categorization
#'
#' @export
resolveGroups <- function(x, groups) {
  
  if (is.list(x)) {
    
    if (length(x) < 2L)
      stop("'x' must contain at least two groups")
    
    if (!missing(groups))
      warning("'x' is a list, so ignoring argument 'groups'")
    
    dname <- deparse1(substitute(x))

    if (!all(vapply(x, is.numeric, logical(1L))))
      stop("all elements of 'x' must be numeric vectors")

    # the names become the group labels, so they must be usable as such
    nm <- names(x)
    
    if (!is.null(nm) && (anyNA(nm) || !all(nzchar(nm)) || anyDuplicated(nm)))
      stop("the names of 'x' must be complete and unique")

    x <- lapply(x, function(z) z[!is.na(z)])
    
    sizes <- lengths(x)
    
    if (any(sizes == 0L))
      stop("all groups must contain observations")
    
    groups <- factor(
      rep.int(seq_along(x), sizes)
    )
    
    if (!is.null(nm))
      levels(groups) <- nm
    
    x <- unlist(x, use.names = FALSE)
    
  } else {
    
    if (missing(groups))
      stop("'groups' is missing")
    
    if (!is.numeric(x) || !is.null(dim(x)))
      stop("'x' must be a numeric vector")
    
    if (!is.atomic(groups) || !is.null(dim(groups)))
      stop("'groups' must be a vector")
    
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

    if (length(x) < 2L)
      stop("not enough observations")

    if (nlevels(groups) < 2L)
      stop("all observations are in the same group")

  }
  
  list(
    x = x,
    groups = groups,
    n = length(x),
    k = nlevels(groups),
    groupSizes = c(table(groups)),
    groupNames = levels(groups),
    dataName = dname
  )
  
}
