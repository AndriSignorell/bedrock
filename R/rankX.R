
#' Fast Ranking with Extended Tie Handling
#'
#' Computes ranks for vectors or multiple inputs using a fast implementation
#' based on \code{data.table::frankv}. Supports additional tie-handling
#' methods such as \code{"dense"} and multi-column ranking via \code{...}.
#'
#' @param ... one or more vectors to be ranked. If multiple vectors are
#'   provided, they are ranked lexicographically (like \code{order}).
#'   All inputs must have the same length.
#' @param decreasing logical; if \code{TRUE}, larger values receive smaller
#'   ranks (i.e., ranking in descending order). When ranking multiple
#'   inputs, a logical vector may be given to control the direction per
#'   input.
#' @param na.last logical or \code{"keep"}; determines the placement of
#'   \code{NA} values. Passed to \code{data.table::frankv}.
#' @param ties.method character string specifying how ties are handled.
#'   One of:
#'   \describe{
#'     \item{\code{"average"}}{average of the ranks for tied values (default).}
#'     \item{\code{"first"}}{ranks assigned in order of appearance.}
#'     \item{\code{"last"}}{ranks assigned in reverse order of appearance.}
#'     \item{\code{"random"}}{ranks assigned at random.}
#'     \item{\code{"max"}}{maximum rank for tied values.}
#'     \item{\code{"min"}}{minimum rank for tied values.}
#'     \item{\code{"dense"}}{like \code{"min"}, but ranks are consecutive
#'       integers without gaps.}
#'   }
#'
#' @details
#' This function is a fast alternative to \code{\link[base]{rank}}, powered by
#' \code{data.table::frankv}. It extends base functionality by:
#' \itemize{
#'   \item Supporting dense ranking (\code{ties.method = "dense"})
#'   \item Allowing multiple input vectors for lexicographic ranking
#'   \item Providing improved performance for large datasets
#' }
#'
#' When multiple inputs are supplied, ranking is performed jointly, similar to:
#' \preformatted{
#' order(x1, x2, ...)
#' }
#'
#' @return
#' an integer or numeric vector of ranks with the same length as the input.
#'
#' @seealso
#' \code{\link[base]{rank}}, \code{\link[data.table]{frankv}}
#'
#' @examples
#' x <- c(10, 20, 20, 30)
#'
#' # Basic ranking
#' rankX(x)
#'
#' # Dense ranking
#' rankX(x, ties.method = "dense")
#'
#' # Descending order
#' rankX(x, decreasing = TRUE)
#'
#' # Handling NA values
#' x2 <- c(3, NA, 1, 2)
#' rankX(x2, na.last = "keep")
#'
#' # Multi-column ranking
#' a <- c(1, 1, 2, 2)
#' b <- c(2, 1, 2, 1)
#' rankX(a, b)
#'
#' @family math.transform
#' @concept transformation
#' @concept ties
#' @export
rankX <- function(..., decreasing = FALSE, na.last = TRUE, 
                 ties.method = c("average", "first", "last", 
                                 "random", "max", "min", "dense")) {

  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required for rankX(), please install it",
         call. = FALSE)

  ties.method <- match.arg(ties.method)

  if (...length() == 0L)
    stop("no input vectors supplied")

  if (!is.logical(decreasing) || anyNA(decreasing))
    stop("'decreasing' must be TRUE or FALSE")

  x <- list(...)

  if (length(x) == 1L) {
    x <- x[[1]]
  } else {
    len <- lengths(x)
    if (length(unique(len)) != 1L)
      stop("All inputs must have same length")
  }
  
  ord <- ifelse(decreasing, -1L, 1L)
  
  if (is.list(x)) {
    ord <- rep_len(ord, length(x))
  }
  
  data.table::frankv(x = x, order = ord,
                     na.last = na.last,
                     ties.method = ties.method)
}



