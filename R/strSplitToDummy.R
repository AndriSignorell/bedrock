
#' Split a Character Vector into a Dummy Matrix
#'
#' Splits a character vector of delimited tokens into a binary dummy
#' data.frame where each unique token becomes a column.
#'
#' @param x A character vector with delimited tokens.
#' @param split A character string to use as delimiter. Default is \code{","}.
#' @param trim Logical. If \code{TRUE} (default), whitespace is trimmed from
#'   each token after splitting.
#' @param na.action A function to handle \code{NA} values. Accepted values are
#'   \code{\link[stats]{na.pass}} (default), \code{\link[stats]{na.omit}},
#'   \code{\link[stats]{na.exclude}}, and \code{\link[stats]{na.fail}}.
#'   \describe{
#'     \item{\code{na.pass}}{NAs are kept as all-zero rows (default).}
#'     \item{\code{na.omit}}{Rows with NAs are silently removed.}
#'     \item{\code{na.exclude}}{Like \code{na.omit} but the indices of removed
#'       rows are stored in a \code{"na.action"} attribute.}
#'     \item{\code{na.fail}}{An error is raised if any \code{NA} is present.}
#'   }
#' @param \dots Additional arguments passed to \code{\link[base]{strsplit}}.
#'
#' @return A \code{data.frame} with one row per element of \code{x} and one
#'   column per unique token. Values are \code{0L} or \code{1L}. Column names
#'   are the token values as-is and may not be syntactically valid R
#'   identifiers. The attribute \code{"tokens"} contains the sorted vector of
#'   unique tokens.
#'
#' @seealso \code{\link[base]{strsplit}}, \code{\link[stats]{na.omit}}
#'
#' @examples
#' dat <- data.frame(id = 1:5,
#'                   txt = c("A,C,D", "A", "B,C", "D", "D,E"))
#'
#' # default: NA passed through as zero row
#' strSplitToDummy(dat$txt)
#'
#' # with an NA in the input
#' x_na <- c("A,B", "B,C", NA, "A")
#'
#' # na.pass: NA becomes an all-zero row (default)
#' strSplitToDummy(x_na, na.action = na.pass)
#'
#' # na.omit: NA rows are silently dropped
#' strSplitToDummy(x_na, na.action = na.omit)
#'
#' # na.exclude: like na.omit but NA indices stored in attribute
#' res <- strSplitToDummy(x_na, na.action = na.exclude)
#' attr(res, "na.action")
#'
#' # na.fail: error if any NA present
#' tryCatch(
#'   strSplitToDummy(x_na, na.action = na.fail),
#'   error = function(e) conditionMessage(e)
#' )
#'

#' @export
strSplitToDummy <- function(x, split = ",", trim = TRUE,
                         na.action = na.pass, ...) {
  
  # --- handle NA ---
  na_idx <- which(is.na(x))
  omitted <- NULL
  
  if (length(na_idx) > 0L) {
    if (identical(na.action, na.fail)) {
      stop("missing values in 'x'", call. = FALSE)
    } else if (identical(na.action, na.omit) ||
               identical(na.action, na.exclude)) {
      x <- x[-na_idx]
      omitted <- na_idx
      class(omitted) <- if (identical(na.action, na.exclude)) "exclude" else "omit"
    }
    # na.pass: do nothing, NA remains → treated as "" below
  }
  
  # replace remaining NA (na.pass case) with ""
  x[is.na(x)] <- ""
  
  n <- length(x)
  
  if (n == 0L)
    return(data.frame(matrix(0L, nrow = 0L, ncol = 0L)))
  
  # --- split and trim ---
  lst <- strsplit(as.character(x), split = split, ...)
  
  if (trim)
    lst <- lapply(lst, trimws)
  
  # --- unique tokens ---
  lvl <- sort(unique(unlist(lst)))
  lvl <- lvl[nzchar(lvl)]
  
  if (length(lvl) == 0L)
    return(data.frame(matrix(0L, nrow = n, ncol = 0L)))
  
  if (any(lvl != make.names(lvl)))
    warning("Some token names are not syntactically valid R identifiers.",
            call. = FALSE)
  
  # --- build dummy matrix (vectorised) ---
  idx_list <- lapply(lst, function(z) match(z, lvl))
  
  i <- rep(seq_along(idx_list), lengths(idx_list))
  j <- unlist(idx_list)
  
  # remove NA indices (tokens not in lvl, shouldn't happen but defensive)
  ok <- !is.na(j)
  i <- i[ok]
  j <- j[ok]
  
  res <- matrix(0L, nrow = n, ncol = length(lvl))
  res[cbind(i, j)] <- 1L
  
  res <- as.data.frame(res, check.names = FALSE)
  colnames(res) <- lvl
  attr(res, "tokens") <- lvl
  
  if (!is.null(omitted))
    attr(res, "na.action") <- omitted
  
  return(res)
  
}

