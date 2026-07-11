
#' Last Observation Carried Forward
#'
#' In longitudinal studies it's common that individuals drop out before all
#' responses can be obtained. Measurements obtained before the individual
#' dropped out can be used to impute the unknown measurement(s). The last
#' observation carried forward method is one way to impute values for the
#' missing observations. For the last observation carried forward (LOCF)
#' approach the missing values are replaced by the last observed value of that
#' variable for each individual regardless of when it occurred.
#'
#' \code{locf()} replaces \code{NA}s with the most recent non-NA prior to it.
#'
#' The function will replace all NAs found in a vector with the last earlier
#' value not being NA. In data frames and matrices each column is treated
#' separately, so that values are never carried across column boundaries.
#' Factors are supported and keep their levels and ordering.
#'
#' It should be noted, that the last observation carried forward approach may
#' result in biased estimates and may underestimate the variability.
#'
#' @param x a vector, a data.frame or a matrix containing NAs
#'
#' @return an object of the same type and dimension as \code{x}.
#'
#' @note
#' Based on code by Daniel Wollschlaeger, adapted to conform to package
#' standards; multi-column, data-frame, and factor support added by the
#' package author.
#'
#' @seealso See also the package \pkg{Hmisc} for less coarse imputation
#' functions.
#'
#' @examples
#'
#' d.frm <- data.frame(
#'   day=rep(c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 4)
#' , val=rep(c(runif(5), rep(NA,2)), 4) )
#'
#' d.frm$locf <- locf( d.frm$val )
#' d.frm
#'
#' @family vector.na
#' @concept imputation
#' @concept time-series
#' @export
locf <- function(x) {

  # columns are handled separately: a single flattened pass would carry
  # the last value of one column into the leading NAs of the next
  if (is.data.frame(x)) {
    x[] <- lapply(x, locf)
    return(x)
  }

  if (is.matrix(x)) {
    for (j in seq_len(ncol(x))) {
      x[, j] <- locf(x[, j])
    }
    return(x)
  }

  # factors: carry forward on the integer codes, then restore the
  # values in place so that levels (and ordering) are preserved
  if (is.factor(x)) {
    x[] <- levels(x)[locf(as.integer(x))]
    return(x)
  }

  # corrected by 0.99.19, as this didn't handle c(NA, 3.0, NA, 5,5) correctly
  # rep(x[!is.na(x)], diff(c(which(!is.na(x)), length(x)+1)))

  ok <- !is.na(x)
  rep(c(NA, x[ok]), diff(c(1L, which(ok), length(x) + 1L)))

}
