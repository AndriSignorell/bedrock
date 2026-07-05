
#' Count Complete Cases
#'
#' Return for each variable of a data frame the number of missing values and
#' the complete cases to be expected if this variable would be omitted.
#'
#' @param x a data.frame containing the data.
#' @return A list with three elements. The first gives the number of rows, the
#' second the number of complete cases for the whole data frame. The third
#' element \code{tab} contains the data for the single variables.
#'
#' @seealso \code{\link[aurora]{plotMiss}},
#' \code{\link{complete.cases}}, \code{\link{is.na}}, \code{\link{na.omit}}
#'
#' @examples
#' countCompCases(airquality)
#'
#' @family data.inspection
#' @concept missing-value
#' @export
countCompCases <- function(x) {

  # is.na() on a data.frame always yields a matrix
  # (sapply would return a vector for a single-row data.frame)
  x <- is.na(x)
  rs <- rowSums(x)

  n <- nrow(x)
  cc <- sum(rs == 0)

  # NAs, columnwise left out
  z <- sapply(seq_len(ncol(x)),
              function(i) sum((rs - x[, i]) == 0))

  m <- colSums(x)

  res <- list(
    n = n, cc = cc,
    tab = setNamesX(
      data.frame(vname=colnames(x),
                 nas=m, nas_p=m/n,
                 cifnot=z, cifnot_p=z/n),
      rownames=NULL)
  )

  class(res) <- "CountCompCases"
  res

}
