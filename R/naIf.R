
#' Replace Values with NA
#'
#' Replaces specified values in a vector with \code{NA}.
#'
#' @param x A vector.
#' @param values Values to be replaced by \code{NA}.
#'
#' @return A vector of the same type as \code{x}.


#' @export
naIf <- function(x, values) {
  replace(x, x %in% values, NA)
}
