
#' Set and Remove Object Attributes
#'
#' Convenience helpers to add or remove attributes from an object.
#'
#' @name set-and-remove-attributes
#'
#' @param x Object to modify
#' @param attrNames Character vector of attribute names
#' @param attrValues Values for the attributes (only for setting)
#'
#' @return Modified object
#'
#' @seealso \code{\link{setNamesX}}, \code{\link{unname}}
#'
#' @examples
#' x <- runif(10)
#'
#' x <- setAttr(
#'   x,
#'   attrNames = c("some_attr", "other_attr"),
#'   attrValues = c("First attribute", "Second attribute")
#' )
#'
#' # remove single attribute
#' removeAttr(x, "other_attr")
#'
#' # remove all attributes
#' removeAttr(x)
NULL


#' @rdname set-and-remove-attributes
#' @family data.manipulation
#' @concept data-manipulation
#' @concept data-structures

#' @export
setAttr <- function(x, attrNames, attrValues) {
  
  if (!is.character(attrNames)) {
    stop("attrNames must be a character vector.")
  }
  
  if (length(attrNames) != length(attrValues)) {
    stop("attrNames and attrValues must have the same length.")
  }
  
  for (i in seq_along(attrNames)) {
    attr(x, which = attrNames[i]) <- attrValues[i]
  }
  
  x
}


#' @rdname set-and-remove-attributes
#' @export
removeAttr <- function(x, attrNames = NULL) {
  
  if (is.null(attrNames)) {
    attributes(x) <- NULL
  } else {
    for (a in attrNames) {
      attr(x, which = a) <- NULL
    }
  }
  
  x
}

