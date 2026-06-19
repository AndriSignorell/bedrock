
#' Set and Remove Object Attributes
#'
#' Convenience helpers to add, remove, or selectively retain attributes
#' of an object.
#'
#' @name setAttr-removeAttr-keepAttr
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
#'
#' # keep only selected attributes, remove all others
#' r.lm <- lm(Fertility ~ ., swiss)
#' keepAttr(r.lm$terms, "class")
NULL


#' @rdname setAttr-removeAttr-keepAttr
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


#' @rdname setAttr-removeAttr-keepAttr
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


#' @rdname setAttr-removeAttr-keepAttr
#' @export
keepAttr <- function(x, attrNames) {
  # remove all attributes except those listed in attrNames
  remove <- setdiff(names(attributes(x)), attrNames)
  for (a in remove)
    attr(x, which = a) <- NULL
  x
}

