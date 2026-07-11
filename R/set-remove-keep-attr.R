
#' Set and Remove Object Attributes
#'
#' Convenience helpers to add, remove, or selectively retain attributes
#' of an object.
#'
#' @name setAttr-removeAttr-keepAttr
#'
#' @param x object to modify
#' @param attrNames character vector of attribute names
#' @param attrValues values for the attributes (only for setting). For a
#'   single attribute name, \code{attrValues} is taken as the value itself
#'   (which may be a vector). For several names, supply one value per name;
#'   use a list for non-scalar or mixed-type values.
#'
#' @return Modified object
#'
#' @seealso [stats::setNames], [base::unname]
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
#' # a single attribute can take a vector value
#' setAttr(1:10, "dim", c(2, 5))
#'
#' # several non-scalar values via list
#' setAttr(1:10, c("dim", "myattr"), list(c(2, 5), "abc"))
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

#' @family label.attrs
#' @concept attribute
#' @concept programming
#' @export
setAttr <- function(x, attrNames, attrValues) {

  if (!is.character(attrNames)) {
    stop("attrNames must be a character vector.")
  }

  if (!is.list(attrValues)) {
    # a single attribute may take a whole vector as its value,
    # e.g. setAttr(x, "dim", c(2, 5))
    attrValues <- if (length(attrNames) == 1L)
      list(attrValues)
    else
      as.list(attrValues)
  }

  if (length(attrNames) != length(attrValues)) {
    stop("attrNames and attrValues must have the same length.")
  }

  for (i in seq_along(attrNames)) {
    attr(x, which = attrNames[i]) <- attrValues[[i]]
  }

  x
}


#' @rdname setAttr-removeAttr-keepAttr
#' @family label.attrs
#' @concept attribute
#' @concept programming
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
#' @family label.attrs
#' @concept attribute
#' @concept programming
#' @export
keepAttr <- function(x, attrNames) {
  # remove all attributes except those listed in attrNames
  remove <- setdiff(names(attributes(x)), attrNames)
  for (a in remove)
    attr(x, which = a) <- NULL
  x
}

