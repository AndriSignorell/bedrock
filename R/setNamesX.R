
#' Set the Names in an Object 
#' 
#' This is a convenience function that sets the names of an object and returns
#' it including the new names. It is most useful at the end of a function
#' definition where one is creating the object to be returned and would prefer
#' not to store it under a name just that the names can be assigned. In
#' addition to the function [setNames()] in base R the user can
#' decide, whether rownames, colnames or simply the names are to be set. 
#' 
#' A name of length one is recycled to the required extent, which is handy for
#' blanking out names with `""`. Names of any other length must match the
#' extent of the object exactly; a deviating length is reported as an error
#' rather than being recycled silently, as an unexpected length is almost
#' always a miscalculation upstream and duplicated names are hard to debug
#' later on.
#' 
#' @param x an object for which a names attribute will be meaningful. 
#' @param ... the names to be assigned to the object. This should be a
#' character vector of names named `dimnames`, `rownames`,
#' `colnames` or `names`. Setting `rownames=NULL` would remove
#' existing rownames. All kind of names can be changed at the same time.
#' Default would be `names`. Abbreviations are supported. 
#' @return an object of the same sort as object with the new names assigned.
#'  
#' @examples
#' 
#' setNamesX(1:5, names=letters[1:5])
#' 
#' # the default, if no argument names are provided, is "names"
#' setNamesX(1:5, letters[1:5])
#' 
#' # rownames and columnnames can be set at the same time
#' setNamesX(matrix(c(1:12), nrow=4), 
#'          rownames=LETTERS[11:14], colnames=c("perc", "lci", "uci"))
#'          
#' # a single name is recycled, so this sets all the names to an empty string
#' setNamesX(diag(6), rownames="", colnames="")
#' 
#' # any other length must fit, a mismatch is an error
#' try(setNamesX(matrix(c(1:12), nrow=4), colnames=c("perc", "lci")))
#' 
#' # setting dimnames works as well
#' tab <- setNamesX(
#'   as.table(rbind(c(84,43), c(10,92))), 
#'     dimnames= list(
#'        dipstick=c("positive","negative"),
#'        culture=c("positive","negative")))
#' 
#' 
#' @seealso [setNames()]
#' 
#' @family label.attrs
#' @concept attribute
#' @concept label
#' @export
setNamesX <- function (x, ...) {
  
  # see also setNames()
  args <- list(...)

  # unnamed arguments default to "names" -- also when mixed with
  # named ones, e.g. setNamesX(m, letters, colnames = cn)
  nm <- names(args)
  if (is.null(nm))
    nm <- rep("", length(args))
  nm[!nzchar(nm)] <- "names"

  names(args) <- vapply(nm, match.arg, character(1L),
                        choices = c("names", "rownames",
                                    "colnames", "dimnames"))
  
  if ("dimnames" %in% names(args))
    dimnames(x) <- args[["dimnames"]]
  
  if ("rownames" %in% names(args))
    rownames(x) <- if (is.null(args[["rownames"]])) NULL
                   else .fitNames(args[["rownames"]], dim(x)[1], "rownames")
  
  if ("colnames" %in% names(args))
    colnames(x) <- if (is.null(args[["colnames"]])) NULL
                   else .fitNames(args[["colnames"]], dim(x)[2], "colnames")
  
  if ("names" %in% names(args))
    names(x) <- if (is.null(args[["names"]])) NULL
                else .fitNames(args[["names"]], length(x), "names")
  
  x
  
}


# recycle a name of length one, insist on an exact match otherwise
# n is NA when the object has no dimensions at all
#' @noRd
.fitNames <- function(value, n, what) {
  
  # dim(x)[1] is NULL for an object without dimensions and NA when the
  # dimension asked for does not exist, e.g. colnames of a 1d array
  if (length(n) != 1L)
    stop(gettextf("attempt to set '%s' on an object with no dimensions", what),
         call. = FALSE)
  
  if (is.na(n))
    stop(gettextf("attempt to set '%s' on an object with less than two dimensions",
                  what), call. = FALSE)
  
  if (length(value) == 1L)
    return(rep_len(value, n))
  
  if (length(value) != n)
    stop(gettextf("length of '%s' [%d] not equal to extent [%d]",
                  what, length(value), n), call. = FALSE)
  
  value
  
}
