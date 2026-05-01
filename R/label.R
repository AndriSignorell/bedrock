
#' Get or set object and variable labels
#'
#' Retrieve or assign a label to an object, or to variables (columns) of a data frame.
#'
#' For atomic objects, a single label can be stored as an attribute \code{"label"}.
#' For data frames, a label can be assigned either to the whole dataset or to
#' individual columns.
#'
#' @param x An object. Typically an atomic vector or a data.frame.
#' @param vars Optional specification of variables (columns) in a data.frame.
#'   Can be:
#'   \itemize{
#'     \item \code{NULL}: operate on the object label (default)
#'     \item \code{TRUE}: all columns
#'     \item numeric indices or character names of columns
#'   }
#' @param value A character vector of labels. For object labels, must be of length 1.
#'   For variable labels, must have length 1 or the same length as \code{vars}.
#'
#' @details
#' The function provides a unified interface for working with labels:
#'
#' \itemize{
#'   \item \code{label(x)} returns the label of an object
#'   \item \code{label(x) <- "text"} sets the label of an object
#'   \item \code{label(x, vars = ...)} returns labels of selected variables
#'   \item \code{label(x, vars = ...) <- value} sets variable labels
#' }
#'
#' Variable labels are stored as attribute \code{"label"} on each column.
#'
#' @return
#' \itemize{
#'   \item Getter: A character scalar (object label) or a named character vector (variable labels).
#'   \item Setter: The modified object \code{x}.
#' }
#'
#' @examples
#' df <- data.frame(age = 1:3, sex = c("m", "f", "m"))
#'
#' # Set dataset label
#' label(df) <- "Example dataset"
#' label(df)
#'
#' # Set variable labels
#' label(df, vars = TRUE) <- c("Age in years", "Sex")
#' label(df, vars = TRUE)
#'
#' # Set single variable label
#' label(df, vars = "age") <- "Age"
#' label(df, vars = "age")
#'
#' # Atomic vector
#' x <- 1:5
#' label(x) <- "Simple vector"
#' label(x)
#'


#' @family label.utils
#' @concept variable-labels
#' @concept data-description
#' @concept data-manipulation
#'
#'
#' @export
label <- function(x, vars = NULL) {
  
  if (is.atomic(x) || is.null(vars)) {
    return(attr(x, "label"))
  }
  
  cols <- if (isTRUE(vars)) seq_along(x) else vars
  
  res <- vapply(
    x[cols],
    function(col) attr(col, "label") %||% NA_character_,
    character(1)
  )
  
  names(res) <- names(x)[cols]
  res
}


#' @rdname label
#' @export
`label<-` <- function(x, vars = NULL, value) {
  
  if (is.list(value))
    stop("cannot assign a list to be an object label")
  
  # atomic → single label
  if (is.atomic(x)) {
    
    if ((length(value) != 1L) && !is.null(value))
      stop("value must be length 1")
    
    attr(x, "label") <- value
    return(x)
  }
  
  # dataset label
  if (is.null(vars)) {
    
    if ((length(value) != 1L) && !is.null(value))
      stop("value must be length 1")
    
    attr(x, "label") <- value
    return(x)
  }
  
  # column labels
  cols <- if (isTRUE(vars)) seq_along(x) else vars
  
  if (length(value) == 1L) {
    value <- rep(value, length.out = length(cols))
  } else if (length(value) != length(cols)) {
    stop("value must have length 1 or same length as vars")
  }
  
  for (i in seq_along(cols)) {
    attr(x[[cols[i]]], "label") <- value[i]
  }
  
  return(x)
}


