
#' Append Elements to Objects
#'
#' Generic function to append elements to vectors, matrices, and data frames.
#'
#' @name appendX
#'
#' @param x Object to which values are appended.
#' @param values Values to insert into \code{x}.
#' @param after Position after which to insert. If \code{NULL}, values are appended
#'   at the end. Use \code{0} to prepend.
#' @param rows Logical; if TRUE, insert rows instead of columns. Ignored for vectors.
#' @param new_names Optional names for inserted elements.
#' @param ... Additional arguments.
#'
#' @return Object of same type as \code{x}.
#'
#' @family data.manipulation
#' @concept data-manipulation
#'
#'

#' @export
appendX <- function(x, values, after = NULL, ...) {
  UseMethod("appendX")
}



#' @rdname appendX
#' @export
appendX.default <- function(x, values, after = NULL, ...) {
  
  after <- .validateAfter(after, length(x))
  
  append(x, values, after = after)
}


#' @rdname appendX
#' @export
appendX.matrix <- function(x, values, after = NULL,
                           rows = FALSE, new_names = NULL, ...) {
  
  if (rows) {
    
    n <- nrow(x)
    after <- .validateAfter(after, n)
    
    if (length(values) %% ncol(x) != 0)
      warning("'values' length is not a multiple of ncol(x)")
    
    values <- matrix(values, ncol = ncol(x))
    
    if (!is.null(new_names))
      rownames(values) <- new_names
    
    if (after == 0) {
      res <- rbind(values, x)
    } else if (after >= n) {
      res <- rbind(x, values)
    } else {
      res <- rbind(
        x[1:after, , drop = FALSE],
        values,
        x[(after + 1):n, , drop = FALSE]
      )
    }
    
    colnames(res) <- colnames(x)
    
  } else {
    
    n <- ncol(x)
    after <- .validateAfter(after, n)
    
    if (length(values) %% nrow(x) != 0)
      warning("'values' length is not a multiple of nrow(x)")
    
    values <- matrix(values, nrow = nrow(x))
    
    if (!is.null(new_names))
      colnames(values) <- new_names
    
    if (after == 0) {
      res <- cbind(values, x)
    } else if (after >= n) {
      res <- cbind(x, values)
    } else {
      res <- cbind(
        x[, 1:after, drop = FALSE],
        values,
        x[, (after + 1):n, drop = FALSE]
      )
    }
    
    rownames(res) <- rownames(x)
  }
  
  res
}



#' @rdname appendX
#' @export
appendX.data.frame <- function(x, values, after = NULL,
                               rows = FALSE, new_names = NULL, ...) {
  
  if (rows) {
    
    n <- nrow(x)
    after <- .validateAfter(after, n)
    
    if (!is.list(values))
      values <- as.list(values)
    
    if (!is.null(new_names))
      names(values) <- new_names
    
    values <- as.data.frame(values, stringsAsFactors = FALSE)
    
    if (ncol(values) != ncol(x))
      stop("Inserted row must have same number of columns as 'x'")
    
    if (after == 0) {
      res <- rbind(values, x)
    } else if (after >= n) {
      res <- rbind(x, values)
    } else {
      res <- rbind(
        x[1:after, , drop = FALSE],
        values,
        x[(after + 1):n, , drop = FALSE]
      )
    }
    
  } else {
    
    n <- ncol(x)
    after <- .validateAfter(after, n)
    
    if (!is.null(new_names))
      values <- setNamesX(list(values), names = new_names)
    else
      values <- list(values)
    
    res <- append(x, values, after = after)
  }
  
  res
}



#' @rdname appendX
#' @export
appendX.TOne <- function(x, values, after = NULL,
                         rows = TRUE, new_names = NULL, ...) {

  res <- appendX.matrix(x, values, after = after,
                        rows = rows, new_names = new_names, ...)

  attr(res, "legend") <- attr(x, "legend")
  class(res) <- "TOne"
  
  res
}



# == internal helper functions ==============================================

.validateAfter <- function(after, default) {
  if (is.null(after)) return(default)
  
  if (!is.numeric(after) || length(after) != 1)
    stop("'after' must be a single numeric value")
  
  if (is.na(after) || after < 0)
    stop("'after' must be >= 0")  
  
  after
}



