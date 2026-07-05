
#' Append Elements to Objects
#'
#' Generic function to append elements to vectors, matrices, and data frames.
#'
#' @name appendX
#'
#' @param x Object to which values are appended.
#' @param values Values to insert into \code{x}. For matrices with
#'   \code{rows = TRUE}, values are read row by row.
#' @param after Position after which to insert. If \code{NULL}, values are appended
#'   at the end. Use \code{0} to prepend.
#' @param rows Logical; if TRUE, insert rows instead of columns. Ignored for
#'   vectors. Note that the method for \code{TOne} objects defaults to
#'   \code{rows = TRUE}, as appending rows is the typical use case there.
#' @param newNames Optional names for the inserted elements: column names when
#'   inserting columns, row names when inserting rows. When inserting a column
#'   into a data.frame without giving \code{newNames}, default names
#'   (\code{"V1"}, \code{"V2"}, ...) are used.
#' @param ... Additional arguments.
#'
#' @return Object of the same class as \code{x}.
#'
#' @seealso \code{\link{append}}
#'
#' @examples
#' # vectors
#' appendX(1:5, 99, after = 2)
#'
#' # matrices: insert a column / a row
#' m <- matrix(1:6, nrow = 2,
#'             dimnames = list(c("r1", "r2"), c("a", "b", "c")))
#' appendX(m, c(9, 9), after = 1, newNames = "z")
#' appendX(m, 7:9, after = 1, rows = TRUE, newNames = "r1b")
#'
#' # data frames: insert a column / a row
#' d <- data.frame(a = 1:3, b = 4:6)
#' appendX(d, 7:9, after = 1, newNames = "z")
#' appendX(d, list(a = 99, b = 88), after = 0, rows = TRUE)
#'
#' @family data.manipulation
#' @concept merge
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
                           rows = FALSE, newNames = NULL, ...) {

  if (rows) {

    n <- nrow(x)
    after <- .validateAfter(after, n)

    if (.badRecycle(length(values), ncol(x)))
      warning("'values' length is not a multiple or sub-multiple of ncol(x)")

    # byrow: values are read row by row (intuitive when inserting rows);
    # the base warning is suppressed, as a controlled warning is issued above
    values <- suppressWarnings(matrix(values, ncol = ncol(x), byrow = TRUE))

    if (!is.null(newNames))
      rownames(values) <- newNames

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

    if (.badRecycle(length(values), nrow(x)))
      warning("'values' length is not a multiple or sub-multiple of nrow(x)")

    values <- suppressWarnings(matrix(values, nrow = nrow(x)))

    if (!is.null(newNames))
      colnames(values) <- newNames

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
                               rows = FALSE, newNames = NULL, ...) {

  if (rows) {

    n <- nrow(x)
    after <- .validateAfter(after, n)

    if (!is.list(values))
      values <- as.list(values)

    if (length(values) != ncol(x))
      stop("Inserted row must have same number of columns as 'x'")

    # match by name if names are given, otherwise assign positionally
    if (!is.null(names(values))) {
      if (!setequal(names(values), colnames(x)))
        stop("Names of 'values' must match the column names of 'x'")
      values <- values[colnames(x)]
    } else {
      names(values) <- colnames(x)
    }

    values <- as.data.frame(values, stringsAsFactors = FALSE)

    if (!is.null(newNames))
      rownames(values) <- newNames

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

    if (!is.list(values))
      values <- list(values)

    if (is.null(newNames) && is.null(names(values)))
      names(values) <- paste0("V", seq_along(values))
    else if (!is.null(newNames))
      names(values) <- newNames

    values <- as.data.frame(values, stringsAsFactors = FALSE)

    # cbind (instead of append) preserves the data.frame class and row names
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
  }

  res
}



#' @rdname appendX
#' @export
appendX.TOne <- function(x, values, after = NULL,
                         rows = TRUE, newNames = NULL, ...) {

  res <- appendX.matrix(x, values, after = after,
                        rows = rows, newNames = newNames, ...)

  attr(res, "legend") <- attr(x, "legend")
  class(res) <- class(x)

  res
}



# == internal helper functions ==============================================

.validateAfter <- function(after, default) {
  if (is.null(after)) return(default)

  if (!is.numeric(after) || length(after) != 1)
    stop("'after' must be a single numeric value")

  if (is.na(after) || after < 0)
    stop("'after' must be >= 0")

  if (after %% 1 != 0)
    stop("'after' must be a whole number")

  after
}


# Crooked recycling: length is neither a divisor nor a multiple of the
# target dimension. Scalars (len == 1) and clean divisors/multiples
# stay silent.
.badRecycle <- function(len, n) {
  len > 0 && n > 0 && n %% len != 0 && len %% n != 0
}
