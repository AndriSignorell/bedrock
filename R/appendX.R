
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
#' @param rows Logical; if TRUE, insert rows instead of columns. Ignored for vectors.
#' @param newNames Optional names for inserted elements.
#' @param ... Additional arguments.
#'
#' @return Object of same type as \code{x}.
#'


#' @family data.manipulation
#' @concept merge
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
                           rows = FALSE, newNames = NULL, ...) {

  if (rows) {

    n <- nrow(x)
    after <- .validateAfter(after, n)

    if (.badRecycle(length(values), ncol(x)))
      warning("'values' length is not a multiple or sub-multiple of ncol(x)")

    # byrow: values werden zeilenweise gelesen (intuitiv beim Zeilen-Einfuegen);
    # base-Warning unterdrueckt, da oben bereits kontrolliert gewarnt wird
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

    if (!is.null(newNames))
      names(values) <- newNames

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

    if (!is.null(newNames))
      values <- setNamesX(list(values), names = newNames)
    else
      values <- list(values)

    res <- append(x, values, after = after)
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


# Krummes Recycling: Laenge ist weder Teiler noch Vielfaches der Zieldimension.
# Skalare (len == 1) und saubere Teiler/Vielfache bleiben still.
.badRecycle <- function(len, n) {
  len > 0 && n > 0 && n %% len != 0 && len %% n != 0
}
