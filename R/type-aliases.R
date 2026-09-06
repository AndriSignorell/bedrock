
#' Type Coercion Shortcuts
#'
#' Concise aliases for common base R coercion functions.
#' `num()`, `int()`, `chr()` are direct wrappers around
#' `as.numeric()`, `as.integer()`, and `as.character()`.
#' `nchr()` handles the common pitfall of coercing factors to numeric.
#' `bin()` converts any two-valued vector to logical.
#'
#' @details
#' \describe{
#'   \item{`num(x, ...)`}{equivalent to `as.numeric(x)`.}
#'   \item{`int(x, ...)`}{equivalent to `as.integer(x)`.}
#'   \item{`chr(x, ...)`}{equivalent to `as.character(x)`.}
#'   \item{`nchr(x)`}{shortcut for `as.numeric(as.character(x))`.
#'     Avoids the trap of `as.numeric(factor)` returning internal
#'     integer codes instead of the label values.}
#'   \item{`bin(x, ...)`}{converts a two-valued vector (character,
#'     factor, integer, or numeric) to logical. Mapping follows
#'     [factor()] level order: the *first* level becomes
#'     `FALSE`, the *second* `TRUE`. To reverse, use
#'     `!bin(x)`.}
#' }
#'
#' @param x a vector. For `bin()`, exactly two unique non-`NA`
#'   values are required.
#' @param ... further arguments passed to the underlying base function
#'   (`as.numeric`, `as.integer`, `as.character`, or
#'   [asBinary()]).
#'
#' @return a vector of the target type and the same length as `x`.
#'
#' @examples
#' num("3.14")
#' int(3.9)                               # truncates, does not round
#' chr(1:3)
#' nchr(factor(c("1.5", "2.0", "1.5"))) # correct: 1.5 2.0 1.5
#' as.numeric(factor(c("1.5", "2.0")))  # wrong:   1   2
#'
#' bin(c(0L, 1L, 0L, 1L))
#' bin(c("no", "yes", "no"))            # "no" -> FALSE, "yes" -> TRUE
#' !bin(c("no", "yes", "no"))           # reversed
#' bin(factor(c("m", "w", "m")))        # "m" -> FALSE, "w" -> TRUE
#'
#' @seealso [nf()], [asBinary()] 
#' @name type-aliases
#' @family data.coerce
#' @concept type-coercion
#' @aliases num int chr nchr bin
NULL


#' @rdname type-aliases
#' @export
num <- function(x, ...) as.numeric(x, ...)

#' @rdname type-aliases
#' @export
int <- function(x, ...) as.integer(x, ...)

#' @rdname type-aliases
#' @export
chr <- function(x, ...) as.character(x, ...)

#' @rdname type-aliases
#' @export
nchr <- function(x) as.numeric(as.character(x))

#' @rdname type-aliases
#' @export
bin <- function(x, ...) {
  b <- asBinary(x, warn = FALSE, ...)
  result <- as.logical(b)
  attr(result, "coding") <- attr(b, "coding")
  result
}

