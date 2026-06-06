
# Type coercion aliases
# Shorter, more intuitive alternatives to base R coercion functions

#' Coerce to numeric
#' @param x a vector
#' @param ... further arguments passed to \code{as.numeric}
#' @rdname type-aliases
#' @export
num <- function(x, ...) as.numeric(x, ...)

#' Coerce to integer
#' @param x a vector
#' @param ... further arguments passed to \code{as.integer}
#' @rdname type-aliases
#' @export
int <- function(x, ...) as.integer(x, ...)

#' Coerce to character
#' @param x a vector
#' @param ... further arguments passed to \code{as.character}
#' @rdname type-aliases
#' @export
chr <- function(x, ...) as.character(x, ...)

#' Coerce factor or character to numeric
#'
#' Shortcut for \code{as.numeric(as.character(x))}. Avoids returning
#' internal factor codes when coercing factors directly with \code{as.numeric}.
#'
#' @param x a factor or character vector
#' @rdname type-aliases
#' @export
nchr <- function(x) as.numeric(as.character(x))


#' Coerce to logical
#'
#' Converts any two-valued vector (character, factor, integer, numeric)
#' to logical. Analogous to \code{\link{num}()}, \code{\link{int}()},
#' \code{\link{chr}()}.
#'
#' @param x a vector with exactly two unique non-\code{NA} values.
#' @param ... further arguments passed to \code{\link{asBinary}}.
#'
#' @details
#' The mapping follows the behaviour of \code{\link{factor}()}: levels are
#' ordered alphabetically (or by existing factor level order), and the
#' \emph{first} level maps to \code{FALSE}, the \emph{second} to
#' \code{TRUE}.
#'
#' To reverse the mapping, negate the result: \code{!bin(x)}.
#'
#' @return A logical vector of the same length as \code{x}.
#'
#' @examples
#' bin(c(0L, 1L, 0L, 1L))               # integer 0/1
#' bin(c(0.0, 1.0, 0.0))                # numeric 0/1
#' bin(c("no", "yes", "no"))            # "no" -> FALSE, "yes" -> TRUE
#' !bin(c("no", "yes", "no"))           # reversed: "no" -> TRUE, "yes" -> FALSE
#' bin(factor(c("m", "w", "m")))        # factor: "m" -> FALSE, "w" -> TRUE
#'
#' @seealso \code{\link{num}()}, \code{\link{int}()}, \code{\link{chr}()},
#'   \code{\link{asBinary}()}
#' @rdname type-aliases
#' @export
bin <- function(x, ...) {
  b <- asBinary(x, warn = FALSE, ...)
  result <- as.logical(b)
  attr(result, "coding") <- attr(b, "coding")
  result
}

