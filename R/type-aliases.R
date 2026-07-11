
#' Type Coercion Shortcuts
#'
#' Concise aliases for common base R coercion functions.
#' \code{num()}, \code{int()}, \code{chr()} are direct wrappers around
#' \code{as.numeric()}, \code{as.integer()}, and \code{as.character()}.
#' \code{nchr()} handles the common pitfall of coercing factors to numeric.
#' \code{bin()} converts any two-valued vector to logical.
#'
#' @details
#' \describe{
#'   \item{\code{num(x, ...)}}{Equivalent to \code{as.numeric(x)}.}
#'   \item{\code{int(x, ...)}}{Equivalent to \code{as.integer(x)}.}
#'   \item{\code{chr(x, ...)}}{Equivalent to \code{as.character(x)}.}
#'   \item{\code{nchr(x)}}{Shortcut for \code{as.numeric(as.character(x))}.
#'     Avoids the trap of \code{as.numeric(factor)} returning internal
#'     integer codes instead of the label values.}
#'   \item{\code{bin(x, ...)}}{Converts a two-valued vector (character,
#'     factor, integer, or numeric) to logical. Mapping follows
#'     \code{\link{factor}()} level order: the \emph{first} level becomes
#'     \code{FALSE}, the \emph{second} \code{TRUE}. To reverse, use
#'     \code{!bin(x)}.}
#' }
#'
#' @param x a vector. For \code{bin()}, exactly two unique non-\code{NA}
#'   values are required.
#' @param ... further arguments passed to the underlying base function
#'   (\code{as.numeric}, \code{as.integer}, \code{as.character}, or
#'   \code{\link{asBinary}})
#'
#' @return A vector of the target type and the same length as \code{x}.
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
#' @aliases num int chr nchr bin
NULL


#' @rdname type-aliases
#' @family data.coerce
#' @concept type-coercion
#' @concept programming
#' @export
num <- function(x, ...) as.numeric(x, ...)

#' @rdname type-aliases
#' @family data.coerce
#' @concept type-coercion
#' @concept programming
#' @export
int <- function(x, ...) as.integer(x, ...)

#' @rdname type-aliases
#' @family data.coerce
#' @concept type-coercion
#' @concept programming
#' @export
chr <- function(x, ...) as.character(x, ...)

#' @rdname type-aliases
#' @family data.coerce
#' @concept type-coercion
#' @concept programming
#' @export
nchr <- function(x) as.numeric(as.character(x))

#' @rdname type-aliases
#' @family data.coerce
#' @concept type-coercion
#' @concept programming
#' @export
bin <- function(x, ...) {
  b <- asBinary(x, warn = FALSE, ...)
  result <- as.logical(b)
  attr(result, "coding") <- attr(b, "coding")
  result
}
