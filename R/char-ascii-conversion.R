
#' Character <-> ASCII Conversion
#'
#' Convert characters to ASCII codes and vice versa.
#'
#' \code{charToAscii()} converts each character in a string to its ASCII code.
#' \code{asciiToChar()} converts ASCII codes back to characters.
#'
#' Only codes in \code{1:127} are standard ASCII and consistent across systems.
#' Codes above 127 depend on the current locale and encoding (e.g. ISO-8859-1).
#'
#' Note that \code{0} (NUL) is not supported in R character strings.
#'
#' @name char-ascii-conversion
#'
#' @param x Character vector.
#' @param i Integer vector of ASCII codes (1–255).
#' @param simplify Logical. If \code{TRUE}, simplify the result of
#'   \code{charToAscii()} when possible:
#'   \itemize{
#'     \item single string -> integer vector
#'     \item all strings length 1 -> integer vector
#'     \item otherwise -> list
#'   }
#'
#' @return
#' \itemize{
#'   \item \code{charToAscii()} returns either an integer vector or a list of
#'   integer vectors depending on \code{simplify}.
#'   \item \code{asciiToChar()} returns a character vector.
#' }
#'
#' @seealso \code{\link{charToRaw}}, \code{\link{rawToChar}}
#'
#' @examples
#' # basic usage
#' x <- charToAscii("Silvia")
#' x
#'
#' asciiToChar(x)
#'
#' # multiple strings
#' charToAscii(c("A", "BC"), simplify = FALSE)
#'
#' # split into individual characters
#' strsplit(asciiToChar(x), split = NULL)
#'
#' # comparison with raw representation
#' charToRaw("Silvia")
NULL


#' @rdname char-ascii-conversion
#' @family string.utilities
#' @concept string-manipulation
#' @concept data-manipulation
#' @export
charToAscii <- function(x, simplify = TRUE) {
  res <- lapply(x, function(s) strtoi(charToRaw(s), 16L))
  
  if (simplify && length(res) == 1) return(res[[1]])
  if (simplify && all(lengths(res) == 1)) return(unlist(res))
  
  res
}


#' @rdname char-ascii-conversion
#' @export
asciiToChar <- function(i) {
  vapply(i, function(i) rawToChar(as.raw(i)), character(1))
}

