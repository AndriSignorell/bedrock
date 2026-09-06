
#' Character <-> ASCII Conversion
#'
#' Convert characters to their numeric character codes and vice versa.
#'
#' `charToAscii()` converts each character in a string to its
#' corresponding numeric code.
#'
#' `asciiToChar()` converts numeric codes back to characters.
#'
#' Only values in the range `1:127` belong to the ASCII standard and
#' therefore have the same meaning across all systems. Values
#' `128:255` depend on the current character encoding (for example
#' ISO-8859-1 or Windows-1252) and may produce different characters on
#' different platforms.
#'
#' Note that `0` (NUL) cannot be represented in R character strings
#' and is therefore not supported.
#'
#' @details
#' The `output` argument controls the representation returned by
#' `charToAscii()`:
#'
#'\describe{
#' \item{`"vector"`}{
#' Simplifies the result whenever possible.
#' 
#' Returns an integer vector if:
#' 
#' \itemize{
#' \item the input consists of a single string, or
#' \item all input strings have length one.
#' }
#' 
#' Otherwise, a list of integer vectors is returned.
#' }
#'
#' \item{`"list"`}{
#'     Always returns a list of integer vectors.
#'   }
#' }
#' 
#' @name char-ascii-conversion
#'
#' @param x a character vector.
#' @param i an integer vector of character codes (1--255).
#' @param output character string specifying the output representation.
#'   One of `"vector"` (simplify the result whenever possible, the
#'   default) or `"list"` (always return a list). See Details.
#'
#' @return
#' \itemize{
#'   \item `charToAscii()` returns either an integer vector or a
#'     list of integer vectors, depending on `output`.
#'
#'   \item `asciiToChar()` returns a character vector.
#' }
#'
#' @examples
#' # basic usage
#' x <- charToAscii("Silvia")
#' x
#'
#' asciiToChar(x)
#'
#' # multiple strings
#' charToAscii(c("A", "BC"), output = "list")
#'
#' # split into individual characters
#' strsplit(asciiToChar(x), split = NULL)
#'
#' # comparison with raw representation
#' charToRaw("Silvia")
#' 
#' @seealso
#' [charToRaw()],
#' [rawToChar()]

#' @family string.transform
#' @concept encoding
NULL


#' @rdname char-ascii-conversion
#' @export
charToAscii <- function(x,
                        output = c("vector", "list")) {

  output <- match.arg(output)

  res <- lapply(
    x,
    function(s) as.integer(charToRaw(s))
  )

  if (output == "list")
    return(res)

  if (length(res) == 1L)
    return(res[[1L]])

  if (all(lengths(res) == 1L))
    return(unlist(res))

  res
}


#' @rdname char-ascii-conversion
#' @export
asciiToChar <- function(i) {

  vapply(
    i,
    function(k) rawToChar(as.raw(k)),
    character(1L)
  )
}
