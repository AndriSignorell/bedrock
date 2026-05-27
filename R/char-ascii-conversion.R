
#' Character <-> ASCII Conversion
#'
#' Convert characters to ASCII codes and vice versa.
#'
#' \code{charToAscii()} converts each character in a string to its
#' ASCII code.
#'
#' \code{asciiToChar()} converts ASCII codes back to characters.
#'
#' Only codes in \code{1:127} are standard ASCII and consistent across
#' systems.
#'
#' Codes above 127 depend on the current locale and encoding
#' (e.g. ISO-8859-1).
#'
#' Note that \code{0} (NUL) is not supported in R character strings.
#'
#' @details
#' The \code{output} argument controls the representation returned by
#' \code{charToAscii()}:
#'
#' \describe{
#'   \item{\code{\"vector\"}}{
#'     Simplify the result when possible:
#'
#'     \itemize{
#'       \item single string -> integer vector
#'       \item all strings length 1 -> integer vector
#'       \item otherwise -> list
#'     }
#'   }
#'
#'   \item{\code{\"list\"}}{
#'     Always return a list of integer vectors.
#'   }
#' }
#'
#' @name char-ascii-conversion
#'
#' @param x Character vector.
#' @param i Integer vector of ASCII codes (1–255).
#' @param output Character string specifying the output representation.
#'   One of:
#'
#'   \describe{
#'     \item{\code{\"vector\"}}{
#'       Return a simplified integer vector when possible.
#'     }
#'     \item{\code{\"list\"}}{
#'       Always return a list of integer vectors.
#'     }
#'   }
#'
#'   Default is \code{\"vector\"}.
#'
#' @return
#' \itemize{
#'   \item \code{charToAscii()} returns either an integer vector or
#'   a list of integer vectors depending on \code{output}.
#'
#'   \item \code{asciiToChar()} returns a character vector.
#' }
#'
#' @seealso
#' \code{\link{charToRaw}},
#' \code{\link{rawToChar}}
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
NULL


#' @rdname char-ascii-conversion
#' @family string.utilities
#' @concept string-manipulation
#' @concept data-manipulation


#' @export
charToAscii <- function(x,
                        output = c("vector", "list")) {
  
  output <- match.arg(output)
  
  res <- lapply(
    x,
    function(s) strtoi(charToRaw(s), 16L)
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
    function(i) rawToChar(as.raw(i)),
    character(1)
  )
}

