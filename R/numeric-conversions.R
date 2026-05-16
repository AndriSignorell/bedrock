
#' Converts Numbers Between Bases
#'
#' These functions convert numbers from one base to another.  There are
#' several solutions for this problem out there, but the naming is quite
#' heterogeneous and so consistent function names might be helpful.
#'
#' \code{binToDec()} converts numbers from binary mode into decimal
#' values; \code{decToBin()} does the reverse.  Oct means octal system
#' and hex hexadecimal.  \code{baseToBase()} is the general case,
#' supporting any base from 2 to 36.
#'
#' @details
#' All specialist functions are special cases of \code{baseToBase()}:
#' \tabular{ll}{
#'   \code{hexToDec(x)} \tab \code{baseToBase(x, 16, 10)} \cr
#'   \code{decToHex(x)} \tab \code{baseToBase(x, 10, 16)} \cr
#'   \code{octToDec(x)} \tab \code{baseToBase(x,  8, 10)} \cr
#'   \code{decToOct(x)} \tab \code{baseToBase(x, 10,  8)} \cr
#'   \code{binToDec(x)} \tab \code{baseToBase(x,  2, 10)} \cr
#'   \code{decToBin(x)} \tab \code{baseToBase(x, 10,  2)} \cr
#' }
#'
#' \code{baseToBase()} uses \code{\link{strtoi}()} for parsing, which
#' operates on \code{long int} internally.  Values above approximately
#' \eqn{2^{31} - 1} may return \code{NA} on 32-bit platforms.
#'
#' @name numeric-conversions
#' @aliases hexToDec decToHex octToDec decToOct binToDec decToBin
#'   romanToInt baseToBase
#'
#' @param x A vector of numbers or character representations to be
#'   converted.
#' @param from A single integer in \eqn{[2, 36]} giving the input base
#'   (\code{baseToBase} only).
#' @param to   A single integer in \eqn{[2, 36]} giving the output base
#'   (\code{baseToBase} only).
#' @param width A single non-negative integer or \code{NULL} (default).
#'   If supplied, the output is left-padded with zeros to at least
#'   \code{width} characters (\code{baseToBase} only).
#'
#' @return A numeric or character vector of the same length as \code{x}
#'   containing the converted values.  Binary, octal and decimal values
#'   are numeric; hex values are returned as class \code{hexmode}.
#'   \code{baseToBase()} always returns a character vector (uppercase).
#'   \code{NA} input produces \code{NA} output.
#'
#' @seealso \code{\link{strtoi}}, \code{\link{as.hexmode}},
#'   \code{\link{as.octmode}}, \code{\link{as.roman}}
#'
#' @examples
#' decToBin(c(17, 25))
#' binToDec(c(101, 11101))
#'
#' decToOct(c(17, 25))
#' octToDec(c(11, 77))
#'
#' decToHex(c(17, 25))
#' hexToDec(c("FF", "AA", "ABC"))
#'
#' # general base conversion
#' baseToBase("FF",       from = 16, to = 10)   # hex -> dec
#' baseToBase("255",      from = 10, to = 16)   # dec -> hex
#' baseToBase("11111111", from = 2,  to = 10)   # bin -> dec
#' baseToBase("1A3F",     from = 16, to = 2)    # hex -> bin
#' baseToBase("Z9",       from = 36, to = 10)   # base-36 -> dec
#'
#' # fixed-width output
#' baseToBase(c("0", "7", "255"), from = 10, to = 2, width = 8)
#'
#' # vectorized
#' baseToBase(c("A", "B", "FF"), from = 16, to = 10)
#'
#' @rdname numeric-conversions
#' @family number.theory
#' @concept number-theory
#' @concept data-manipulation
#' @concept string-manipulation
#'
#' @export
hexToDec <- function(x)
  strtoi(gsub("^#", "", x), 16L)

#' @rdname numeric-conversions
#' @export
decToHex <- function(x) as.hexmode(as.numeric(x))

#' @rdname numeric-conversions
#' @export
octToDec <- function(x) strtoi(x, 8L)

#' @rdname numeric-conversions
#' @export
decToOct <- function(x) as.numeric(as.character(as.octmode(as.numeric(x))))

#' @rdname numeric-conversions
#' @export
binToDec <- function(x) strtoi(x, 2L)

#' @rdname numeric-conversions
#' @export
decToBin <- function(x) {
  z <- dec_to_bin_cpp(x)
  z[x > 536870911] <- NA
  z <- sub("^0+", "", z)
  z[z == ""] <- "0"   # sub strips the only zero for input 0
  z
}

#' @rdname numeric-conversions
#' @export
romanToInt <- function(x) {
  roman <- trimws(toupper(as.character(x)))
  tryIt <- function(x) {
    retval <- try(roman_to_int_cpp(x), silent = TRUE)
    if (is.numeric(retval)) retval else NA
  }
  sapply(roman, tryIt)
}

#' @rdname numeric-conversions
#' @export
baseToBase <- function(x, from, to, width = NULL) {
  
  # --- validate from / to ----------------------------------------------
  .check_base <- function(b, name) {
    if (!is.numeric(b) || length(b) != 1L || is.na(b) ||
        b < 2L || b > 36L || b %% 1 != 0)
      stop(gettextf(
        "Argument '%s' must be a single integer in [2, 36].", name
      ))
    as.integer(b)
  }
  from <- .check_base(from, "from")
  to   <- .check_base(to,   "to")
  
  # --- validate width --------------------------------------------------
  if (!is.null(width)) {
    if (!is.numeric(width) || length(width) != 1L || is.na(width) ||
        width < 0L || width %% 1 != 0)
      stop("Argument 'width' must be a single non-negative integer or NULL.")
    width <- as.integer(width)
  }
  
  # --- coerce x to character -------------------------------------------
  if (is.numeric(x)) {
    if (from != 10L)
      stop("Numeric 'x' is only accepted when 'from = 10'.")
    x <- as.character(as.integer(x))
  } else {
    x <- as.character(x)
  }
  
  # digits 0-9, A-Z
  digits <- c(as.character(0:9), LETTERS)
  
  # --- single-value worker ---------------------------------------------
  .convert_one <- function(s) {
    
    if (is.na(s)) return(NA_character_)
    
    s <- toupper(trimws(s))
    if (!nzchar(s))
      stop("Empty string is not a valid number representation.")
    
    # parse: string in base `from` -> integer via strtoi
    dec <- strtoi(s, base = from)
    if (is.na(dec))
      stop(gettextf(
        "'%s' is not a valid representation in base %d.", s, from
      ))
    
    # format: integer -> string in base `to`
    if (dec == 0L) {
      res <- if (!is.null(width)) strrep("0", max(width, 1L)) else "0"
      return(res)
    }
    
    out <- character(0)
    n   <- dec
    while (n > 0L) {
      out <- c(digits[n %% to + 1L], out)
      n   <- n %/% to
    }
    res <- paste(out, collapse = "")
    
    if (!is.null(width) && nchar(res) < width)
      res <- paste0(strrep("0", width - nchar(res)), res)
    
    res
  }
  
  vapply(x, .convert_one, character(1L), USE.NAMES = FALSE)
}


