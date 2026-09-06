
#' Convert Numbers Between Bases
#'
#' Vectorized conversion between positional numeral systems (bases 2-36),
#' plus Roman-numeral parsing.  The convenience wrappers cover the most
#' common cases; `baseToBase()` handles any combination of bases.
#'
#' @section Convenience wrappers:
#' All specialist functions are thin wrappers around `baseToBase()`:
#' \tabular{lll}{
#'   **Function**    \tab **Equivalent call**              \tab **Returns**         \cr
#'   `binToDec(x)` \tab `baseToBase(x,  2, 10)`       \tab integer                \cr
#'   `decToBin(x)` \tab `baseToBase(x, 10,  2)`       \tab character              \cr
#'   `octToDec(x)` \tab `baseToBase(x,  8, 10)`       \tab integer                \cr
#'   `decToOct(x)` \tab `baseToBase(x, 10,  8)`       \tab numeric (octal digits) \cr
#'   `hexToDec(x)` \tab `baseToBase(x, 16, 10)`       \tab integer                \cr
#'   `decToHex(x)` \tab `baseToBase(x, 10, 16)`       \tab `hexmode`         \cr
#' }
#' `hexToDec()` additionally strips a leading `#` from CSS-style
#' colour strings.
#'
#' @section Roman numerals:
#' `romanToInt()` converts Roman numeral strings (e.g. `"XIV"`) to
#' integers.  Input is trimmed and upper-cased before parsing; invalid strings
#' return `NA`.  See also base R's [as.roman()] for the
#' reverse direction.
#'
#' @section Platform limits:
#' `baseToBase()` uses [strtoi()] internally, which operates
#' on `long int`.  On 32-bit platforms values 
#' above \eqn{2^{31} - 1}{2^31 - 1} may silently return `NA`.  
#' `decToBin()` applies the same cap
#' explicitly (values > `536870911` become `NA`).
#'
#' @name numeric-conversions
#'
#' @param x a vector of numbers or character strings representing values in
#'   the input base.  For `baseToBase()` a numeric `x` is accepted
#'   only when `from = 10`.  `NA` propagates to the output.
#' @param from a single integer in \[2, 36\] specifying the input base
#'   (`baseToBase()` only).
#' @param to a single integer in \[2, 36\] specifying the output base
#'   (`baseToBase()` only).
#' @param width a single non-negative integer or `NULL` (default).
#'   When given, output strings are left-padded with zeros to at least
#'   `width` characters (`baseToBase()` only).
#'
#' @return
#' a vector of the same length as `x`:
#' \itemize{
#'   \item `binToDec()`, `octToDec()`, `hexToDec()`,
#'         `romanToInt()` - integer or numeric vector.
#'   \item `decToHex()` - object of class [hexmode()].
#'   \item `decToOct()` - numeric vector (octal digit string coerced to
#'         numeric).
#'   \item `decToBin()`, `baseToBase()` - character vector
#'         (uppercase digits).
#' }
#' `NA` input always produces `NA` output.
#'
#' @examples
#' # binary
#' decToBin(c(0, 1, 17, 255))
#' binToDec(c("0", "1", "10001", "11111111"))
#'
#' # octal
#' decToOct(c(8, 64, 255))
#' octToDec(c(10, 100, 377))
#'
#' # hexadecimal  (CSS colour strings are also accepted by hexToDec)
#' decToHex(c(0, 255, 65535))
#' hexToDec(c("FF", "ff", "#1A2B3C"))
#'
#' # Roman numerals
#' romanToInt(c("I", "IV", "XIV", "MCMXCIX"))   # 1, 4, 14, 1999
#' romanToInt("invalid")                          # NA
#'
#' # baseToBase: general case
#' baseToBase("FF",       from = 16, to = 10)   # 255
#' baseToBase("255",      from = 10, to = 16)   # "FF"
#' baseToBase("11111111", from =  2, to = 10)   # 255
#' baseToBase("1A3F",     from = 16, to =  2)   # binary expansion
#' baseToBase("Z9",       from = 36, to = 10)   # base-36 -> decimal
#'
#' # fixed-width padding (useful for bit-pattern alignment)
#' baseToBase(c(0, 7, 255), from = 10, to = 2, width = 8)
#'
#' # vectorized over x
#' baseToBase(c("A", "B", "FF"), from = 16, to = 10)
#'
#' @seealso [strtoi()], [as.hexmode()],
#'   [as.octmode()], [as.roman()]
#'
#' @rdname numeric-conversions
#' @family number.baseconv
#' @concept encoding
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
  x <- as.numeric(x)
  z <- dec_to_bin_cpp(x)
  z[is.na(x) | x > 536870911] <- NA
  z <- sub("^0+", "", z)
  z[!is.na(z) & z == ""] <- "0"   # sub strips the only zero for input 0
  z
}

#' @rdname numeric-conversions
#' @export
romanToInt <- function(x) {
  roman <- trimws(toupper(as.character(x)))
  tryIt <- function(s) {
    retval <- try(roman_to_int_cpp(s), silent = TRUE)
    if (is.numeric(retval)) as.numeric(retval) else NA_real_
  }
  vapply(roman, tryIt, numeric(1L), USE.NAMES = FALSE)
}


#' @rdname numeric-conversions
#' @export
baseToBase <- function(x, from, to, width = NULL) {
  
  # --- validate from / to ----------------------------------------------
  .checkBase <- function(b, name) {
    if (!is.numeric(b) || length(b) != 1L || is.na(b) ||
        b < 2L || b > 36L || b %% 1 != 0)
      stop(gettextf(
        "Argument '%s' must be a single integer in [2, 36].", name
      ))
    as.integer(b)
  }
  from <- .checkBase(from, "from")
  to   <- .checkBase(to,   "to")
  
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
  .convertOne <- function(s) {
    
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

    if (dec < 0L)
      stop("Negative values are not supported.")
    
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
  
  vapply(x, .convertOne, character(1L), USE.NAMES = FALSE)
}


