
#' Convert Custom Date Format to strftime Format
#'
#' Translates a custom date format string using tokens like
#' \code{yyyy}, \code{mm}, \code{dd}, \code{mmm}, etc. into a valid
#' \code{strftime}-compatible format string (C-style).
#'
#' The function parses the input string sequentially and replaces
#' recognized tokens while leaving all other characters unchanged.
#' This makes it robust to compact formats (e.g. \code{yyyymmdd})
#' and mixed text.
#'
#' Supported tokens:
#' \itemize{
#'   \item \code{d}, \code{dd}, \code{ddd}, \code{dddd}
#'   \item \code{m}, \code{mm}, \code{mmm}, \code{mmmm}
#'   \item \code{y}, \code{yy}, \code{yyyy}
#' }
#'
#' Mapping:
#' \itemize{
#'   \item \code{yyyy} -> \code{\%Y}
#'   \item \code{yy}, \code{y} -> \code{\%y}
#'   \item \code{mm}, \code{m} -> \code{\%m}
#'   \item \code{mmm} -> \code{\%b}
#'   \item \code{mmmm} -> \code{\%B}
#'   \item \code{dd} -> \code{\%d}
#'   \item \code{d} -> \code{\%e}
#'   \item \code{ddd} -> \code{\%a}
#'   \item \code{dddd} -> \code{\%A}
#' }
#'
#' @param fmt character string. Custom date format.
#'
#' @return character string. A valid \code{strftime} format.
#'
#' @examples
#' asCDateFmt("yyyy-mm-dd")
#' asCDateFmt("dd.mm.yy")
#' asCDateFmt("yyyymmdd")
#' asCDateFmt("mmm d, yyyy")
#'
#' @family date.format
#' @concept date-time
#' @concept formatting
#' @export
asCDateFmt <- function(fmt) {
  as_cdate_fmt_cpp(fmt)
}
