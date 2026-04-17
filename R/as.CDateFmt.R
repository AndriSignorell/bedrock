
#' Convert custom date format to strftime format
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
#' @param fmt Character string. Custom date format.
#'
#' @return Character string. A valid \code{strftime} format.
#'
#' @examples
#' as.CDateFmt("yyyy-mm-dd")
#' as.CDateFmt("dd.mm.yy")
#' as.CDateFmt("yyyymmdd")
#' as.CDateFmt("mmm d, yyyy")
#'


#' @export
as.CDateFmt <- function(fmt) {
  as_CDateFmt_cpp(fmt)
}

