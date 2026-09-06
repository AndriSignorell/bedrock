
#' Convert Custom Date Format to strftime Format
#'
#' Translates a custom date format string using tokens like
#' `yyyy`, `mm`, `dd`, `mmm`, etc. into a valid
#' `strftime`-compatible format string (C-style).
#'
#' The function parses the input string sequentially and replaces
#' recognized tokens while leaving all other characters unchanged.
#' This makes it robust to compact formats (e.g. `yyyymmdd`)
#' and mixed text.
#'
#' Supported tokens:
#' \itemize{
#'   \item `d`, `dd`, `ddd`, `dddd`
#'   \item `m`, `mm`, `mmm`, `mmmm`
#'   \item `y`, `yy`, `yyyy`
#' }
#'
#' Mapping:
#' \itemize{
#'   \item `yyyy` -> `\%Y`
#'   \item `yy`, `y` -> `\%y`
#'   \item `mm`, `m` -> `\%m`
#'   \item `mmm` -> `\%b`
#'   \item `mmmm` -> `\%B`
#'   \item `dd` -> `\%d`
#'   \item `d` -> `\%e`
#'   \item `ddd` -> `\%a`
#'   \item `dddd` -> `\%A`
#' }
#'
#' @param fmt character string. Custom date format.
#'
#' @return character string. A valid `strftime` format.
#'
#' @examples
#' asCDateFmt("yyyy-mm-dd")
#' asCDateFmt("dd.mm.yy")
#' asCDateFmt("yyyymmdd")
#' asCDateFmt("mmm d, yyyy")
#'
#' @family date.format
#' @concept formatting
#' @export
asCDateFmt <- function(fmt) {
  as_cdate_fmt_cpp(fmt)
}
