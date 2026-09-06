
#' Construct a Normalized File Path
#'
#' Safely constructs a file path from a directory and a filename, independent
#' of whether the directory ends with a trailing slash. The resulting path
#' uses forward slashes.
#'
#' Trailing slashes (or backslashes) in `dir` are removed before the
#' components are joined, so `buildPath("data", "file.csv")` and
#' `buildPath("data/", "file.csv")` yield the same result.
#'
#' The path is then passed through
#' [normalizePath()]with
#' `mustWork = FALSE`, so paths that do not (yet) exist are allowed.
#' Note that `normalizePath` resolves existing paths to absolute form,
#' while non-existing paths are returned as constructed (i.e. possibly
#' relative).
#'
#' Both arguments are vectorized in the usual
#' [file.path()] manner.
#'
#' @param dir character string. Directory path.
#' @param filename character string. File name to append to `dir`.
#'
#' @return a character string representing the file path.
#'
#' @note Converting between forward slashes and backslashes is a frequent
#' necessity -- and a hassle -- especially in Windows. The
#' `cycleSlashes()` function in the `swissValet` package is
#' useful for this purpose.
#'
#' @examples
#' buildPath("data", "file.csv")
#' buildPath("data/", "file.csv")
#'
#' @family file.path
#' @concept path-handling
#' @concept string-manipulation
#' @export
buildPath <- function(dir, filename) {

  # strip trailing (back)slashes, so that the promise of slash-independence
  # holds for non-existing paths too (normalizePath leaves those untouched)
  dir <- sub("[/\\\\]+$", "", dir)

  normalizePath(file.path(dir, filename), winslash = "/", mustWork = FALSE)

}
