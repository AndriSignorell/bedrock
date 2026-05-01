
#' Construct a normalized file path
#'
#' Safely constructs a file path from a directory and a filename, independent
#' of whether the directory ends with a trailing slash. The resulting path is
#' normalized and uses forward slashes.
#'
#' @param dir Character string. Directory path.
#' @param filename Character string. File name to append to \code{dir}.
#'
#' @return A character string representing the normalized file path.
#'
#' @details
#' This function is a thin wrapper around \code{\link[base:file.path]{file.path}}
#' and \code{\link[base:normalizePath]{normalizePath}}. It ensures consistent path
#' construction across platforms and avoids issues with trailing slashes.
#'
#' The argument \code{mustWork = FALSE} allows returning paths that do not yet exist.
#'
#' @examples
#' buildPath("data", "file.csv")
#' buildPath("data/", "file.csv")
#'

#' @family file.utils
#' @concept file-utilities
#' @concept string-manipulation
#'
#'
#' @export
buildPath <- function(dir, filename) {
  
  # constructs ‘dir/filename’ reliably, regardless of whether ‘dir’ is 
  # followed by a trailing slash or not
  
  normalizePath(file.path(dir, filename), winslash = "/", mustWork = FALSE)
  
}


