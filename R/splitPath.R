

#' Split a File Path into Its Components
#'
#' Splits a file path into its components such as directory, file name,
#' and extension. The function is OS-aware and works on both Windows
#' and Unix-like systems.
#'
#' @param path a character vector of file paths
#' @param lastIsFile logical; if \code{TRUE}, the last component of
#'   \code{path} is treated as a file name. If \code{FALSE}, it is treated
#'   as part of the directory path. If \code{NULL} (default), the function
#'   determines this automatically based on whether the path ends with a
#'   path separator.
#'
#' @return A list with the following components (each a vector of the same
#' length as \code{path}):
#' \describe{
#'   \item{normpath}{Normalized path as returned by \code{\link{normalizePath}}.}
#'   \item{drive}{Drive letter on Windows systems (e.g., \code{"C:"}),
#'     otherwise \code{NA}.}
#'   \item{dirname}{Directory path without drive letter, including trailing
#'     separator.}
#'   \item{fullfilename}{Full file name including extension (if applicable).}
#'   \item{fullpath}{Full directory path including drive letter and trailing
#'     separator.}
#'   \item{filename}{File name without extension.}
#'   \item{extension}{File extension without leading dot.}
#' }
#'
#' @details
#' The function uses \code{\link{basename}} and \code{\link{dirname}} for
#' platform-independent path handling. File name and extension are extracted
#' using \code{\link[tools]{file_path_sans_ext}} and \code{\link[tools]{file_ext}}.
#'
#' If \code{lastIsFile = FALSE}, the path is treated as a directory and
#' file-related components (\code{fullfilename}, \code{filename},
#' \code{extension}) are returned as \code{NA}.
#'
#' @seealso \code{\link{basename}}, \code{\link{dirname}},
#' \code{\link[tools]{file_ext}}, \code{\link[tools]{file_path_sans_ext}}
#'
#' @examples
#' splitPath("C:/temp/file.txt")
#'
#' splitPath("/home/user/data.csv")
#'
#' # treat as directory
#' splitPath("/home/user/folder/", lastIsFile = FALSE)
#'



#' @family file.path
#' @concept path-handling
#' @concept string-manipulation
#' @export
splitPath <- function(path, lastIsFile = NULL) {

  if (!is.character(path))
    stop("'path' must be a character vector.")

  # detect trailing separator BEFORE normalizePath(), which strips it
  if (is.null(lastIsFile)) {
    lastIsFile <- !grepl("[/\\\\]$", path)
  } else {
    lastIsFile <- rep_len(as.logical(lastIsFile), length(path))
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  hasDrive <- .Platform$OS.type == "windows" & grepl("^[A-Za-z]:", path)
  drive    <- ifelse(hasDrive, substr(path, 1L, 2L), NA_character_)

  fullfilename <- ifelse(lastIsFile, basename(path), NA_character_)
  dirn         <- ifelse(lastIsFile, dirname(path), path)

  filename  <- ifelse(lastIsFile,
                      tools::file_path_sans_ext(fullfilename),
                      NA_character_)
  extension <- ifelse(lastIsFile,
                      tools::file_ext(fullfilename),
                      NA_character_)

  # dirname without the drive letter (fixed-prefix strip, no regex)
  dirnNoDrive <- ifelse(hasDrive, substring(dirn, 3L), dirn)

  list(
    normpath     = path,
    drive        = drive,
    dirname      = paste0(dirnNoDrive, "/"),
    fullfilename = fullfilename,
    fullpath     = paste0(dirn, "/"),
    filename     = filename,
    extension    = extension
  )
}


