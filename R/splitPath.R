

#' Split a File Path into Components
#'
#' Splits a file path into its components such as directory, file name,
#' and extension. The function is OS-aware and works on both Windows
#' and Unix-like systems.
#'
#' @param path A character vector of file paths.
#' @param last.is.file Logical; if \code{TRUE}, the last component of
#'   \code{path} is treated as a file name. If \code{FALSE}, it is treated
#'   as part of the directory path. If \code{NULL} (default), the function
#'   determines this automatically based on whether the path ends with a
#'   path separator.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{normpath}{Normalized path as returned by \code{\link{normalizePath}}.}
#'   \item{drive}{Drive letter on Windows systems (e.g., \code{"C:"}),
#'     otherwise \code{NA}.}
#'   \item{dirname}{Directory path including trailing separator.}
#'   \item{fullfilename}{Full file name including extension (if applicable).}
#'   \item{fullpath}{Full directory path (same as \code{dirname}).}
#'   \item{filename}{File name without extension.}
#'   \item{extension}{File extension without leading dot.}
#' }
#'
#' @details
#' The function uses \code{\link{basename}} and \code{\link{dirname}} for
#' platform-independent path handling. File name and extension are extracted
#' using \code{\link[tools]{file_path_sans_ext}} and \code{\link[tools]{file_ext}}.
#'
#' If \code{last.is.file = FALSE}, the path is treated as a directory and
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
#' splitPath("/home/user/folder/", last.is.file = FALSE)
#'
#' @family file utilities
#' @concept file paths
#' @concept string parsing
#'


#' @export
splitPath <- function(path, last.is.file = NULL) {
  
  path <- normalizePath(path, mustWork = FALSE)
  
  if (is.null(last.is.file)) {
    last.is.file <- !grepl("[/\\\\]$", path)
  }
  
  fullfilename <- if (last.is.file) basename(path) else NA_character_
  dirname_ <- dirname(path)
  
  if (!last.is.file) {
    dirname_ <- file.path(dirname_, basename(path))
  }
  
  filename <- if (last.is.file) tools::file_path_sans_ext(fullfilename) else NA_character_
  extension <- if (last.is.file) tools::file_ext(fullfilename) else NA_character_
  
  list(
    normpath = path,
    drive = if (.Platform$OS.type == "windows") substr(path, 1, 2) else NA_character_,
    dirname = paste0(dirname_, "/"),
    fullfilename = fullfilename,
    fullpath = paste0(dirname_, "/"),
    filename = filename,
    extension = extension
  )
}


