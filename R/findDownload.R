
#' Locate a File in the Downloads Directory
#'
#' Returns the full path to a file located in the user's Downloads directory.
#'
#' The function resolves the path to the user's Downloads directory using
#' an internal helper and appends \code{file}. It does not perform any
#' downloading; it only locates files that already exist locally.
#'
#' If the file does not exist, an error is thrown.
#'
#' @param file character string. Name of the file.
#'
#' @return
#' A character string giving the full path to the file.
#'
#' @examples
#' \dontrun{
#' # Locate a file in Downloads
#' findDownload("data.csv")
#' }
#'
#' @family file.path
#' @concept path-handling
#' @concept file-io
#' @export
findDownload <- function(file) {

  path <- file.path(.getDownloadsPath(), file)

  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path))
  }

  path
}


# == internal helper functions ==========================================

.getDownloadsPath <- function() {

  # --- Windows ----------------------------------------------------------
  if (.Platform$OS.type == "windows") {

    key <- "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders"

    # readRegistry only exists in Windows builds of R; a direct
    # utils::readRegistry reference would raise a NOTE when the package
    # is checked on other platforms
    readReg <- tryCatch(
      get("readRegistry", envir = asNamespace("utils")),
      error = function(e) NULL
    )

    val <- if (!is.null(readReg)) {
      tryCatch(readReg(key, hive = "HCU"), error = function(e) NULL)
    }

    if (!is.null(val)) {
      path <- val[["{374DE290-123F-4565-9164-39C4925E467B}"]]
      if (!is.null(path)) {
        return(normalizePath(path, winslash = "/", mustWork = FALSE))
      }
    }
  }

  # --- Linux (XDG) ------------------------------------------------------
  if (Sys.info()[["sysname"]] == "Linux") {

    xdgFile <- file.path(path.expand("~"), ".config", "user-dirs.dirs")

    if (file.exists(xdgFile)) {

      lines <- readLines(xdgFile, warn = FALSE)

      dlLine <- grep("XDG_DOWNLOAD_DIR", lines, value = TRUE)

      if (length(dlLine) > 0) {
        path <- sub('.*="(.*)"', "\\1", dlLine[1])
        path <- gsub("\\$HOME", path.expand("~"), path)

        return(normalizePath(path, winslash = "/", mustWork = FALSE))
      }
    }
  }

  # --- general fallback (macOS and others use ~/Downloads) ---------------
  file.path(path.expand("~"), "Downloads")

}
