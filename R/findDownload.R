
#' Locate a File in the Downloads Directory
#'
#' Returns the full path to a file located in the user's Downloads directory.
#'
#' @param fname Character string. Name of the file.
#'
#' @details
#' The function resolves the path to the user's Downloads directory using
#' an internal helper and appends \code{fname}. It does not perform any
#' downloading; it only locates files that already exist locally.
#'
#' If the file does not exist, an error is thrown.
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
#' @seealso \code{\link{readD}}
#'


#' @export
findDownload <- function(fname) {
  
  dir <- .getDownloadsPath()
  file <- file.path(dir, fname)
  
  if (!file.exists(file)) {
    stop(sprintf("File not found: %s", file))
  }
  
  file
}


# == internal helper functions ==========================================

.getDownloadsPath <- function() {
  
  # --- Windows ----------------------------------------------------------
  if (.Platform$OS.type == "windows") {
    
    key <- "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders"
    
    val <- tryCatch(
      utils::readRegistry(key, hive = "HCU"),
      error = function(e) NULL
    )
    
    if (!is.null(val)) {
      path <- val[["{374DE290-123F-4565-9164-39C4925E467B}"]]
      if (!is.null(path)) {
        return(normalizePath(path, winslash = "/", mustWork = FALSE))
      }
    }
  }
  
  # --- Linux (XDG) ------------------------------------------------------
  if (Sys.info()[["sysname"]] == "Linux") {
    
    xdg_file <- file.path(path.expand("~"), ".config", "user-dirs.dirs")
    
    if (file.exists(xdg_file)) {
      
      lines <- readLines(xdg_file, warn = FALSE)
      
      dl_line <- grep("XDG_DOWNLOAD_DIR", lines, value = TRUE)
      
      if (length(dl_line) > 0) {
        path <- sub('.*="(.*)"', "\\1", dl_line[1])
        path <- gsub("\\$HOME", path.expand("~"), path)
        
        return(normalizePath(path, winslash = "/", mustWork = FALSE))
      }
    }
  }
  
  # --- macOS fallback ---------------------------------------------------
  # (macOS normally uses ~/Downloads)
  
  # --- general fallback ---------------------------------------------
  file.path(path.expand("~"), "Downloads")
  
}







