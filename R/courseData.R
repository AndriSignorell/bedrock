
#' Load Course Dataset from Server
#'
#' Downloads and loads a dataset from predefined course servers or a user-defined URL.
#'
#' @param name Character string. File name including extension (e.g. \code{"data.csv"}).
#' @param url Optional character string. Base URL where the file is located.
#'   If \code{NULL}, default course repositories are searched.
#' @param header Logical. Whether the file contains a header row. Passed to \code{read.table()}.
#' @param sep Character. Field separator used in the file. Default is \code{";"}.
#' @param ... Additional arguments passed to the underlying import functions
#'   such as \code{read.table()} or \code{openDataObject()}.
#'
#' @return
#' A data frame or object returned by the respective import function:
#' \itemize{
#'   \item For text files: a \code{data.frame}
#'   \item For Excel files: an object returned by \code{openDataObject()}
#' }
#'
#' @details
#' If no \code{url} is provided, the function searches for the file in the following locations:
#' \itemize{
#'   \item \url{http://www.signorell.net/hwz/datasets/}
#'   \item \url{http://www.signorell.net/buch/}
#' }
#'
#' The first location where the file exists is used.
#'
#' File type handling:
#' \itemize{
#'   \item \code{.xls}, \code{.xlsx}: loaded via \code{openDataObject()}
#'   \item other files: loaded via \code{read.table()}
#' }
#'
#' @examples
#' \dontrun{
#' # Load from default repositories
#' courseData("mydata.csv")
#'
#' # Load from custom URL
#' courseData("mydata.csv", url = "http://example.com/data/")
#' }
#'
#' @importFrom utils read.table
#' @seealso \code{\link{fileExistURL}}
#'


#' @family datasets
#' @concept datasets
#' @concept data-manipulation
#'
#'
#' @export
courseData <- function(name,
                       url    = NULL,
                       header = TRUE,
                       sep    = ";",
                       ...) {
  
  if (is.null(url)) {
    candidates <- c(
      "http://www.signorell.net/hwz/datasets/",
      "http://www.signorell.net/buch/"
    )
    url <- .resolveCourseURL(name, candidates)
    
    if (is.null(url)) {
      stop(sprintf(
        "Datei '%s' wurde in keinem Suchpfad gefunden:\n%s",
        name,
        paste(candidates, collapse = "\n")
      ))
    }
  } else {
    if (!fileExistURL(file.path(url, name))) {
      stop(sprintf("Datei '%s/%s' existiert nicht.", url, name))
    }
  }
  
  ext <- tolower(tools::file_ext(name))
  full_path <- file.path(url, name)
  
  if (ext %in% c("xls", "xlsx")) {
    return(openDataObject(name = name, url = url, doc = NA, ...))
  } else {
    return(read.table(full_path, header = header, sep = sep, ...))
  }
}




# == internal helper functions ===========================================


#' @keywords internal
.resolveCourseURL <- function(name, candidates) {
  
  # yields first valid URL or NULL
  
  for (base in candidates) {
    full <- file.path(base, name)
    if (fileExistURL(full)) return(base)
  }
  return(NULL)
}


