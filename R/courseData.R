
#' Load Course Dataset from Server
#'
#' Downloads and loads a dataset from predefined course servers or a user-defined URL.
#'
#' If no \code{url} is provided, the function searches for the file in the following locations:
#' \itemize{
#'   \item \url{https://www.signorell.net/hwz/datasets/}
#'   \item \url{https://www.signorell.net/buch/}
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
#' @param name character string. File name including extension (e.g. \code{"data.csv"}).
#' @param url optional character string. Base URL where the file is located.
#'   If \code{NULL}, default course repositories are searched.
#' @param header logical. Whether the file contains a header row. Passed to \code{read.table()}.
#' @param sep character. Field separator used in the file. Default is \code{";"}.
#' @param ... additional arguments passed to the underlying import functions
#'   such as \code{read.table()} or \code{openDataObject()}.
#'
#' @return
#' a data frame or object returned by the respective import function:
#' \itemize{
#'   \item for text files: a \code{data.frame}.
#'   \item for Excel files: an object returned by \code{openDataObject()}.
#' }
#'
#' @examples
#' \dontrun{
#' # Load from default repositories
#' courseData("mydata.csv")
#'
#' # Load from custom URL
#' courseData("mydata.csv", url = "https://example.com/data/")
#' }
#'
#' @family datasets
#' @concept dataset
#' @concept file-io
#' @export
courseData <- function(name,
                       url    = NULL,
                       header = TRUE,
                       sep    = ";",
                       ...) {

  if (is.null(url)) {
    candidates <- c(
      "https://www.signorell.net/hwz/datasets/",
      "https://www.signorell.net/buch/"
    )
    url <- .resolveCourseURL(name, candidates)

    if (is.null(url)) {
      stop(sprintf(
        "File '%s' was not found in any of the search paths:\n%s",
        name,
        paste(candidates, collapse = "\n")
      ))
    }
  } else {
    if (!fileExistURL(file.path(sub("/+$", "", url), name))) {
      stop(sprintf("File '%s/%s' does not exist.", sub("/+$", "", url), name))
    }
  }

  url <- sub("/+$", "", url)

  ext <- tolower(tools::file_ext(name))
  fullPath <- file.path(url, name)

  if (ext %in% c("xls", "xlsx")) {
    return(openDataObject(name = name, url = url, doc = NA, ...))
  } else {
    return(read.table(fullPath, header = header, sep = sep, ...))
  }
}




# == internal helper functions ===========================================


#' @keywords internal
#' @noRd
.resolveCourseURL <- function(name, candidates) {

  # yields first valid URL or NULL

  for (base in candidates) {
    full <- file.path(sub("/+$", "", base), name)
    if (fileExistURL(full)) return(base)
  }
  return(NULL)
}
