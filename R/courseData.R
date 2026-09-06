
#' Load Course Dataset from Server
#'
#' Downloads and loads a dataset from predefined course servers or a user-defined URL.
#'
#' If no `url` is provided, the function searches for the file in 
#' the following locations (see <https://github.com/AndriSignorell/Teaching>):
#' \itemize{
#'   \item `https://raw.githubusercontent.com/AndriSignorell/Teaching/main/book/`
#'   \item `https://raw.githubusercontent.com/AndriSignorell/Teaching/main/data/`
#' }
#'
#' The first location where the file exists is used.
#'
#' File type handling:
#' \itemize{
#'   \item `.xls`, `.xlsx`: loaded via `openDataObject()`
#'   \item other files: loaded via `read.table()`
#' }
#'
#' @param name character string. File name including extension (e.g. `"data.csv"`).
#' @param url optional character string. Base URL where the file is located.
#'   If `NULL`, default course repositories are searched.
#' @param header logical. Whether the file contains a header row. Passed to `read.table()`.
#' @param sep character. Field separator used in the file. Default is `";"`.
#' @param ... additional arguments passed to the underlying import functions
#'   such as `read.table()` or `openDataObject()`.
#'
#' @return
#' a data frame or object returned by the respective import function:
#' \itemize{
#'   \item for text files: a `data.frame`.
#'   \item for Excel files: an object returned by `openDataObject()`.
#' }
#'
#' @examples
#' \dontrun{
#' # Load from default repositories
#' courseData("fullmoon.xlsx")
#'
#' # Load from custom URL
#' courseData("mydata.csv", url = "https://example.com/data/")
#' }
#'
#' @family datasets
#' @concept file.io
#' @export
courseData <- function(name,
                       url    = NULL,
                       header = TRUE,
                       sep    = ";",
                       ...) {

  if (is.null(url)) {
    candidates <- c(
      "https://raw.githubusercontent.com/AndriSignorell/Teaching/main/book/",
      "https://raw.githubusercontent.com/AndriSignorell/Teaching/main/data/"
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
