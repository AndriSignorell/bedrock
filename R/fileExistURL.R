
#' Check if a File Exists at a URL
#'
#' Performs an HTTP request to determine whether a resource exists at a given URL.
#' Uses a \code{HEAD} request by default and falls back to \code{GET} if necessary.
#'
#' The function first sends an HTTP \code{HEAD} request to minimize data transfer.
#' If the server responds with a status indicating that \code{HEAD} itself is not
#' supported (403, 405, 501), a \code{GET} request is attempted as a fallback. A
#' plain 404 is taken at face value, so that non-existing files do not trigger a
#' second request.
#'
#' If the request fails (e.g., due to network issues or invalid URLs), the function
#' returns \code{FALSE} and stores the error message as an attribute.
#'
#' @param url character string. The full URL to check.
#' @param timeout numeric. Timeout in seconds for the HTTP request. Default is 5.
#'
#' @return
#' logical value indicating whether the resource exists (\code{TRUE}) or not (\code{FALSE}).
#'
#' The returned value has additional attributes:
#' \itemize{
#'   \item \code{status}: HTTP status code returned by the server (e.g. 200, 404).
#'   \item \code{error}: error message (if a request error occurred).
#' }
#'
#' @examples
#' \dontrun{
#' fileExistURL("https://www.example.com/data.csv")
#'
#' res <- fileExistURL("https://invalid-url.test/file.csv")
#' attr(res, "status")
#' attr(res, "error")
#' }
#'
#' @family file.path
#' @concept path-handling
#' @concept type-test
#' @export
fileExistURL <- function(url, timeout = 5) {

  HTTP_STATUS_OK <- 200

  res <- tryCatch({
    r <- HEAD(url, httr::timeout(timeout))
    status <- status_code(r)

    # fallback: some servers do not properly support HEAD (403 Forbidden,
    # 405 Method Not Allowed, 501 Not Implemented); a plain 404 is trusted
    if (status %in% c(403L, 405L, 501L)) {
      r <- GET(url, httr::timeout(timeout))
      status <- status_code(r)
    }

    out <- status == HTTP_STATUS_OK
    attr(out, "status") <- status
    out

  }, error = function(e) {
    out <- FALSE
    attr(out, "status") <- NA_integer_
    attr(out, "error") <- e$message
    out
  })

  return(res)
}
