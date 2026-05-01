
#' Check if a File Exists at a URL
#'
#' Performs an HTTP request to determine whether a resource exists at a given URL.
#' Uses a \code{HEAD} request by default and falls back to \code{GET} if necessary.
#'
#' @param url Character string. The full URL to check.
#' @param timeout Numeric. Timeout in seconds for the HTTP request. Default is 5.
#'
#' @return
#' Logical value indicating whether the resource exists (\code{TRUE}) or not (\code{FALSE}).
#'
#' The returned value has additional attributes:
#' \itemize{
#'   \item \code{status}: HTTP status code returned by the server (e.g. 200, 404)
#'   \item \code{error}: Error message (if a request error occurred)
#' }
#'
#' @details
#' The function first sends an HTTP \code{HEAD} request to minimize data transfer.
#' If the server responds with an error status (>= 400), a \code{GET} request is
#' attempted as a fallback because some servers do not properly support \code{HEAD}.
#'
#' If the request fails (e.g., due to network issues or invalid URLs), the function
#' returns \code{FALSE} and stores the error message as an attribute.
#'
#' @examples
#' \dontrun{
#' fileExistURL("http://www.example.com/data.csv")
#'
#' res <- fileExistURL("http://invalid-url.test/file.csv")
#' attr(res, "status")
#' attr(res, "error")
#' }
#'
#' @importFrom httr HEAD GET timeout status_code
#'


#' @family file.utils
#' @concept file-utilities
#' @concept data-inspection
#'
#'
#' @export
fileExistURL <- function(url, timeout = 5) {
  HTTP_STATUS_OK <- 200
  
  res <- tryCatch({
    r <- httr::HEAD(url, httr::timeout(timeout))
    status <- httr::status_code(r)
    
    # Fallback: try GET if HEAD fails
    if (status >= 400) {
      r <- httr::GET(url, httr::timeout(timeout))
      status <- httr::status_code(r)
    }
    
    out <- status == HTTP_STATUS_OK
    attr(out, "status") <- status
    out
    
  }, error = function(e) {
    out <- FALSE
    attr(out, "status") <- NA
    attr(out, "error") <- e$message
    out
  })
  
  return(res)
}

