
#' Check Whether a String Is a URL
#'
#' Returns `TRUE` if the given string starts with a recognised URL scheme,
#' `FALSE` otherwise. Convenience wrapper around the internal
#' \code{.detectInputType()} helper.
#'
#' @param x `character(1)` - the string to test.
#'
#' @return `logical(1)` - `TRUE` if `x` is a URL, `FALSE` otherwise.
#'
#' @seealso [isFilePath()] for the complementary file-path check.
#'
#' @examples
#' isURL("https://example.com/data.csv")   # TRUE
#' isURL("ftp://files.example.org/x.zip")  # TRUE
#' isURL("s3://my-bucket/file.parquet")    # TRUE
#' isURL("/home/user/file.csv")            # FALSE
#' isURL("./script.R")                     # FALSE
#'
#' @family data.inspection
#' @concept ordering
#' @export
isURL <- function(x) {
  .detectInputType(x) == "url"
}


#' Check Whether a String Is a File Path
#'
#' Returns `TRUE` if the given string looks like a local file path (absolute
#' or relative, Unix/Windows style), `FALSE` otherwise. Convenience wrapper
#' around the internal \code{.detectInputType()} helper.
#'
#' @param x `character(1)` - the string to test.
#'
#' @return `logical(1)` - `TRUE` if `x` is a file path, `FALSE` otherwise.
#'
#' @seealso [isURL()] for the complementary URL check.
#'
#' @examples
#' isFilePath("/home/user/data/file.csv")   # TRUE
#' isFilePath("~/documents/report.pdf")     # TRUE
#' isFilePath("./relative/path/file.R")     # TRUE
#' isFilePath("../other/folder/data.rds")   # TRUE
#' isFilePath("C:/Users/Hans/file.xlsx")    # TRUE
#' isFilePath("https://example.com/f.csv")  # FALSE
#'
#' @family data.inspection
#' @concept ordering
#' @export
isFilePath <- function(x) {
  .detectInputType(x) == "filepath"
}


# == internal helper functions =====================================

# .detectInputType
#
# Detect whether a string is a URL, a file path, or neither.
#
# Parameters:
#   x  character(1) - the string to inspect.
#
# Returns:
#   character(1) - one of "url", "filepath", or "unknown".

.detectInputType <- function(x) {

  if (!is.character(x) || length(x) != 1L)
    stop("'x' must be a single character string.")

  # --- URL detection ----------------------------------------------------
  # Match any known protocol scheme at the start of the string
  urlPattern <- paste0(
    "^(",
    "https?://",          # HTTP and HTTPS
    "|ftps?://",          # FTP and FTPS
    "|file://",           # local file URI
    "|s3://|gs://|az://", # cloud storage (AWS, GCP, Azure)
    ")"
  )

  if (grepl(urlPattern, x, ignore.case = TRUE)) {
    return("url")
  }

  # --- File path detection ----------------------------------------------
  # Match common path prefixes for Unix, macOS, and Windows
  pathPattern <- paste0(
    "^(",
    "/",                  # absolute Unix path
    "|~/",                # home directory shorthand
    "|\\.{1,2}/",         # relative path: ./ or ../
    "|[A-Za-z]:[/\\\\]",  # Windows drive letter: C:/ or C:\
    ")"
  )

  if (grepl(pathPattern, x)) {
    return("filepath")
  }

  # If the string contains any path separator but no URL scheme,
  # it is most likely a relative or ambiguous file path
  if (grepl("[/\\\\]", x)) {
    return("filepath")
  }

  # --- Fallback ---------------------------------------------------------
  return("unknown")
}
