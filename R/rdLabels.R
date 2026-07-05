
#' Extract Variable Labels from Rd Documentation
#'
#' Extracts variable descriptions from the \code{\\describe} section of a
#' dataset's Rd documentation and returns them as a named character vector.
#' The names correspond to variable names and the values to their
#' descriptions.
#'
#' This function is useful for automatically generating variable labels from
#' documented datasets in R packages.
#'
#' The function parses the Rd database via \code{\link[tools:Rd_db]{tools::Rd_db}}
#' and recursively searches for the \code{\\describe} section. It then extracts
#' all \code{\\item\{var\}\{description\}} entries. Note that dataset pages
#' documenting their variables with \code{\\tabular} instead of
#' \code{\\describe} (as many base R datasets do) are not supported and
#' raise an error.
#'
#' If no Rd file matches \code{dataName} directly, the aliases of all Rd
#' files are searched, so that datasets documented on pages with a
#' different file name are found as well.
#'
#' @param dataName Character string. Name of the dataset.
#' @param package Character string. Name of the package containing the dataset.
#'
#' @return A named character vector where names are variable names and values
#'   are their corresponding descriptions extracted from the Rd file.
#'
#' @examples
#' # Extract labels from a package dataset
#' \dontrun{
#' rdLabels("Pizza", "bedrock")
#' }
#'
#' @seealso \code{\link{label}}
#'
#' @family pkg.introspection
#' @concept label
#' @export
rdLabels <- function(dataName, package) {

  if (missing(package)) {
    stop("Please provide a package name.")
  }

  # --- 1. load the Rd database ---
  rdDb <- tools::Rd_db(package)

  # Rd file names typically look like "Pizza.Rd"
  rdName <- paste0(dataName, ".Rd")

  if (!rdName %in% names(rdDb)) {
    # fall back to searching the aliases: the documenting file may be
    # named differently from the dataset (@rdname, combined pages, ...)
    hit <- NULL
    for (nm in names(rdDb)) {
      aliases <- vapply(
        Filter(function(x) identical(attr(x, "Rd_tag"), "\\alias"), rdDb[[nm]]),
        function(x) trimws(paste(unlist(x), collapse = "")),
        character(1L)
      )
      if (dataName %in% aliases) {
        hit <- nm
        break
      }
    }
    if (is.null(hit)) {
      stop("No Rd entry found for ", dataName)
    }
    rdName <- hit
  }

  rd <- rdDb[[rdName]]

  # --- 2. find \describe ---
  descNode <- .findRdTag(rd, "\\describe")

  if (is.null(descNode)) {
    stop("No \\describe section found.")
  }

  # --- 3. extract the items ---
  labels <- list()

  for (item in descNode) {
    if (identical(attr(item, "Rd_tag"), "\\item")) {

      # remove white space and line breaks
      var <- gsub("\\s+", "", paste(unlist(item[[1]]), collapse = ""))
      desc <- trimws(paste(unlist(item[[2]]), collapse = " "))

      labels[[var]] <- desc
    }
  }

  return(unlist(labels))

}



# == internal helper functions =====================================

.findRdTag <- function(x, tag) {

  # --- only check whether Rd_tag exists ---
  if (identical(attr(x, "Rd_tag"), tag))
    return(x)

  # --- recursively through lists ---
  if (is.list(x)) {
    for (el in x) {
      res <- .findRdTag(el, tag)
      if (!is.null(res)) return(res)
    }
  }

  NULL

}
