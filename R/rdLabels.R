
#' Extract Variable Labels from Rd Documentation
#'
#' Reads the variable descriptions out of the `\describe` section of a
#' documented dataset and returns them as a named character vector, the names
#' being the variable names. This turns documentation that already exists into
#' labels usable in tables, plots and codebooks, instead of maintaining the
#' same descriptions a second time in the code.
#'
#' @param dataName character string, the name of the dataset.
#' @param package character string, the name of the package holding the
#'   dataset.
#'
#' @return a named character vector of variable descriptions, the names being
#'   the variable names.
#'
#' @details
#' The Rd database is read with [tools::Rd_db()] and searched recursively for
#' the first `\describe` section, from which all
#' \code{\\item\{var\}\{description\}} entries are taken. Only that first
#' section is read: on a page documenting more than one dataset, the labels of
#' the first one are returned.
#'
#' Descriptions are returned as written in the Rd file, with whitespace and
#' line breaks collapsed to single spaces. Rd markup inside a description, such
#' as `\code{}` or `\eqn{}`, contributes its content without the surrounding
#' command.
#'
#' The package must be installed, as the documentation is read from the
#' installed Rd database rather than from the sources.
#'
#' @examples
#' \dontrun{
#' rdLabels("Pizza", "bedrock")
#' ## price               temperature         delivery_min
#' ## "Price of the ..."  "Temperature ..."   "Delivery ..."
#' }
#'
#' @seealso [tools::Rd_db()]
#'
#' @family pkg.funinfo
#' @concept introspection
#' @concept label
#' @importFrom tools Rd_db
#' @export
rdLabels <- function(dataName, package) {

  if (missing(dataName))
    stop("'dataName' is missing")

  if (missing(package))
    stop("'package' is missing")

  checkString(dataName)
  checkString(package)

  # --- 1. load the Rd database ---
  rdDb <- Rd_db(package)

  # entries are named after their file, e.g. "Pizza.Rd"
  rdName <- paste0(dataName, ".Rd")

  if (!rdName %in% names(rdDb))
    stop("no Rd entry found for ", dataName, " in package ", package)

  rd <- rdDb[[rdName]]

  # --- 2. find the \describe section ---
  descNode <- .findRdTag(rd, "\\describe")

  if (is.null(descNode))
    stop("no \\describe section found in ", rdName)

  # --- 3. extract the items ---
  items <- Filter(function(el) identical(attr(el, "Rd_tag"), "\\item"),
                  descNode)

  if (length(items) == 0L)
    stop("no \\item entries found in the \\describe section of ", rdName)

  # parse_Rd() keeps the line breaks of the source inside a text fragment, so
  # runs of whitespace are collapsed rather than only trimmed at the ends
  labels <- vapply(items, function(el)
                     trimws(gsub("\\s+", " ",
                                 paste(unlist(el[[2L]]), collapse = " "))),
                   character(1L), USE.NAMES = FALSE)

  names(labels) <- vapply(items, function(el)
                            gsub("\\s+", "", paste(unlist(el[[1L]]),
                                                   collapse = "")),
                          character(1L), USE.NAMES = FALSE)

  labels

}



# == internal helper functions =====================================

# depth-first search for the first node carrying the given Rd tag,
# returning NULL when the tree holds none
.findRdTag <- function(x, tag) {

  if (identical(attr(x, "Rd_tag"), tag))
    return(x)

  if (is.list(x)) {
    for (el in x) {
      res <- .findRdTag(el, tag)
      if (!is.null(res))
        return(res)
    }
  }

  NULL

}
