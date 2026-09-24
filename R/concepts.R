
#' Concept Utilities for Package Documentation
#'
#' Helper functions to inspect and analyse the use of
#' `\\concept` tags within a package.
#'
#' These utilities extract concept metadata from Rd files
#' and allow structured auditing of conceptual organisation
#' inside a package.
#'
#' **Functions**
#' \itemize{
#'   \item `getConcepts()` - Returns all unique concepts used in a package.
#'   \item `conceptMap()` - Returns a mapping of concepts to functions.
#'   \item `conceptAudit()` - Returns a summary table of concept usage.
#' }
#'
#' @param pkg character string. Name of the installed package.
#' @param prefix optional character string. If given, `getConcepts()`
#'   returns only the concepts starting with it, with the prefix removed.
#'
#' @name concepts
#' @details
#' The functions use [tools::Rd_db()] to parse Rd files and
#' extract `\\concept` tags programmatically.
#'
#' These tools are intended for package development,
#' documentation consistency checks, and conceptual audits.
#'
#' @return
#' \describe{
#'   \item{`getConcepts`}{character vector of unique concept names,
#'     sorted}
#'   \item{`conceptMap`}{named list mapping concepts to topics}
#'   \item{`conceptAudit`}{data frame with the columns `concept`
#'     and `nTopics`, ordered by decreasing frequency}
#' }
#'
#' @examples
#' getConcepts("stats")
#' getConcepts("stats", prefix = "erf")
#' head(conceptMap("stats"))
#' head(conceptAudit("stats"))
#'
#' @rdname concepts
#' @family pkg.introspection
#' @concept introspection
#' @export
getConcepts <- function(pkg, prefix = NULL) {

  res <- sort(unique(unlist(.conceptsByTopic(pkg), use.names = FALSE)))

  if (!is.null(prefix)) {
    if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix))
      stop("'prefix' must be a single character string")
    res <- substring(res[startsWith(res, prefix)], nchar(prefix) + 1L)
  }

  res
}


#' @rdname concepts
#' @export
conceptMap <- function(pkg) {

  byTopic <- .conceptsByTopic(pkg)

  # one topic entry per concept it carries; split() orders by concept
  split(rep(names(byTopic), lengths(byTopic)),
        unlist(byTopic, use.names = FALSE))
}


#' @rdname concepts
#' @export
conceptAudit <- function(pkg) {

  cm <- conceptMap(pkg)

  res <- data.frame(
    concept = names(cm),
    nTopics = lengths(cm, use.names = FALSE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  res[order(res$nTopics, res$concept, decreasing = c(TRUE, FALSE),
            method = "radix"), , drop = FALSE]
}


# == internal helper ======================================================

# Reads the Rd database once and returns a named list: one character
# vector of concepts per topic. All three exported functions were parsing
# the database themselves, twice over in conceptAudit().
#
# Two robustness points over the previous inline version:
#   * identical() instead of ==. attr(x, "Rd_tag") is NULL for a node
#     without a tag, and NULL == "\\concept" is logical(0); Filter()
#     drops those silently while keeping the element, which shifts the
#     whole index vector.
#   * the fragments of a concept node are pasted, not unlisted, so a tag
#     containing markup yields one string rather than several, and
#     surrounding whitespace is trimmed.
#' @noRd
.conceptsByTopic <- function(pkg) {

  if (!is.character(pkg) || length(pkg) != 1L || is.na(pkg))
    stop("'pkg' must be a single package name")

  rdDb <- tools::Rd_db(pkg)

  res <- lapply(rdDb, function(rd) {
    nodes <- Filter(function(z) identical(attr(z, "Rd_tag"), "\\concept"), rd)
    if (length(nodes) == 0L)
      return(character(0))
    vapply(nodes,
           function(z) trimws(paste(unlist(z), collapse = "")),
           character(1L), USE.NAMES = FALSE)
  })

  names(res) <- sub("\\.Rd$", "", names(rdDb))

  res[lengths(res) > 0L]
}
