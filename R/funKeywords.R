
#' List Keywords For R Manual Pages
#'
#' List the keywords for specific R man pages or return a list of valid R
#' keywords.
#'
#' If `topic` is provided, return a list of the Keywords associated with
#' `topic`.  Otherwise, display the list of valid R Keywords from the R
#' doc/Keywords file.
#'
#' @param topic optional, object or man page topic.
#'
#' @return if `topic` is missing, the R keywords documentation file is
#'   opened for display via [file.show()], invisibly
#'   returning `NULL`. Otherwise, a character vector of topic names
#'   whose keywords match `topic`.
#'
#' @note
#' Substantially based on the `keywords()` function from the
#' \pkg{gtools} package by Gregory R. Warnes, with minor adaptations by the
#' package author.
#'
#' @examples
#' \donttest{
#' ## Show all valid R Keywords
#' funKeywords()
#'
#' ## Show Keywords associated with the 'merge' function
#' funKeywords(merge)
#' funKeywords("merge")
#' }
#' 
#' @seealso [help()]
#'
#' @family pkg.funinfo
#' @concept introspection
#' @concept programming
#' @export
funKeywords <- function(topic) {

  # essentially verbatim from the gtools package (see @author)

  file <- file.path(R.home("doc"), "KEYWORDS")

  if (missing(topic)) {

    file.show(file)

  } else {

    kw <- scan(file = file, what = character(), sep = "\n", quiet = TRUE)
    kw <- grep("&", kw, value = TRUE)
    kw <- gsub("&[^&]*$", "", kw)
    kw <- gsub("&+", " ", kw)
    kw <- na.omit(trimws(kw))

    ischar <- tryCatch(
      is.character(topic) && length(topic) == 1L,
      error = identity
    )
    if (inherits(ischar, "error"))
      ischar <- FALSE
    if (!ischar)
      topic <- deparse(substitute(topic))

    item <- paste("^", topic, "$", sep = "")

    topics <- function(k) {
      matches <- help.search(keyword = k)$matches
      matches[, match("topic", tolower(colnames(matches)))]
    }

    matches <- lapply(kw, topics)
    names(matches) <- kw

    tmp <- unlist(lapply(matches, function(m) grep(item, m, value = TRUE)))
    names(tmp)
  }
}
