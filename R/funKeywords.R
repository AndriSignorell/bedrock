
#' List Keywords For R Manual Pages
#'
#' List the keywords for specific R man pages or return a list of valid R
#' keywords.
#'
#' If \code{topic} is provided, return a list of the Keywords associated with
#' \code{topic}.  Otherwise, display the list of valid R Keywords from the R
#' doc/Keywords file.
#'
#' @param topic optional, object or man page topic
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[utils]{help}}
#' @keywords documentation
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
#' @family pkg.introspection
#' @concept introspection
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
