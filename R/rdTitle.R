#' Extract the Title from an Rd Help File
#'
#' Searches all \file{.Rd} files in a package's \file{man/} directory for a
#' given topic (matched against \code{\\alias} entries) and returns its
#' \code{\\title} string.
#'
#' @param topic a single character string giving the topic (function name or
#'   alias) to look up
#' @param man a single character string giving the path to the directory
#'   containing \file{.Rd} files.  Defaults to \code{"man"}, i.e. the
#'   \file{man/} subdirectory of the current working directory.
#'
#' @return A single character string with the title, trimmed of leading and
#'   trailing whitespace.  Stops with an error if \code{topic} is not found.
#'
#' @seealso \code{\link[tools]{parse_Rd}}
#'
#' @examples
#' \dontrun{
#' rdTitle("mean")
#' rdTitle("lm", man = "path/to/pkg/man")
#' }
#'

#' @family pkg.funinfo
#' @concept introspection
#' @concept programming
#' @export
rdTitle <- function(topic, man = "man") {

  if (!is.character(topic) || length(topic) != 1L)
    stop("'topic' must be a single character string.")

  if (!dir.exists(man))
    stop("Directory not found: ", sQuote(man))

  files <- list.files(man, pattern = "\\.Rd$", full.names = TRUE)

  if (!length(files))
    stop("No .Rd files found in ", sQuote(man))

  for (f in files) {

    rd <- tools::parse_Rd(f)

    aliases <- vapply(
      Filter(function(x) attr(x, "Rd_tag") == "\\alias", rd),
      function(x) trimws(paste(unlist(x), collapse = "")),
      character(1L)
    )

    if (topic %in% aliases) {

      title <- Filter(function(x) attr(x, "Rd_tag") == "\\title", rd)

      if (!length(title))
        return(NA_character_)

      return(trimws(paste(unlist(title[[1]]), collapse = "")))
    }
  }

  # Topic not found...
  NA_character_
  
}
