
#' Extract the Title from an Rd Help File
#'
#' Searches all \file{.Rd} files in a package's \file{man/} directory for a
#' given topic (matched against `\\alias` entries) and returns its
#' `\\title` string.
#'
#' @param topic a single character string giving the topic (function name or
#'   alias) to look up.
#' @param man a single character string giving the path to the directory
#'   containing \file{.Rd} files.  Defaults to `"man"`, i.e. the
#'   \file{man/} subdirectory of the current working directory.
#'
#' @return a single character string with the title, trimmed of leading and
#'   trailing whitespace.  Stops with an error if `topic` is not found.
#'
#' @seealso [tools::parse_Rd()]
#'
#' @examples
#' # a minimal man/ directory to search in
#' man <- file.path(tempdir(), "man")
#' dir.create(man, showWarnings = FALSE)
#'
#' writeLines(c("\\\\name{foo}", "\\\\alias{foo}", "\\\\alias{bar}",
#'              "\\\\title{A Minimal Help Page}",
#'              "\\\\description{Nothing to see here.}"),
#'            file.path(man, "foo.Rd"))
#'
#' rdTitle("foo", man = man)
#' rdTitle("bar", man = man)          # aliases are matched as well
#' rdTitle("nothing", man = man)      # NA
#'
#' unlink(man, recursive = TRUE)
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
