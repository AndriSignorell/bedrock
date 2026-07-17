
#' List Calls Used in Function
#'
#' For screening purposes it can be useful to get a list of all function calls
#' our function may depend on. \code{funCalls()} parses the function
#' source and returns all found function calls grouped by their package.
#'
#' The source packages are resolved via \code{\link[utils]{find}}, which only
#' sees attached packages. Calls to functions from packages that are not on
#' the search path are reported under \code{"<not found>"}.
#'
#' @param name the name of the function.
#' @param package optional name of a package; if given, the result is
#'   filtered to source environments matching \code{package}.
#' @param sorted logical; whether calls are sorted alphabetically. Defaults to
#'   \code{FALSE}.
#'
#' @return a list of character vectors with the function calls, grouped by
#'   the environment the called functions were found in.
#'
#' @note
#' Based on code by Nicholas Cooper.
#'
#' @seealso \code{\link{funList}}, \code{\link{funArgs}},
#' \code{\link[utils]{getParseData}}
#'
#' @examples
#'
#' funCalls("combN", package="bedrock")
#'
#' @family pkg.funinfo
#' @concept introspection
#' @concept programming
#' @export
funCalls <- function(name, package = NULL, sorted = FALSE) {

  fn <- get(name, mode = "function", envir = parent.frame())

  # parse the deparsed source: parsing the getAnywhere() object itself
  # would inject a phantom 'list' call into the results
  tmp <- utils::getParseData(
    parse(text = deparse(fn), keep.source = TRUE)
  )

  nms <- unique(tmp$text[tmp$token == "SYMBOL_FUNCTION_CALL"])

  if (sorted)
    nms <- sort(nms)

  src <- vapply(
    nms,
    function(f) paste(utils::find(f), collapse = ", "),
    character(1)
  )
  src[!nzchar(src)] <- "<not found>"

  outlist <- split(nms, factor(src))

  if (!is.null(package))
    outlist <- outlist[grep(package, names(outlist))]

  return(outlist)
}
