
#' List Functions in a Package
#'
#' List all the functions in a package.
#'
#' This is just a wrapper for the namespace inspection functions (as I always
#' forgot how to do the trick). By default only the exported functions are
#' returned; with \code{exported = FALSE} all functions defined in the
#' package namespace are listed, including internal ones.
#'
#' @param package the name of the package
#' @param exported logical (default \code{TRUE}) should only exported
#'   functions be listed?
#'
#' @return A sorted character vector with the function names.
#'
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}},
#' \code{\link{getNamespaceExports}}
#'
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole.
#'
#' @examples
#'
#' funList("bedrock")
#'
#' @family pkg.introspection
#' @concept introspection
#' @export
funList <- function(package, exported = TRUE) {

  ns <- getNamespace(package)

  objs <- if (exported) {
    getNamespaceExports(package)
  } else {
    ls(ns, all.names = TRUE)
  }

  sort(objs[vapply(
    objs,
    function(x) is.function(get(x, envir = ns)),
    logical(1L)
  )])

}
