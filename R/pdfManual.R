
#' Open CRAN PDF Manual of a Package
#'
#' PDF versions of the manual are usually not included as vignettes in R
#' packages. Still this format is convenient for reading and doing full text
#' search. \cr This function creates the appropriate link to the pdf file on
#' CRAN and opens the pdf manual in a browser window.
#'
#' A warning (not an error) is issued if the package is not installed
#' locally, as the manual may well exist on CRAN anyway.
#'
#' @param package package name (symbol or character)
#'
#' @return The URL of the PDF manual, invisibly. Called for its side effect
#'   of opening the browser.
#'
#' @seealso \code{\link{browseURL}}
#'
#' @examples
#' \dontrun{
#' pdfManual(DescToolsX)
#' pdfManual("bedrock")
#' }
#'
#' @family file.io
#' @concept file-io
#' @concept introspection
#' @export
pdfManual <- function(package) {

  # evaluate if possible (character constant or variable), otherwise take
  # the unevaluated symbol -- is.character(package) alone would force the
  # promise and crash for pdfManual(SomePackage)
  pkg <- tryCatch(package, error = function(e) NULL)

  pkg <- if (is.character(pkg)) {
    pkg[1]
  } else {
    as.character(substitute(package))
  }

  if (length(pkg) != 1L || !nzchar(pkg))
    stop("Invalid package name")

  # find.package instead of requireNamespace: checking should not load
  # the package as a side effect
  if (!length(find.package(pkg, quiet = TRUE))) {
    warning("Package '", pkg, "' is not installed")
  }

  url <- sprintf(
    "https://cran.r-project.org/web/packages/%s/%s.pdf",
    pkg, pkg
  )

  browseURL(url)

  invisible(url)
}
