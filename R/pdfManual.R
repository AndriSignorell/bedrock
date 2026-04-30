
#' Open CRAN PDF manual of a package
#' 
#' PDF versions of the manual are usually not included as vignettes in R
#' packages. Still this format is convenient for reading and doing full text
#' search. \cr This function creates the appropriate link to the pdf file on
#' CRAN and opens the pdf manual in a browser window. 
#' 
#' @param package Package name (symbol or character)


#' @export
pdfManual <- function(package) {
  
  pkg <- if (is.character(package)) {
    package[1]
  } else {
    as.character(substitute(package))
  }
  
  if (!nzchar(pkg))
    stop("Invalid package name")
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning("Package '", pkg, "' is not installed")
  }
  
  url <- sprintf(
    "https://cran.r-project.org/web/packages/%s/%s.pdf",
    pkg, pkg
  )
  
  browseURL(url)
}


