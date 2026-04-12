
#' Get PDF Manual of a Package From CRAN 
#' 
#' PDF versions of the manual are usually not included as vignettes in R
#' packages. Still this format is convenient for reading and doing full text
#' search. \cr This function creates the appropriate link to the pdf file on
#' CRAN and opens the pdf manual in a browser window. 
#' 
#' 
#' @param package name of the package. 
 
#' @examples
#' 
#' \dontrun{
#' pdfManual(DescTools)
#' }
#' 


#' @export
pdfManual <- function(package){
  package <- as.character(substitute(package))
  browseURL(paste("http://cran.r-project.org/web/packages/", package,"/", package, ".pdf", sep = ""))
}

