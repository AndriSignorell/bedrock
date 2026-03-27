

.onLoad <- function(libname, pkgname) {
  
  # presetting DescToolsX options not already defined by the user
  
  # here the same as in DescToolsX to ensure they are defined, even
  # id aurora are loaded as single package
  
}


#' @useDynLib bedrock, .registration = TRUE

#' @importFrom Rcpp sourceCpp
#' 
#' @importFrom stats complete.cases na.omit quantile uniroot runif
#' 
#' @importFrom utils combn modifyList tail browseURL find
#'             getAnywhere head help.search str

NULL
