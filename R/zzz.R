

.onLoad <- function(libname, pkgname) {
  
  # presetting DescToolsX options not already defined by the user
  
  # here the same as in DescToolsX to ensure they are defined, even
  # id aurora are loaded as single package
  
}


#' @useDynLib bedrock, .registration = TRUE

#' @importFrom Rcpp sourceCpp
#' 
#' @importFrom stats complete.cases na.omit quantile uniroot runif
#'             relevel filter is.ts na.exclude na.fail na.pass
#'             model.frame plogis qlogis
#' 
#' @importFrom utils combn modifyList tail browseURL find
#'             getAnywhere head help.search str capture.output
#'             readRegistry
#'             
#' @importFrom tools Rd_db file_ext
#' @importFrom readr read_csv read_tsv read_table
#' @importFrom readxl read_excel
#' 
NULL

