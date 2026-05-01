
#' List Calls Used in Function
#' 
#' For screening purposes it can be useful to get a list of all function calls
#' our function may depend on. \code{funCalls()} parses the function 
#' source and return all found function calls grouped by their package. 
#' 
#' 
#' @name funCalls
#' @param name the name of the function
#' @param package the name of the package 
#' @param sort logical (default \code{FALSE}) should the arguments be alphabetically sorted?
#' 
#' @note
#' Based on code by Nicholas Cooper.
#' 
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}} 
#' 
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole. 
#' 
#' @examples
#' 
#' funCalls("combN", package="bedrock")
#' 


#' @family pkg.introspection
#' @concept package-utilities
#' @concept data-inspection
#'
#'
#' @export
funCalls <- function (name, package=NULL, sort=FALSE) {
  
  tmp <- utils::getParseData(parse(text = getAnywhere(name), keep.source = TRUE))
  nms <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")]
  funs <- unique(if (sort) {
    sort(nms)
  } else {
    nms
  })
  
  src <- paste(as.vector(sapply(funs, find)))
  outlist <- tapply(funs, factor(src), c)
  
  if(!is.null(package))
    outlist <- outlist[grep(package, names(outlist))]
  return(outlist)
}

