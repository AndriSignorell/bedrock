
#' List Objects, Functions Or Data in a Package 
#' 
#' List all the objects, functions or data in a package. 
#' 
#' This is just a wrapper for \code{\link{ls}}, \code{\link{ls.str}} and
#' \code{\link{lsf.str}} with the appropriate arguments (as I always forgot how
#' to do the trick). \code{objList()} lists all objects, \code{funList()} just the
#' functions in a package. 
#' 
#' @param package the name of the package 
#' @param exported logical (default \code{TRUE}) should only exported functions be listed?
#' 
 
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}} 
#' 
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole. 
#' 
#' @family topic.systemTools
#' @concept Infrastructure
#' @concept Programming Utilities 
#' 
#' @examples
#' 
#' funList("bedrock")
#' 

#' @export
funList <- function(package, exported = TRUE) {
  
  ns <- getNamespace(package)
  
  objs <- if (exported) {
    getNamespaceExports(package)
  } else {
    ls(ns, all.names = TRUE)
  }
  
  objs[sapply(objs, function(x) is.function(get(x, envir = ns)))]
  
  # less robust: 
  # as.vector(unclass(lsf.str(pos = gettextf("package:%s", package) )))

}
