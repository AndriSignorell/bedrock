
#' List All Arguments in a Function
#' 
#' List all arguments in a function.
#' 
#' @param name the name of the function
#' @param sort logical (default \code{FALSE}) should the arguments be alphabetically sorted?
#' 
 
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}} 
#' 
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole. 
#' 
#' @examples
#' 
#' funArgs("combN")
#' funArgs("combN")$value
#' 
#' cat(attr(funArgs("combN"), "string"))
#' 


#' @family pkg.introspection
#' @concept package-utilities
#' @concept data-inspection
#'
#'
#' @export
funArgs <- function(name, sort=FALSE) {

  # got that somewhere, but don't know from where...
  
  if(is.function(name)) name <- as.character(substitute(name))
  a <- formals(get(name, pos=1))
  if(is.null(a))
    return(NULL)
  arg.labels <- names(a)
  arg.values <- as.character(a)
  char <- sapply(a, is.character)
  arg.values[char] <- paste("\"", arg.values[char], "\"", sep="")
  
  if(sort)
  {
    ord <- order(arg.labels)
    if(any(arg.labels == "..."))
      ord <- c(ord[-which(arg.labels[ord]=="...")],
               which(arg.labels=="..."))
    arg.labels <- arg.labels[ord]
    arg.values <- arg.values[ord]
  }
  
  output <- data.frame(name=arg.labels, value=I(arg.values))
  attr(output, "string") <- paste(gettextf("%s = %s", arg.labels, arg.values),
                                   collapse = ", ")
  
  class(output) <- c("FunArgs", "data.frame")
  
  return(output)
  
}

