
#' List all arguments in a function.
#' 
#' @param fun the name of the function
#' @param package the name of the package which contains the function
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
funArgs <- function(fun, package = NULL, sort = FALSE) {
  
  # Funktion auflösen
  if (is.character(fun)) {
    
    if (!is.null(package)) {
      
      fun <- getExportedValue(package, fun)
      
    } else {
      
      fun <- get(fun, mode = "function")
    }
  }
  
  fmls <- formals(fun)
  
  if (is.null(fmls))
    return(NULL)
  
  out <- data.frame(
    name = names(fmls),
    value = vapply(
      fmls,
      function(x)
        paste(deparse(x), collapse = " "),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  
  if (sort) {
    
    dots <- out$name == "..."
    
    out <- rbind(
      out[order(out$name[!dots]), ],
      out[dots, ]
    )
  }
  
  attr(out, "string") <-
    paste(sprintf("%s = %s", out$name, out$value),
          collapse = ", ")
  
  class(out) <- c("FunArgs", "data.frame")
  
  out
}
