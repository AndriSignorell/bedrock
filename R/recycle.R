
#' Recyle a List of Elements 
#' 
#' This function recycles all supplied elments to the maximal dimension. 
#' 
#' 
#' @param strict defines if number of arguments must be 1 or maxdim.
#' @param \dots a number of vectors of elements. 
#' 
#' @return a list of the supplied elements\cr \code{attr(,"maxdim")} contains
#' the maximal dimension of the recyled list 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{rep}}, \code{\link{replicate}} 
#' 
#' @keywords utilities
#' @examples
#' 
#' recycle(x=1:5, y=1, s=letters[1:2])
#' 
#' z <- recycle(x=letters[1:5], n=2:3, sep=c("-"," "))
#' sapply(1:attr(z, "maxdim"), function(i) paste(rep(z$x[i], times=z$n[i]), 
#'                                         collapse=z$sep[i]))
#' 

#' @export 
recycle <- function(..., strict=FALSE){
  lst <- list(...)
  lens <- lengths(lst)
  # optimization suggestion by moodymudskipper 20.11.2019  
  maxdim <- max(lens) 

  if (strict && !all(lens %in% c(1, maxdim)))
    stop("Arguments must have length 1 or common length.")
  
  # rep_len would not work for Dates
  res <- lapply(lst, rep, length.out=maxdim)
  
  attr(res, "maxdim") <- maxdim
  
  return(res)
}

