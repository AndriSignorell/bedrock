
#' Check a Vector For Being Zero
#' 
#' Test if x is zero. This is done by checking if the numeric value is
#' below the machine tolerance.
#' 
#' @name isZero
#' @aliases isZero
#' 
#' @param x a (non-empty) numeric vector of data values. 
#' @param tol tolerance to be used 
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}. 
#' 
#' @return logical vector of the same dimension as x.
#' @author R-Core, Andri Signorell <andri@@signorell.net>, Thomas W. Yee 
#' @seealso \code{\link{is.integer}} 
#' @keywords logic
#' @examples
#' # ... These are people who live in ignorance of the Floating Point Gods.
#' # These pagans expect ... (Burns, 2011)" the following to be TRUE:
#' (.1 - .3 / 3) == 0
#' 
#' # they might be helped by
#' isZero(.1 - .3 / 3)
#' 

#' @rdname isZero
#' @export
isZero <-function(x, tol = sqrt(.Machine$double.eps), na.rm=FALSE) {
  
  # Define check if a numeric is 0
  
  if (na.rm)
    x <- x[!is.na(x)]
  
  if(is.numeric(x))
    abs(x) < tol
  else
    FALSE
  
}
