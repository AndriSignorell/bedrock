
#' Back to Basics with Tibbles
#' 
#' Sometimes we might wish for the old days be back and want to work with
#' familiar objects. This function helps to convert \code{tibbles} to
#' \code{data.frames} as smoothly as possible.
#' 
#' 
#' @param x the object to be converted
#' @param \dots arguments passed on
#' @return converted object

#' @examples
#' 
#' \dontrun{
#' # read a Stata file
#' url <- "http://www.stata.com/videos13/data/webclass.dta"
#' d.webclass <- toBaseR(haven::read_dta(url))
#' 
#' # read a SPSS file
#' url <- "https://stats.idre.ucla.edu/wp-content/uploads/2020/10/missing.sav"
#' d.miss <- toBaseR(haven::read_sav(url))
#' }



#' @family data.coerce
#' @concept type-coercion
#' @concept tibble
#' @export
toBaseR <- function(x, ...){
  UseMethod("toBaseR")
}


#' @rdname toBaseR
#' @export
toBaseR.tbl_df <- function(x, ...){
  # rollback a tibble to data.frame, with usual factors etc.
  res <- as.data.frame(x)
  
  # get rid of unimportant SPSS specific attributes
  res <- as.data.frame(
    lapply(res, 
           removeAttr, 
           attrNames = c("format.spss", "display_width", "format.stata")),
    check.names = FALSE)
  
  for(i in which(vapply(x, inherits, logical(1L), what = "haven_labelled"))){
    res[[i]] <- toBaseR(x[[i]])
  }
  
  return(res)
}


#' @rdname toBaseR
#' @export
toBaseR.haven_labelled <- function(x, ...) {
  
  if (requireNamespace("haven", quietly = TRUE)) {
    haven::as_factor(x, ...)
    
  } else {
    warning("Package 'haven' not installed: returning original object")
    x
  }
}


#' @rdname toBaseR
#' @export
toBaseR.default <- function(x, ...){
  
  # return the object unchanged: destroying data in a pipeline by
  # returning NULL would be worse than doing nothing
  warning(
    gettextf(
      'Not implemented for class(es) "%s", returning object unchanged',
      paste(class(x), collapse = ", ")
    )
  )
  
  invisible(x)
  
}


