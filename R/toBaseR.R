
#' Back to Basics with Tibbles
#' 
#' Sometimes we might wish for the old days be back and want to work with
#' familiar objects. This function helps to convert `tibbles` to
#' `data.frames` as smoothly as possible.
#' 
#' 
#' @param x the object to be converted.
#' @param \dots arguments passed on.
#' @return converted object.
#'
#' @examples
#' # a tibble is rolled back to a plain data.frame
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   tbl <- tibble::as_tibble(head(iris))
#'   class(toBaseR(tbl))
#' }
#'
#' # an object without a method is returned unchanged, with a warning
#' x <- suppressWarnings(toBaseR(1:3))
#' identical(x, 1:3)
#'
#' \donttest{
#' # labelled data from other statistical packages: needs 'haven' and
#' # an internet connection, hence the try()
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   url <- "http://www.stata.com/videos13/data/webclass.dta"
#'   d.webclass <- try(toBaseR(haven::read_dta(url)))
#' }
#' }
#'
#' @family data.coerce
#' @concept type-coercion
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


