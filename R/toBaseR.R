
#' Back to Basics with Tibbles and data.tables
#' 
#' Sometimes we might wish for the old days be back and want to work with
#' familiar objects. This function helps to convert `tibbles` and
#' `data.tables` to plain `data.frames` as smoothly as possible.
#' 
#' For a `data.table` the key (`"sorted"`), secondary indices (`"index"`)
#' and the `".internal.selfref"` pointer are dropped, and a deep copy is 
#' returned, so that later modifications by reference on the original 
#' cannot leak into the result. Columns of class `haven_labelled` are
#' converted to factors in both cases.
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
#' # same for a keyed data.table
#' dt <- data.table::as.data.table(head(iris))
#' data.table::setkey(dt, Species)
#' df <- toBaseR(dt)
#' class(df)
#' attr(df, "sorted")
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
  .toBaseRCols(as.data.frame(x), x)
}


#' @rdname toBaseR
#' @export
toBaseR.data.table <- function(x, ...){
  
  # deep copy first, so that later modifications by reference on x
  # cannot leak into the result
  res <- as.data.frame(data.table::copy(x))
  
  # drop key, secondary indices and the selfref pointer
  for(a in c("sorted", "index", ".internal.selfref"))
    attr(res, a) <- NULL
  
  .toBaseRCols(res, x)
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


# shared column cleanup for data.frame-like containers:
# res is the already coerced data.frame, x the original object
.toBaseRCols <- function(res, x){
  
  # get rid of unimportant SPSS/Stata specific attributes;
  # res[] <- keeps list and matrix columns intact
  res[] <- lapply(res, 
                  removeAttr, 
                  attrNames = c("format.spss", "display_width", "format.stata"))
  
  for(i in which(vapply(x, inherits, logical(1L), what = "haven_labelled"))){
    res[[i]] <- toBaseR(x[[i]])
  }
  
  res
}
