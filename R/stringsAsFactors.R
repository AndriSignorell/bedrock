
#' Convert Character Columns to Factors
#'
#' Strings as factors have recently been downgraded in base R's
#' \code{\link{data.frame}()} function. However,
#' it is still usually a good idea to encode string variables
#' as factors. This function helps to convert some or
#' all columns of a data.frame to factors.
#'
#' @param x the data.frame.
#' @param columns names or indexes of the columns to be converted;
#'  negative values can be used to omit columns.
#' @return the given data.frame including the converted factors.
#' @examples
#' d.dat <- data.frame(char_x = LETTERS[1:5],
#'                     char_y = LETTERS[6:10],
#'                     n = 1:5)
#'
#' # all character columns
#' str(stringsAsFactors(d.dat))
#' # only char_y
#' str(stringsAsFactors(d.dat, columns = "char_y"))
#' # only char_x
#' str(stringsAsFactors(d.dat, columns = "char_x"))
#' # all character columns except the second one ("char_y")
#' str(stringsAsFactors(d.dat, columns = -2))
#'



#' @family data.recode
#' @concept categorization
#' @concept type-coercion
#' @export
stringsAsFactors <- function(x, columns = NULL) {

  if (!is.data.frame(x))
    stop("'x' must be a data.frame.")

  char_cols <- which(vapply(x, is.character, logical(1L)))
  
  if (is.null(columns)) {
    # convert all character columns
    columns <- char_cols
    
  } else if (is.numeric(columns)) {
    if (!all(columns < 0) && !all(columns > 0))
      stop("'columns' must be either all positive or all negative indices.")
    
    if (all(columns < 0)) {
      # exclude the specified columns; only convert remaining character columns
      columns <- setdiff(char_cols, abs(columns))
    }
    # positive indices: use as-is (non-character columns silently skipped below)
    
  } else if (is.character(columns)) {
    bad <- setdiff(columns, names(x))
    if (length(bad) > 0)
      stop("Column(s) not found in x: ", paste(bad, collapse = ", "))
    
    # convert names to indices so intersect() with char_cols works
    columns <- which(names(x) %in% columns)   
      
  } else {
    stop("'columns' must be NULL, a numeric vector, or a character vector of column names.")
  }
  
  # convert only character columns (silently skip numeric/other types)
  columns <- intersect(columns, char_cols)
  x[columns] <- lapply(x[columns], as.factor)
  
  return(x)
}


