

#' Split Strings into Multiple Columns
#'
#' Splits character vectors into multiple columns based on a delimiter.
#' Each element of \code{x} is split using \code{\link[base]{strsplit}},
#' and the resulting parts are expanded into separate columns.
#'
#' All rows are padded to the same number of columns per input element.
#' Missing values are filled with \code{na.form}.
#'
#' @param x A vector, list, or data frame of character elements to be split.
#' Each element (or column) is processed separately.
#' @param split Character string specifying the delimiter for splitting.
#' Passed to \code{\link[base]{strsplit}}.
#' @param fixed Logical; if \code{TRUE}, \code{split} is used as a fixed string.
#' Otherwise, it is treated as a regular expression.
#' @param na.form Character value used to replace missing elements created
#' by unequal split lengths.
#' @param colnames Optional character vector specifying column names for the
#' resulting data frame. Recycled if necessary.
#'
#' @details
#' For each element (or column) in \code{x}, the function:
#' \enumerate{
#'   \item Splits each entry using \code{\link[base]{strsplit}}
#'   \item Determines the maximum number of split parts
#'   \item Pads shorter splits with \code{na.form}
#'   \item Combines results into a matrix via \code{\link[base]{rbind}}
#' }
#'
#' The final result is a data frame where each original element contributes
#' one or more columns depending on the number of splits.
#'
#' An attribute \code{"cols"} is attached, indicating the number of columns
#' generated for each element of \code{x}.
#'
#' @return
#' A data frame containing the split components of \code{x}.
#' Additional attribute:
#' \itemize{
#'   \item \code{cols}: Integer vector with number of columns per input element
#' }
#'
#' @examples
#' x <- c("A B C", "D E", "F")
#' splitToCol(x)
#'
#' # Custom delimiter
#' x <- c("A|B|C", "D|E", "F")
#' splitToCol(x, split = "|")
#'
#' # Multiple columns
#' df <- data.frame(
#'   a = c("x y", "z"),
#'   b = c("1 2 3", "4 5")
#' )
#' splitToCol(df)
#'



#' @export
splitToCol <- function(x, split=" ", fixed = TRUE, na.form="", colnames=NULL){
  
  lst <- lapply(x, function(z)
    strsplit(z, split = split, fixed = fixed))
  
  # we don't want to have values recycled here, but need same number
  # of elements to afterwards be able to use rbind()
  for(i in seq_along(lst)){
    # find the maximal length of the splits in the column
    maxlen <- max(sapply(lst[[i]], length))
    # set all character vectors to same length
    for(j in seq_along(lst[[i]])){
      length(lst[[i]][[j]]) <- maxlen
      # set na.form for missings
      lst[[i]][[j]][is.na(lst[[i]][[j]])] <- na.form
    }
  }
  
  # rbind all the columns
  lst <- lapply(lst, function(z) do.call(rbind, z))
  
  res <- do.call(data.frame, list(lst, stringsAsFactors=FALSE))
  
  if(!is.null(colnames))
    colnames(res) <- rep(colnames, length.out=ncol(res))
  
  # communicate the number of columns found 
  attr(res, "cols") <- sapply(lst, ncol)
  
  return(res)
  
}


