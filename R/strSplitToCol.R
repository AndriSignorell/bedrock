
#' Split Strings into Multiple Columns
#'
#' Splits character vectors into multiple columns based on a delimiter.
#' Each element of \code{x} is split using \code{\link[base]{strsplit}},
#' and the resulting parts are expanded into separate columns.
#'
#' All rows are padded to the same number of columns per input element.
#' Missing values are filled with \code{naForm}.
#'
#' @param x A character vector or a data frame of character columns to be split.
#'   Each element (or column) is processed separately.
#' @param split Character string specifying the delimiter for splitting.
#'   Passed to \code{\link[base]{strsplit}}.
#' @param fixed Logical; if \code{TRUE}, \code{split} is used as a fixed string.
#'   Otherwise, it is treated as a regular expression.
#' @param naForm Character value used to replace missing elements created
#'   by unequal split lengths.
#' @param colNames Optional character vector specifying column names for the
#'   resulting data frame. Recycled if necessary.
#'
#' @details
#' For each element (or column) in \code{x}, the function:
#' \enumerate{
#'   \item Splits each entry using \code{\link[base]{strsplit}}
#'   \item Determines the maximum number of split parts
#'   \item Pads shorter splits with \code{naForm}
#'   \item Combines results into a matrix via \code{\link[base]{rbind}}
#' }
#'
#' The final result is a data frame where each original element or column
#' contributes one or more columns depending on the number of splits.
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
#' strSplitToCol(x)
#'
#' # Custom delimiter
#' x <- c("A|B|C", "D|E", "F")
#' strSplitToCol(x, split = "|")
#'
#' # Multiple columns
#' df <- data.frame(
#'   a = c("x y", "z"),
#'   b = c("1 2 3", "4 5"),
#'   stringsAsFactors = FALSE
#' )
#' strSplitToCol(df)
#'



#' @family string.utilities  
#' @concept string-manipulation  
#' @concept reshape
#'
#'
#' @export
strSplitToCol <- function(x, split = " ", fixed = TRUE, 
                          naForm = "", colNames = NULL) {
  
  # input validation
  if (!is.data.frame(x) && !is.character(x))
    stop("'x' must be a character vector or a data.frame of character columns.")
  
  if (is.data.frame(x) && !all(vapply(x, is.character, logical(1))))
    stop("All columns in 'x' must be character.")

  if (NROW(x) == 0L)
    return(structure(data.frame(), cols = integer(0)))
  
  # normalise input: data.frame -> list of columns, vector -> list of one column
  lst <- if (is.data.frame(x)) {
    lapply(x, function(col) strsplit(col, split = split, fixed = fixed))
  } else {
    list(strsplit(x, split = split, fixed = fixed))
  }
  
  # pad each split to the same length within its column
  lst <- lapply(lst, function(splits) {
    maxlen <- max(vapply(splits, length, integer(1)))
    lapply(splits, function(s) {
      if (length(s) < maxlen)
        s <- c(s, rep(naForm, maxlen - length(s)))
      s
    })
  })
  
  # collapse each column's list of vectors into a matrix;
  # clear dimnames to prevent "[,1]"-style artefacts in the data.frame
  lst <- lapply(lst, function(splits) {
    mat <- do.call(rbind, splits)
    colnames(mat) <- NULL
    mat
  })
  
  res <- do.call(data.frame, c(lst, list(stringsAsFactors = FALSE)))
  
  # optionally rename columns (colNames recycled to fit)
  if (!is.null(colNames))
    colnames(res) <- rep(colNames, length.out = ncol(res))
  
  # communicate the number of columns generated per input element
  attr(res, "cols") <- vapply(lst, ncol, integer(1))
  
  return(res)
  
}

