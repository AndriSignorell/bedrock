
#' Append Rownames to a Data Frame
#' 
#' Append rownames to a data.frame as first column.
#' 
#' 
#' @param x a data.frame 
#' @param col_name the name of the new inserted column containing the rownames. 
#' @param after a subscript, after which the values are to be appended.  If
#' missing the rownames will be inserted as first column. 
#' @param remove_rownames logical defining if the existing rownames should be
#' removed.  Default is \code{TRUE}. 
#' 
#' @name appendRowNames
#' @return the object x with appended rownames
#' 
#' @seealso \code{\link[aurora]{appendX}}
#' @examples
#' 
#' (dd <- data.frame(x=1:5, y=6:10, z=LETTERS[1:5], 
#'                   row.names = letters[1:5]))
#' appendRowNames(dd)
#' 



#' @rdname appendRowNames
#' @export
appendRowNames <- function(x, col_name = "rowname", 
                           after = 0, remove_rownames = TRUE) {
  
  if(!(is.data.frame(x) || is.matrix(x)))
    stop("'x' must be a data.frame or matrix")
  
  rn <- rownames(x)
  if(is.null(rn))
    rn <- seq_len(nrow(x))
  
  res <- appendX(x, rn, after = after, names = col_name)
  
  if(remove_rownames)
    rownames(res) <- NULL
  
  return(res)
  
}

