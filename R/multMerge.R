
#' Merge Multiple Data Frames 
#' 
#' Merge multiple data frames by row names, or do other versions of database
#' join operations. 
#' 
#' 
#' @param \dots data frames to be coerced to one. 
#' @param all.x logical; if \code{TRUE}, then extra rows will be added to the
#' output, one for each row in x that has no matching row in y. These rows will
#' have \code{NA}s in those columns that are usually filled with values from y.
#' The default is \code{TRUE}, so that non-matching rows are kept and
#' padded with \code{NA}s (full outer join). 
#' @param all.y logical; analogous to \code{all.x}. 
#' @param by column used for merging, if this is not defined rownames will be
#' used by default. The column must be included in all the provided data
#' frames and its values must be unique within each data frame. Note that
#' the restored key column is of type character.
#' @return A data frame. The rows are sorted according to the appearance of
#' previously unobserved rownames. So the rownames appearing in the first data
#' frame are first, then the rownames in the second data frame, which have no
#' corespondence in the first data frame and so on. The columns are the
#' remaining columns in x1 and then those in x2 and then those in x3. The
#' result has the row names resulting from the merge. 
#' 
#' @seealso \code{\link{merge}} 
#' 
#' @examples
#' 
#' x1 <- setNamesX(data.frame(v=letters[1:6], w=1:6), 
#'                 rownames=c("A", "B", "C", "D", "E", "F"))
#' x2 <- setNamesX(data.frame(v=letters[1:3], ww=11:13), 
#'                 rownames=c("B", "C", "D"))
#' x3 <- setNamesX(data.frame(v=letters[12:16], wwww=22:26), 
#'                 rownames=c("A", "C", "E", "G", "J"))
#' 
#' # default is "merge by rownames" 
#' multMerge(x1, x2, x3)
#' # ... which does not really make sense here
#' 
#' # merge by column v
#' multMerge(x1, x2, x3, by="v")
#'
#' @family data.manipulation
#' @concept merge
#' @export
multMerge <- function(..., all.x = TRUE, all.y = TRUE, by = NULL) {
  
  lst <- list(...)
  
  # if just one object, there's nothing to merge
  if (length(lst) == 1) return(lst[[1]])
  
  if (!is.null(by)) {
    # merge column is given and must exist in all the data.frames;
    # we overwrite the row.names and remove the merge column, hence
    # the values must be unique within each data frame
    for (i in seq_along(lst)) {
      if (!by %in% colnames(lst[[i]]))
        stop(sprintf("'by' column '%s' not found in data frame %d", by, i))
      if (anyDuplicated(lst[[i]][[by]]))
        stop(sprintf("'by' values must be unique within each data frame (data frame %d)", i))
      rownames(lst[[i]]) <- lst[[i]][[by]]
      lst[[i]][by] <- NULL
    }
  }
  
  # the columnnames must be unique within the resulting data.frame
  unames <- splitAt(make.unique(unlist(lapply(lst, colnames)), sep = "."),
                    cumsum(sapply(head(lst, -1), ncol)) + 1)
  
  for (i in seq_along(unames))
    colnames(lst[[i]]) <- unames[[i]]
  
  # merge by an explicit internal key column (named so that it cannot
  # collide with user columns) to prevent .x/.y splitting across rounds
  res <- Reduce(
    function(y, z)
      merge(y, z,
            by     = ".mmKey",
            all.x  = all.x,
            all.y  = all.y,
            sort   = FALSE),
    lapply(lst, function(x)
      data.frame(.mmKey = row.names(x), x, stringsAsFactors = FALSE)
    ))
  rownames(res) <- res$.mmKey
  res$.mmKey <- NULL
  
  # order rows: rownames from left to right, new ones appended as they appear
  seq_ord <- function(xlst) {
    jj <- character(0)
    for (i in seq_along(xlst))
      jj <- c(jj, setdiff(xlst[[i]], jj))
    return(jj)
  }
  
  ord <- seq_ord(lapply(lst, rownames))
  ord <- intersect(ord, rownames(res))
  
  res <- res[ord, , drop = FALSE]
  
  if (!is.null(by)) {
    # restore key column and remove rownames
    res <- data.frame(row.names(res), res)
    colnames(res)[1] <- by
    rownames(res) <- NULL
  }
  
  return(res)
  
}

