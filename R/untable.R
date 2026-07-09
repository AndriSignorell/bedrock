
#' Recover Original Data From Contingency Table
#' 
#' Recreates the data.frame out of a contingency table x.
#' 
#' For x being a vector this reduces to \code{rep(..., n)} with n as vector
#' (which is not supported by \code{rep()}). \code{NA}s in the table will be
#' treated as 0 without raising an error.
#' 
#' @name untable
#' @aliases untable untable.default untable.data.frame
#' @param x a numeric vector, a matrix, a table or a data.frame. If x is a
#' vector, a matrix or a table it is interpreted as frequencies which are to be
#' inflated to the original list. \cr If x is a data.frame it is interpreted as
#' a table in frequency form (containing one or more factors and a frequency
#' variable). 
#' @param dimnames the dimension names of x to be used for expanding. Can be
#' used to expand a weight vector to its original values. If set to \code{NULL}
#' (default) the dimnames of x will be used.
#' @param type defines the data type generated. This allows to directly define
#' factors or ordered factors, but also numeric values. See examples.
#' @param rownames A names vector for the rownames of the resulting data.frame.
#' If set to \code{NULL} (default) the names will be defined according to the
#' table's dimnames.
#' @param colnames A names vector for the colnames of the resulting data.frame.
#' If set to \code{NULL} (default) the names will be defined according to the
#' table's dimnames.
#' @param freq character, the name of the frequency variable in case x is a
#' data.frame.
#' @param \dots further arguments passed to or from functions (not used here).
#' 
#' @return a data.frame with the detailed data (even if x was a 1-dimensional
#' table)
#' 

#' @seealso \code{\link{expand.grid}}, \code{\link{rep}}, \code{\link{gl}},
#' \code{\link{xtabs}}
#' @keywords manip
#' @examples
#' 
#' d.titanic <- untable(Titanic)
#' str(d.titanic)
#' 
#' # ... not the same as:
#' data.frame(Titanic)
#' 
#' 
#' tab <- table(set1=sample(letters[1:5], size=40, replace=TRUE), 
#'              set2=sample(letters[11:15], size=40, replace=TRUE))
#' untable(tab)
#' 
#' 
#' # return a numeric vector by setting type and coerce to a vector by [,]
#' untable(c(6,2,2), type="as.numeric")[,]
#' 
#' 
#' # how to produce the original list based on frequencies, given as a data.frame
#' d.freq <- data.frame(xtabs(Freq ~ Sex + Survived, data=Titanic))
#' 
#' # a data list with each individual
#' d.data <- untable( xtabs(c(1364, 126, 367, 344) ~ ., 
#'              expand.grid(levels(d.freq$Sex),levels(d.freq$Survived)))) 
#' head(d.data)
#' 
#' # expand a weights vector
#' untable(c(1,4,5), dimnames=list(c("Zurich","Berlin","London")))
#' 
#' # and the same with a numeric vector 
#' untable(c(1,4,5), dimnames=list(c(5,10,15)), type="as.numeric")[,]
#' # ... which again is nothing else than
#' rep(times=c(1,4,5), x=c(5,10,15))
#' 
#' # the data.frame interface
#' d.freq <- data.frame(f1=c("A","A","B","B"), f2=c("C","D","C","D"), Freq=c(1,2,3,4))
#' untable(d.freq)
#' 

#' @rdname untable

#' @family math.utils  
#' @concept frequency-table  
#' @concept reshape
#'
#'
#' @export
untable <- function(x, ...){
  UseMethod("untable")
}


#' @rdname untable
#' @export
untable.data.frame <- function(x, freq = "Freq", rownames = NULL, ...){
  
  if(all(is.na(match(freq, names(x)))))
    stop(gettextf("Frequency column %s does not exist!", freq))
  
  res <- x[untable(x[,freq], type="as.numeric")[,], -match(freq, names(x)), drop=FALSE]
  rownames(res) <- rownames
  
  return(res)
}



#' @rdname untable
#' @export
untable.default <- function(x, dimnames=NULL, type = NULL, rownames = NULL, colnames = NULL, ...) {
  
  # recreates the data.frame out of a contingency table
  # check fo NAs
  if(anyNA(x))
    warning("Provided object to untable contains NAs.")
  
  # coerce to table, such as also be able to handle vectors
  x <- as.table(naReplace(x, 0))
  
  if(!is.null(dimnames)) dimnames(x) <- dimnames
  if(is.null(dimnames) && identical(type, "as.numeric")) dimnames(x) <- list(seq_along(x))
  
  # name unnamed dimensions Var1, Var2, ...
  # (the old loop modified a copy via names(dimnames(x)[i]) <- and was a no-op)
  dn <- dimnames(x)
  if (is.null(names(dn)))
    names(dn) <- rep("", length(dn))
  empty <- !nzchar(names(dn))
  names(dn)[empty] <- paste0("Var", which(empty))
  dimnames(x) <- dn
  
  res <- expand.grid(dimnames(x))[rep(1:prod(dim(x)), as.vector(x)), , drop = FALSE]
  rownames(res) <- NULL
  colnames(res) <- names(dimnames(x))
  
  # return ordered factors, if wanted...
  if(is.null(type)) type <- "as.factor"
  # recycle type:
  if(length(type) < ncol(res)) type <- rep(type, length.out=ncol(res))
  
  for(i in 1:ncol(res)){
    if(type[i]=="as.numeric"){
      res[,i] <- as.numeric(as.character(res[,i]))
    } else {
      res[,i] <- match.fun(type[i])(res[,i])
    }
  }
  
  # overwrite the dimnames, if requested
  if(!is.null(rownames)) rownames(res) <- rownames
  if(!is.null(colnames)) colnames(res) <- colnames
  
  return(res)
  
}


