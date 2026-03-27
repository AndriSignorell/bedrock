
#' Pairwise Calculations
#' 
#' Implements a logic to run pairwise calculations on the columns of a
#' data.frame or a matrix. 
#' 
#' This code is based on the logic of \code{cor()} and extended for asymmetric
#' functions. 
#' 
#' @param x a list, a data.frame or a matrix with columns to be processed
#' pairwise. 
#' @param FUN a function to be calculated. It is assumed, that the first 2
#' arguments denominate x and y. 
#' @param \dots the dots are passed to FUN. 
#' @param symmetric logical. Does the function yield the same result for FUN(x,
#' y) and FUN(y, x)? \cr If \code{TRUE} just the lower triangular matrix is
#' calculated and transposed. Default is FALSE. 
#' 
#' @return a matrix with the results of FUN. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{outer}}, \code{\link{combPairs}},
#' \code{\link{pairwise.table}}
#' @keywords manip
#' @examples
#' 
#' # build a dataset
#' set.seed(1)
#' d.sub <- transform(
#'   data.frame(
#'     X1 = rnorm(n <- 300), 
#'     X3 = rnorm(n)),
#'   X2 = 0.8*X1 + rnorm(n),
#'   X4 = 0.5*X3 + rnorm(n)
#'   )
#' 
#' pairApply(d.sub, FUN = cor, method="spearman")
#' 
#' # user defined functions are ok as well
#' pairApply(d.sub, 
#'   FUN = function(x,y) 
#'     wilcox.test(as.numeric(x), as.numeric(y))$p.value, symmetric=TRUE)
#' 


#' @export
pairApply <- function(x, FUN = NULL, ..., symmetric = FALSE){
  
  # Alternative names: pairApply, PwApply, pwapply, papply, ...
  
  if(is.function(FUN)) {
    # if FUN is a function, then save it under new name and
    # overwrite function name in FUN, which has to be character
    fct <- FUN
    FUN <- "fct"
  }
  
  if(is.matrix(x)) x <- as.data.frame(x)
  x <- as.list(x)
  
  ix <- 1:length(x)
  # pairwise logic from pairwise.table
  pp <- outer(ix, ix, function(ivec, jvec) sapply(seq_along(ivec),
                                                  function(k) {
                                                    i <- ivec[[k]]
                                                    j <- jvec[[k]]
                                                    if (i >= j)
                                                      eval(parse(text = gettextf("%s(x[[i]], x[[j]], ...)", FUN)))
                                                    else NA
                                                  }))
  # why did we need that? in any case it's wrong, if no symmetric calcs are done
  # diag(pp) <- 1
  if(symmetric){
    pp[upper.tri(pp)] <- t(pp)[upper.tri(t(pp))]
  } else {
    pp.upr <- outer(ix, ix, function(ivec, jvec) sapply(seq_along(ivec),
                                                        function(k) {
                                                          i <- ivec[[k]]
                                                          j <- jvec[[k]]
                                                          if (i >= j)
                                                            eval(parse(text = gettextf("%s(x[[j]], x[[i]], ...)", FUN)))
                                                          else NA
                                                        }))
    pp[upper.tri(pp)] <- t(pp.upr)[upper.tri(pp.upr)]
    
  }
  
  dimnames(pp) <- list(names(x),names(x))
  
  return(pp)
}

