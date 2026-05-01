
#' Compactly Display the Structure of any R Object 
#' 
#' Basically a wrapper for \code{\link{str}()}, extended with an enumeration
#' for the variables of a data.frame. 
#' 
#' 
#' @param x any \code{R} object about which you want to have some information.
#' @param \dots dots are passed to \code{\link{str}}. 
#' 
#' @seealso \code{\link{str}} 
#' @keywords utilities
#' @examples
#' 
#' strX(d.pizza)


#' @family pkg.introspection
#' @concept package-utilities
#' @concept data-inspection
#'
#'
#' @export
strX <- function(x, ...) {
  
  if (is.data.frame(x) || is.list(x)) {
    
    args <- list(...)
    if (is.null(args$strict.width)) args$strict.width <- "cut"
    
    out <- capture.output(do.call(str, c(list(object = x), args)))
    
    idx <- grep("^ \\$", out)
    
    out[idx] <- sub("^ \\$", 
                    paste0(" ", seq_along(idx), " $"), 
                    out[idx])
    
    res <- out
    
  } else {
    res <- capture.output(str(x, ...))
  }
  
  cat(res, sep = "\n")
  invisible(res)
}

