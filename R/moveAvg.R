
#' Moving Average 
#' 
#' Compute a simple moving average (running mean). 
#' 
#' The implementation is using the function \code{filter} to calculate the
#' moving average.
#' 
#' @param x univariate time series.
#' @param order order of moving average.
#' @param align specifies whether result should be centered (default),
#' left-aligned or right-aligned.
#' @param endrule Character string indicating how the values at the beginning
#' and the end of the data should be treated.
#' \describe{
#'   \item{"keep"}{
#'     Keeps the first and last \eqn{k_2}{k2} values at both ends,
#'     where \eqn{k_2}{k2} is the half-bandwidth \code{k2 = k \%/\% 2},
#'     i.e., \code{y[j] = x[j]} for
#'     \eqn{j \in \{1,\ldots,k_2\}} and
#'     \eqn{j \in \{n-k_2+1,\ldots,n\}}.
#'   }
#'   \item{"constant"}{
#'     Fills the ends with the first and last calculated value in the output
#'     array, e.g., \code{out[1:k2] = out[k2+1]}.
#'   }
#'   \item{"NA"}{
#'     The default. Leaves the values as \code{NA},
#'     as returned by \code{\link{filter}}.
#'   }
#' }
#'  
#' @return Returns a vector of the same size and same class as x.
#' 
#' @seealso There's a faster implementation of running mean in the package
#' \pkg{caTools} \code{\link[caTools]{runmean}()} and a slower one in
#' \pkg{forecast} \code{\link[forecast]{ma}()}. There's similar code in
#' \code{\link{midx}()}. 
#' 
#' @keywords univar
#' @examples
#' 
#' moveAvg(AirPassengers, order=5)
#' 
 


#' @family vector.ops
#' @concept vector-manipulation
#' @concept descriptive-statistics
#' @concept time-series
#'
#'
#' @export
moveAvg <- function(x, order,
                         align = c("center","left","right"),
                         endrule = c("NA", "keep", "constant")) {
  
  align   <- match.arg(align)
  endrule <- match.arg(endrule)
  
  n <- length(x)
  
  if (!is.numeric(order) || length(order) != 1 || order < 1 || order %% 1 != 0) {
    stop("'order' must be a positive integer")
  }
  if (order > n) {
    stop("'order' must be <= length(x)")
  }
  
  # --- core: cumulative sum ---
  cs <- c(0, cumsum(x))
  
  # rolling mean (right-aligned base)
  ma <- (cs[(order+1):(n+1)] - cs[1:(n-order+1)]) / order
  
  # pad with NA to full length
  z <- rep(NA_real_, n)
  
  if (align == "right") {
    
    z[order:n] <- ma
    idx <- seq_len(order - 1)
    idx_const <- rep(order, length(idx))
    
  } else if (align == "left") {
    
    z[1:(n-order+1)] <- ma
    idx <- (n-order+2):n
    idx_const <- rep(n-order+1, length(idx))
    
  } else { # center
    
    k2 <- order %/% 2
    
    if (order %% 2 == 1) {
      # odd window
      z[(k2+1):(n-k2)] <- ma
      idx <- c(seq_len(k2), (n-k2+1):n)
      idx_const <- c(rep(k2+1, k2), rep(n-k2, k2))
      
    } else {
      # even window: shift by half
      z[(k2+1):(n-k2)] <- (ma[1:(length(ma)-1)] + ma[2:length(ma)]) / 2
      idx <- c(seq_len(k2), (n-k2+1):n)
      idx_const <- c(rep(k2+1, k2), rep(n-k2, k2))
    }
  }
  
  # --- end rules ---
  if (endrule == "keep") {
    z[idx] <- x[idx]
  } else if (endrule == "constant") {
    z[idx] <- z[idx_const]
  }
  
  # --- preserve class ---
  attributes(z) <- attributes(x)
  
  return(z)
}
