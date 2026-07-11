
#' Moving Average
#'
#' Computes a simple moving average (running mean) of a numeric vector
#' or time series.
#'
#' @details
#' The core computation uses cumulative sums for O(n) efficiency:
#' \deqn{
#'   \bar x_i = \frac{1}{k}\sum_{j} x_{i+j}
#' }
#' where the summation range depends on \code{align}.
#'
#' \strong{Even-order windows and center alignment}
#'
#' For even \code{order}, centering is ambiguous.  This implementation
#' averages the two adjacent right-aligned windows of width \code{order},
#' which is the convention used by \code{forecast::ma()}.
#'
#' \strong{Boundary handling (\code{endrule = "trim"})}
#'
#' At the boundaries the window is contracted to include only the
#' available observations.  For center alignment with even \code{order},
#' the boundary window width at position \eqn{i} is
#' \eqn{i + \lfloor order/2 \rfloor}.
#'
#' \strong{Missing values}
#'
#' \code{NA} in \code{x} propagates through \code{cumsum()} and will
#' produce \code{NA} in all moving-average values whose window contains
#' that observation.  There is no \code{na.rm} option; pre-filter with
#' \code{x[!is.na(x)]} if needed (note this changes index positions).
#'
#' @param x       a univariate numeric vector or \code{ts} object.
#'   Matrices and multi-column objects are not supported.
#' @param order   a single positive integer giving the window width.
#'   Must satisfy \code{1 <= order <= length(x)}.
#' @param align   a character string controlling how the window is
#'   positioned relative to each output value:
#'   \describe{
#'     \item{\code{"center"}}{Default.  The window is centred on the
#'       current observation.  For odd \code{order} the window is
#'       symmetric; for even \code{order} see Details.}
#'     \item{\code{"left"}}{The window starts at the current observation
#'       and extends to the right.}
#'     \item{\code{"right"}}{The window ends at the current observation
#'       and extends to the left.}
#'   }
#' @param endrule a character string indicating how boundary values
#'   (where a full window is unavailable) are handled:
#'   \describe{
#'     \item{\code{"NA"}}{Default.  Boundary values are left as
#'       \code{NA}.}
#'     \item{\code{"keep"}}{Boundary values are taken from the original
#'       \code{x}.}
#'     \item{\code{"constant"}}{Boundary values are filled with the
#'       nearest computed moving-average value.}
#'     \item{\code{"trim"}}{Boundary values are computed from all
#'       available observations in a progressively smaller window.}
#'   }
#'
#' @return A vector of the same length and class as \code{x}, with
#'   \code{NA} at boundary positions unless \code{endrule} specifies
#'   otherwise.
#'
#' @examples
#' moveAvg(AirPassengers, order = 5)
#' moveAvg(AirPassengers, order = 5, endrule = "trim")
#' moveAvg(AirPassengers, order = 4, align = "right", endrule = "constant")
#'
#' @seealso [zoo::rollmean()], [forecast::ma()], [stats::runmed()]
#'
#' @family vector.window
#' @concept moving-window
#' @concept smoother
#' @export
moveAvg <- function(x,
                    order,
                    align   = c("center", "left", "right"),
                    endrule = c("NA", "keep", "constant", "trim")) {
  
  # --- input checks --------------------------------------------------
  if (!is.null(dim(x)))
    stop("'x' must be a univariate vector or time series, not a matrix.")
  if (!is.numeric(x))
    stop("'x' must be numeric.")
  
  if (!is.numeric(order) || length(order) != 1L ||
      is.na(order) || order < 1L || order %% 1 != 0)
    stop("'order' must be a single positive integer.")
  
  n <- length(x)
  order <- as.integer(order)
  
  if (order > n)
    stop("'order' must be <= length(x) (", n, ").")
  
  # order == 1: moving average is the series itself
  if (order == 1L)
    return(x)
  
  align   <- match.arg(align)
  endrule <- match.arg(endrule)
  
  # --- cumulative-sum core -------------------------------------------
  # cs[i+1] - cs[j] = sum(x[j:i]), so a window of width `order` ending
  # at position i is cs[i+1] - cs[i - order + 1].
  # `ma` contains right-aligned running means for positions order..n.
  cs <- c(0, cumsum(x))
  ma <- (cs[(order + 1L):(n + 1L)] - cs[1L:(n - order + 1L)]) / order
  
  # --- align running means into output --------------------------------
  z <- rep(NA_real_, n)
  
  if (align == "right") {
    
    z[order:n] <- ma
    bnd       <- seq_len(order - 1L)
    bndConst <- rep(order, length(bnd))
    
  } else if (align == "left") {
    
    z[1:(n - order + 1L)] <- ma
    bnd       <- (n - order + 2L):n
    bndConst <- rep(n - order + 1L, length(bnd))
    
  } else {
    # center alignment
    k2 <- order %/% 2L
    
    if (order %% 2L == 1L) {
      # odd window: exactly symmetric
      z[(k2 + 1L):(n - k2)] <- ma
    } else if (n > order) {
      # even window: average the two adjacent right-aligned windows
      z[(k2 + 1L):(n - k2)] <-
        (ma[seq_len(length(ma) - 1L)] + ma[seq_len(length(ma) - 1L) + 1L]) / 2
    }
    # even window with order == n: no interior points, all cells are
    # boundary cells and are handled by the endrule below
    
    bnd       <- c(seq_len(k2), (n - k2 + 1L):n)
    bndConst <- c(rep(k2 + 1L, k2), rep(n - k2, k2))
  }
  
  # --- end rules ------------------------------------------------------
  if (endrule == "keep") {
    
    z[bnd] <- x[bnd]
    
  } else if (endrule == "constant") {
    
    z[bnd] <- z[bndConst]
    
  } else if (endrule == "trim") {
    
    if (align == "right") {
      for (i in seq_len(order - 1L))
        z[i] <- mean(x[seq_len(i)])
      
    } else if (align == "left") {
      for (i in seq_len(order - 1L))
        z[n - i + 1L] <- mean(x[(n - i + 1L):n])
      
    } else {
      k2 <- order %/% 2L
      for (i in seq_len(k2)) {
        z[i]          <- mean(x[seq_len(i + k2)])
        z[n - i + 1L] <- mean(x[(n - k2 - i + 1L):n])
      }
    }
  }
  # endrule == "NA": boundaries stay NA — nothing to do
  
  # --- preserve ts attributes (tsp, class) ----------------------------
  mostattributes(z) <- attributes(x)
  z
}

