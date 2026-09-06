
#' Round to a Multiple 
#' 
#' Rounds the values of a numeric vector to the nearest multiple of a given
#' step width. Where [round()] is tied to multiples of a power of ten,
#' `roundTo()` accepts an arbitrary step, so that prices can be rounded to the
#' nearest 5 cents, durations to the nearest quarter of an hour or axis limits
#' to the nearest 250. The direction of the rounding is controlled by `FUN`,
#' which allows rounding to the nearest, upwards, downwards or towards zero
#' with the same interface.
#' 
#' There are several functions in base R to convert to integers. [round()]
#' rounds to the nearest integer or to any number of digits. Using a negative
#' number of digits rounds to a power of ten, so that `round(x, -3)` rounds to
#' thousands. Each of [trunc()], [floor()] and [ceiling()] rounds in a fixed
#' direction, towards zero, down and up respectively. [round()] is documented
#' to round half to even, so `round(2.5)` is `2`.
#' 
#' `roundTo()` evaluates `FUN(x / multiple) * multiple`. With the default
#' `FUN = round` a value lying exactly halfway between two multiples is
#' therefore rounded to the one with the even quotient: `roundTo(1, 2)` is `0`
#' and `roundTo(3, 2)` is `4`. Setting `FUN = ceiling` always rounds up,
#' `FUN = floor` always rounds down and `FUN = trunc` always towards zero (see
#' the examples for a comparison).
#' 
#' Ties are rare in practice, as most decimal fractions have no exact binary
#' representation. `1.3 / 0.2` is marginally smaller than `6.5` in double
#' precision, so `roundTo(1.3, 0.2)` returns `1.2` and not the `1.4` that the
#' rule for ties would suggest. Results for a fractional `multiple` are
#' likewise only accurate to within representation error, which is why
#' `roundTo(x, 0.05)` may still print more than two decimal places.
#' 
#' A single `multiple` is used for all the values in `x`. A vector of step
#' widths is applied elementwise and must then be exactly as long as `x`, so
#' that a length mismatch is reported as an error instead of being recycled
#' silently.
#' 
#' @param x numeric. The values to be rounded. 
#' @param multiple numeric. The step width to whose multiples the values are to
#' be rounded, defaults to `1`. Must be finite and positive and either a single
#' value or as long as `x`.
#' @param FUN the rounding function applied to `x / multiple`. Typically one of
#' [round()] (default), [trunc()], [ceiling()] or [floor()]. Other functions
#' accepting and returning a numeric vector can be used as well.
#' 
#' @return a numeric vector of the rounded values, as long as `x`. `NA`s in `x`
#' are returned as `NA`.
#' 
#' @examples
#' 
#' roundTo(10, 3)     # rounds 10 to the nearest multiple of 3 (9)
#' roundTo(-10, 3)    # rounds -10 to the nearest multiple of 3 (-9)
#' 
#' roundTo(1.3, 0.2)  # rounds 1.3 to the nearest multiple of 0.2 (1.2)
#' roundTo(-1.3, 0.2) # rounds -1.3 to the nearest multiple of 0.2 (-1.2)
#' 
#' # prices to the nearest 5 cents
#' roundTo(c(1.02, 1.03, 12.375), 0.05)
#' 
#' # a step width for every value
#' roundTo(c(1.23, 123, 1234), c(0.05, 10, 100))
#' 
#' # any other length is an error, the values are not recycled
#' try(roundTo(1:6, c(2, 3)))
#' 
#' # round down
#' roundTo(c(1, -1) * 1.2335, 0.05, floor)
#' roundTo(c(1, -1) * 1233.5, 100, floor)
#' 
#' # round up
#' roundTo(c(1, -1) * 1.2335, 0.05, ceiling)
#' roundTo(c(1, -1) * 1233.5, 100, ceiling)
#' 
#' # round towards zero
#' roundTo(c(1, -1) * 1.2335, 0.05, trunc)
#' roundTo(c(1, -1) * 1233.5, 100, trunc)
#' 
#' # the four directions side by side
#' x <- c(-1.5, -1.3, 1.3, 1.5)
#' cbind(x       = x,
#'       round   = roundTo(x, 0.2, FUN = round),
#'       trunc   = roundTo(x, 0.2, FUN = trunc),
#'       ceiling = roundTo(x, 0.2, FUN = ceiling),
#'       floor   = roundTo(x, 0.2, FUN = floor)
#' )
#' 
#' # note how the ties in the first column are resolved to even multiples
#' x <- -5:5
#' cbind(x       = x,
#'       round   = roundTo(x, 2, FUN = round),
#'       trunc   = roundTo(x, 2, FUN = trunc),
#'       ceiling = roundTo(x, 2, FUN = ceiling),
#'       floor   = roundTo(x, 2, FUN = floor)
#' )
#' 
#' 
#' @seealso [round()], [trunc()], [ceiling()], [floor()] 
#' 
#' @family math.basic
#' @concept numerical-methods
#' @export
roundTo <- function(x, multiple = 1, FUN = round) {

  if (!is.function(FUN))
    stop("`FUN` must be a function.")

  # a scalar applies to all of x, several step widths are used elementwise
  if (!(length(multiple) == 1L || length(multiple) == length(x)))
    stop(gettextf("length of 'multiple' [%d] must be 1 or the length of 'x' [%d]",
                  length(multiple), length(x)), call. = FALSE)

  # multiple is the step width, a negative one would silently flip the
  # direction of floor(), ceiling() and trunc()
  if (!all(is.finite(multiple)) || any(multiple <= 0))
    stop("`multiple` must be finite and positive.")

  FUN(x / multiple) * multiple

}
