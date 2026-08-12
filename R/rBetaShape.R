
#' Generate Beta-Distributed Random Values by Shape
#'
#' Generates beta-distributed random values using predefined distributional
#' shapes and transforms them to a specified interval.
#'
#' @param n non-negative integer giving the number of values to generate.
#' @param shape distributional shape: either one of the predefined names
#'   listed under \strong{Details}, or a numeric vector of length 2 giving
#'   `shape1` and `shape2` directly.
#' @param bounds numeric vector containing the lower and upper bound.
#'
#' @return a numeric vector of length `n` with values within `bounds`.
#'
#' @details
#' The following predefined shapes and beta parameters are available:
#'
#' \tabular{lrrl}{
#' \strong{Shape} \tab \strong{shape1} \tab \strong{shape2} \tab \strong{Description} \cr
#' `"norm"`  \tab 5.0 \tab 5.0 \tab symmetric and bell-shaped \cr
#' `"left"`  \tab 5.0 \tab 2.0 \tab left-skewed with values concentrated near the upper bound \cr
#' `"right"` \tab 2.0 \tab 5.0 \tab right-skewed with values concentrated near the lower bound \cr
#' `"unif"`  \tab 1.0 \tab 1.0 \tab uniform \cr
#' `"u"`     \tab 0.5 \tab 0.5 \tab U-shaped with values concentrated near both bounds \cr
#' `"j"`     \tab 2.0 \tab 0.5 \tab J-shaped with values concentrated near the upper bound \cr
#' `"inv-j"` \tab 0.5 \tab 2.0 \tab inverse J-shaped with values concentrated near the lower bound
#' }
#'
#' Note that `"left"` and `"right"` name the direction of the *skew*, i.e.
#' of the long tail, so `"right"` places the bulk of the values near the
#' lower bound. This is the standard convention, but it is the opposite of
#' what the names suggest at first reading - and unrelated to the meaning
#' of `"left"`/`"right"` in the `sides` argument of the interval functions,
#' where they name the side carrying the finite bound.
#'
#' The `"norm"` shape is symmetric and bell-shaped but is not a normal
#' distribution. Unlike the normal distribution, all generated values are
#' bounded.
#'
#' Values from the standard beta distribution on the interval \eqn{[0,1]}
#' are transformed to the interval specified by `bounds` as
#'
#' \deqn{a + (b-a)X}{a + (b - a) * X}
#'
#' where \eqn{a} and \eqn{b} are the lower and upper bounds, respectively.
#'
#' @section Random number generation:
#' The values are drawn with [stats::rbeta()] and therefore depend on the
#' state of R's global random number generator. No seed is set internally;
#' call [base::set.seed()] beforehand, or wrap the call in
#' [bedrock::withSeed()], for reproducible results.
#'
#' @seealso [stats::rbeta()], [stats::runif()]
#'
#' @family random.numbers
#' @concept sampling
#'
#' @examples
#' set.seed(42)
#'
#' x <- rBetaShape(
#'   1000,
#'   shape = "right",
#'   bounds = c(10, 90)
#' )
#'
#' summary(x)
#' range(x)
#'
#' # shape parameters can also be given directly
#' rBetaShape(5, shape = c(3, 1.5), bounds = c(0, 100))
#'
#' @export
rBetaShape <- function(
    n,
    shape = c(
      "norm",
      "left",
      "right",
      "unif",
      "u",
      "j",
      "inv-j"
    ),
    bounds = c(0, 1)) {

  if (length(n) != 1L ||
      !is.numeric(n) ||
      is.na(n) ||
      !is.finite(n) ||
      n < 0 ||
      n != floor(n)) {

    stop(
      "'n' must be a non-negative integer.",
      call. = FALSE
    )
  }

  if (!is.numeric(bounds) ||
      length(bounds) != 2L ||
      anyNA(bounds) ||
      any(!is.finite(bounds)) ||
      bounds[1L] >= bounds[2L]) {

    stop(
      "'bounds' must contain two finite numbers in increasing order.",
      call. = FALSE
    )
  }

  # An explicit (shape1, shape2) pair is accepted alongside the presets -
  # the same either-a-name-or-the-thing-itself pattern as the 'weights'
  # argument of cohenKappa(). Without it there is no way to ask for a
  # shape between two presets.
  if (is.numeric(shape)) {

    if (length(shape) != 2L || anyNA(shape) || any(!is.finite(shape)) ||
        any(shape <= 0))
      stop(
        "A numeric 'shape' must be two finite positive numbers ",
        "(shape1, shape2).",
        call. = FALSE
      )

    pars <- c(shape1 = shape[[1L]], shape2 = shape[[2L]])

  } else {

    shape <- match.arg(shape)

    pars <- switch(
      shape,
      "norm"  = c(shape1 = 5.0, shape2 = 5.0),
      "left"  = c(shape1 = 5.0, shape2 = 2.0),
      "right" = c(shape1 = 2.0, shape2 = 5.0),
      "unif"  = c(shape1 = 1.0, shape2 = 1.0),
      "u"     = c(shape1 = 0.5, shape2 = 0.5),
      "j"     = c(shape1 = 2.0, shape2 = 0.5),
      "inv-j" = c(shape1 = 0.5, shape2 = 2.0)
    )
  }

  # No as.integer() here: n has already been validated as a non-negative
  # whole number, and coercing it would turn anything above
  # .Machine$integer.max into NA (with a warning), which rbeta() then
  # rejects with an unrelated message. rbeta() takes a double n fine.
  x <- stats::rbeta(
    n,
    shape1 = unname(pars[["shape1"]]),
    shape2 = unname(pars[["shape2"]])
  )

  bounds[1L] + (bounds[2L] - bounds[1L]) * x
}
