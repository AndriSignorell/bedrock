
#' Evaluate an expression under a temporary random seed
#'
#' Sets the random seed for the duration of \code{expr} and restores the
#' caller's random state afterwards. This makes a single result
#' reproducible without hijacking the random stream of the surrounding
#' script: two calls with the same seed give the same result, and whatever
#' is drawn after them is unaffected by either.
#'
#' The plain idiom \code{set.seed(s); expr} lacks the second half. In a
#' script that generates a series of random objects, seeding one of them
#' shifts every draw that follows, so results that were correct before the
#' seed was added silently change.
#'
#' @param seed a single number, or \code{NULL} to leave the random stream
#'   untouched and simply evaluate \code{expr}
#' @param expr the expression to evaluate; evaluated lazily, in the
#'   caller's environment
#'
#' @return the value of \code{expr}
#'
#' @examples
#' set.seed(1)
#' a <- runif(1)
#'
#' set.seed(1)
#' withSeed(99, runif(1))     # unrelated draw in between
#' identical(runif(1), a)     # the stream continued as if it never happened
#'
#' identical(withSeed(7, runif(3)), withSeed(7, runif(3)))
#'
#' @family utils
#' @concept random
#'
#' @export
withSeed <- function(seed, expr) {

  if (is.null(seed))
    return(expr)

  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed))
    stop("'seed' must be a single number or NULL")

  hasState <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)

  if (hasState) {
    state <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", state, envir = globalenv()), add = TRUE)
  } else {
    # no state yet: leave the session as unseeded as we found it
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())),
            add = TRUE)
  }

  set.seed(seed)
  expr
}
