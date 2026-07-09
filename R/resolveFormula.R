
#' Parse and Classify a Model Formula
#'
#' Parses a model formula, constructs a model frame, and classifies
#' the resulting design into one of five dependency structures.
#' The function serves as a unified entry point for functions that accept
#' a formula interface.
#'
#' @param formula a model formula. Supported forms are:
#'   \describe{
#'     \item{\code{y ~ 1} or \code{y}}{one-sample design}
#'     \item{\code{Pair(x, y) ~ 1}}{two-sample dependent (paired).
#'       \code{\link[stats]{Pair}} constructs a two-column matrix of
#'       paired observations.}
#'     \item{\code{y ~ g}}{two-sample or n-sample independent group
#'       comparison}
#'     \item{\code{y ~ x}, \code{x} numeric}{numeric-numeric (correlation,
#'       regression)}
#'     \item{\code{y ~ trt | block}}{n-sample dependent (blocked design)}
#'   }
#' @param data an optional data frame containing the variables in
#'   \code{formula}. A matrix is coerced to a data frame.
#' @param subset an optional expression indicating which observations to
#'   use. Must be captured via \code{substitute()} in the calling function
#'   to avoid collision with \code{base::subset()}. See Details.
#' @param na.action a function specifying how missing values are handled.
#'   Defaults to \code{\link[stats]{na.pass}}.
#' @param allowed a character vector restricting which design types are
#'   accepted. Any combination of:
#'   \code{"one-sample"},
#'   \code{"two-sample-independent"},
#'   \code{"two-sample-dependent"},
#'   \code{"n-sample-independent"},
#'   \code{"n-sample-dependent"},
#'   \code{"numeric-numeric"}.
#'   An error is raised if the detected type is not in \code{allowed}.
#'   Default allows all types.
#'
#' @details
#' \strong{Design types:}
#'
#' \tabular{lll}{
#'   \strong{type}                  \tab \strong{Formula}        \tab \strong{Examples} \cr
#'   \code{one-sample}              \tab \code{y ~ 1}            \tab t-test, Wilcoxon one-sample \cr
#'   \code{two-sample-independent}  \tab \code{y ~ g} (k=2)     \tab t-test, Wilcoxon rank-sum \cr
#'   \code{two-sample-dependent}    \tab \code{Pair(x,y) ~ 1}   \tab paired t-test, Wilcoxon signed-rank \cr
#'   \code{n-sample-independent}    \tab \code{y ~ g} (k>2)     \tab ANOVA, Kruskal-Wallis \cr
#'   \code{n-sample-dependent}      \tab \code{y ~ trt | block} \tab repeated measures ANOVA, Friedman \cr
#'   \code{numeric-numeric}         \tab \code{y ~ x} (x numeric) \tab correlation, regression \cr
#' }
#'
#' \strong{Field naming contract (binding across all types):}
#'
#' \itemize{
#'   \item \code{group} is reserved exclusively for a categorical,
#'     factor-coercible variable of length \code{n} (the full sample) that
#'     splits the response into groups. It is never pre-split and never
#'     used for a continuous variable. \code{x} + \code{group} have an
#'     \emph{identical shape} for both \code{two-sample-independent} and
#'     \code{n-sample-independent} - callers can use
#'     \code{split(r$x, r$group)} uniformly, without branching on \code{k}.
#'   \item \code{predictor} is used for a continuous, numeric right-hand
#'     side variable (\code{numeric-numeric}). Never called \code{group}.
#'   \item \code{treatment} is used for the treated/explanatory variable in
#'     a blocked design (\code{n-sample-dependent}), distinct from
#'     \code{block}, the stratification factor. Never called \code{group}.
#'   \item \code{y}, where present, is always a \emph{convenience} field
#'     (e.g. group 2 of a two-sample design, or the second paired vector).
#'     It is never required for correct use - \code{x} + \code{group} (or
#'     \code{x} + \code{predictor} / \code{treatment} + \code{block}) is
#'     always sufficient and is the canonical access path.
#' }
#'
#' \strong{subset handling:}
#'
#' Because \code{subset} is both an argument name and a base R function,
#' name collisions can occur when forwarding to \code{stats::model.frame}.
#' The calling function must capture \code{subset} as an unevaluated
#' expression:
#'
#' \preformatted{
#' myFun <- function(formula, data, subset, na.action = na.pass, ...) {
#'   subset_expr <- if (!missing(subset)) substitute(subset) else NULL
#'   resolveFormula(formula, data,
#'                  subset    = subset_expr,
#'                  na.action = na.action)
#' }
#' }
#'
#' \strong{Return value components by type:}
#'
#' All return values contain \code{type}, \code{mf} and \code{data.name}.
#' Additional components depend on the design:
#'
#' \tabular{ll}{
#'   \strong{type}                  \tab \strong{Additional components} \cr
#'   \code{one-sample}              \tab \code{x} \cr
#'   \code{two-sample-independent}  \tab \code{x}, \code{group}, \code{y} (convenience: group 2) \cr
#'   \code{two-sample-dependent}    \tab \code{x}, \code{y} \cr
#'   \code{n-sample-independent}    \tab \code{x}, \code{group} \cr
#'   \code{n-sample-dependent}      \tab \code{response}, \code{treatment}, \code{block} \cr
#'   \code{numeric-numeric}         \tab \code{x}, \code{predictor} \cr
#' }
#'
#' @return a named list with at minimum:
#' \describe{
#'   \item{\code{type}}{character, one of the design types listed above}
#'   \item{\code{mf}}{the \code{\link[stats]{model.frame}}}
#'   \item{\code{data.name}}{the deparsed formula string}
#' }
#' Plus design-specific components as described in Details.
#'
#' @seealso
#'   \code{\link[stats]{model.frame}},
#'   \code{\link[stats]{Pair}},
#'   \code{\link[DescToolsX]{desc}}
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y   = rnorm(30, 50, 10),
#'   g2  = rep(c("A", "B"), 15),
#'   g3  = rep(c("A", "B", "C"), 10),
#'   trt = rep(c("T1", "T2", "T3"), 10),
#'   blk = rep(1:10, 3)
#' )
#'
#' # one-sample
#' resolveFormula(y ~ 1, data = df)$type
#' #> [1] "one-sample"
#'
#' # two-sample independent: x + group have full length, same shape as k>2
#' r2 <- resolveFormula(y ~ g2, data = df,
#'                      allowed = c("two-sample-independent",
#'                                  "n-sample-independent"))
#' r2$type
#' #> [1] "two-sample-independent"
#' length(r2$x) == length(r2$group)
#' #> [1] TRUE
#'
#' # n-sample independent
#' resolveFormula(y ~ g3, data = df,
#'                allowed = "n-sample-independent")$type
#' #> [1] "n-sample-independent"
#'
#' # two-sample dependent (paired)
#' df2 <- data.frame(pre = rnorm(15, 50, 10), post = rnorm(15, 55, 10))
#' resolveFormula(Pair(pre, post) ~ 1, data = df2,
#'                allowed = c("one-sample",
#'                            "two-sample-dependent"))$type
#' #> [1] "two-sample-dependent"
#'
#' # n-sample dependent (blocked): treatment, not group
#' r4 <- resolveFormula(y ~ trt | blk, data = df,
#'                      allowed = "n-sample-dependent")
#' r4$type
#' #> [1] "n-sample-dependent"
#' names(r4)
#'
#' # numeric-numeric: predictor, not group
#' df3 <- data.frame(y = rnorm(20), x = rnorm(20))
#' r5 <- resolveFormula(y ~ x, data = df3, allowed = "numeric-numeric")
#' r5$type
#' #> [1] "numeric-numeric"
#' is.numeric(r5$predictor)
#' #> [1] TRUE
#'

#' @family data.utils  
#' @concept formula  
#' @concept modelling
#'
#'
#' @export
resolveFormula <- function(
    formula,
    data,
    subset,
    na.action = na.pass,
    allowed   = c("one-sample",
                  "two-sample-independent",
                  "two-sample-dependent",
                  "n-sample-independent",
                  "n-sample-dependent",
                  "numeric-numeric")
) {
  
  # ── Validate ──────────────────────────────────────────────────────────────
  if (missing(formula))
    stop("'formula' is missing")
  
  if (!inherits(formula, "formula"))
    stop("'formula' must be a formula object")
  
  
  # ── Coerce matrix data ────────────────────────────────────────────────────
  if (!missing(data) && is.matrix(data))
    data <- as.data.frame(data)
  
  # ── Capture environment and subset before any frame changes ───────────────
  env         <- parent.frame()
  subset_expr <- if (!missing(subset)) substitute(subset) else NULL
  has_data    <- !missing(data)
  dname       <- deparse1(formula)
  
  # ── Helper: build model.frame via bquote/eval ─────────────────────────────
  # Using bquote + eval(envir=env) avoids match.call() manipulation and
  # correctly resolves variables in the caller's environment.
  .mf <- function(f) {
    args <- list(formula   = f,
                 na.action = na.action)
    
    if (has_data)
      args$data <- data
    
    if (!is.null(subset_expr))
      args$subset <- eval(subset_expr, envir = if (has_data) data else env,
                          enclos = env)
    
    do.call(model.frame, args)
  }
  
  # ── 1. n-sample-dependent: y ~ trt | block ───────────────────────────────
  rhs <- formula[[3L]]
  
  if (is.call(rhs) && identical(rhs[[1L]], as.name("|"))) {
    
    if (!"n-sample-dependent" %in% allowed)
      stop("'n-sample-dependent' design not allowed by 'allowed' argument")
    
    f2             <- formula
    f2[[3L]][[1L]] <- as.name("+")
    mf             <- .mf(f2)
    
    if (ncol(mf) != 3L)
      stop("blocked formula must be of the form y ~ trt | block")
    
    return(list(
      type      = "n-sample-dependent",
      mf        = mf,
      response  = mf[[1L]],
      treatment = mf[[2L]],
      block     = mf[[3L]],
      data.name = dname
    ))
  }
  
  # ── 2. All other ──────────────────────────────────────────────────────────
  mf       <- .mf(formula)
  if (ncol(mf) > 2L)
    stop("'formula' should be of the form response ~ group")
  
  response <- mf[[1L]]
  
  # ── 2a. One-sample or two-sample dependent ────────────────────────────────
  if (ncol(mf) == 1L) {
    
    if (!any(c("one-sample", "two-sample-dependent") %in% allowed))
      stop("'one-sample' / 'two-sample-dependent' design not allowed by 'allowed' argument")
    
    if (inherits(response, "Pair")) {
      
      if (!"two-sample-dependent" %in% allowed)
        stop("'two-sample-dependent' design not allowed by 'allowed' argument")
      
      return(list(
        type      = "two-sample-dependent",
        mf        = mf,
        x         = response[, 1L],
        y         = response[, 2L],
        data.name = dname
      ))
    }
    
    if (!"one-sample" %in% allowed)
      stop("'one-sample' design not allowed by 'allowed' argument")
    
    return(list(
      type      = "one-sample",
      mf        = mf,
      x         = response,
      data.name = dname
    ))
  }
  
  # ── 2b. numeric ~ numeric ────────────────────────────────────────────────
  if (is.numeric(mf[[2L]])) {
    if (!"numeric-numeric" %in% allowed)
      stop("right-hand side of 'formula' is numeric, but a ",
           "'numeric-numeric' design is not allowed here; ",
           "supply a grouping factor instead")
    return(list(
      type      = "numeric-numeric",
      mf        = mf,
      x         = response,
      predictor = mf[[2L]],     # numeric predictor, never called 'group'
      data.name = dname
    ))
  }
  
  # ── 2c. Grouped: two-sample or n-sample independent ──────────────────────
  g <- droplevels(factor(mf[[2L]], exclude = NA))
  k <- nlevels(g)
  
  # k == 1: Fallback to one-sample if allowed
  if (k == 1L) {
    if (!"one-sample" %in% allowed)
      stop("grouping factor has only 1 level")
    return(list(
      type      = "one-sample",
      mf        = mf,
      x         = response,
      data.name = dname
    ))
  }
  
  if (k == 2L && !"two-sample-independent" %in% allowed &&
      !"n-sample-independent" %in% allowed)
    stop("grouped design not allowed by 'allowed' argument")
  
  if (k > 2L && !"n-sample-independent" %in% allowed)
    stop("'n-sample-independent' design not allowed by 'allowed' argument")
  
  type <- if (k == 2L && "two-sample-independent" %in% allowed)
    "two-sample-independent"
  else
    "n-sample-independent"
  
  out <- list(
    type      = type,
    mf        = mf,
    x         = response,   # full response, length n - same shape for k=2 and k>2
    group     = g,          # full factor, length n - same shape for k=2 and k>2
    data.name = dname
  )
  
  # y is a convenience-only field for the binary case; x + group remains
  # the canonical access path and has identical shape across k.
  if (type == "two-sample-independent")
    out$y <- split(response, g, drop = TRUE)[[2L]]
  
  out
}

