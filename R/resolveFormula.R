
#' Parse and Classify a Model Formula
#'
#' Parses a model formula, builds the model frame and classifies the resulting
#' design into one of six dependency structures. The pieces of the design are
#' returned under a fixed set of names, so that every function offering a
#' formula interface can share one entry point instead of re-implementing the
#' parsing, the `subset` handling and the distinction between a grouping
#' factor, a numeric predictor and a blocking variable.
#'
#' @param formula a two-sided model formula. Supported forms are:
#'   \describe{
#'     \item{`y ~ 1`}{one-sample design.}
#'     \item{`Pair(x, y) ~ 1`}{two-sample dependent (paired). [Pair()]
#'       constructs a two-column matrix of paired observations.}
#'     \item{`y ~ g`}{two-sample or n-sample independent group comparison.}
#'     \item{`y ~ x`, `x` numeric}{numeric-numeric (correlation, regression).}
#'     \item{`y ~ trt | block`}{n-sample dependent (blocked design).}
#'   }
#' @param data an optional data frame containing the variables in `formula`.
#'   A matrix is coerced to a data frame.
#' @param subset an optional expression indicating which observations to use.
#'   Must be captured via [substitute()] in the calling function to avoid
#'   collision with [subset()]. See Details.
#' @param na.action a function specifying how missing values are handled,
#'   defaults to [na.pass()].
#' @param allowed a character vector restricting which design types are
#'   accepted, any combination of `"one-sample"`,
#'   `"two-sample-independent"`, `"two-sample-dependent"`,
#'   `"n-sample-independent"`, `"n-sample-dependent"` and
#'   `"numeric-numeric"`. The values are matched exactly, an unknown one is an
#'   error rather than being ignored. A further error is raised if the detected
#'   type is not among the allowed ones. Defaults to all types.
#'
#' @details
#' **Design types**
#'
#' \describe{
#'   \item{`one-sample`}{`y ~ 1`, as in the one-sample t-test or the
#'     one-sample Wilcoxon test.}
#'   \item{`two-sample-independent`}{`y ~ g` with two groups, as in the
#'     two-sample t-test or the Wilcoxon rank-sum test.}
#'   \item{`two-sample-dependent`}{`Pair(x, y) ~ 1`, as in the paired t-test
#'     or the Wilcoxon signed-rank test.}
#'   \item{`n-sample-independent`}{`y ~ g` with more than two groups, as in
#'     the analysis of variance or the Kruskal-Wallis test.}
#'   \item{`n-sample-dependent`}{`y ~ trt | block`, as in a repeated-measures
#'     analysis of variance or the Friedman test.}
#'   \item{`numeric-numeric`}{`y ~ x` with a numeric right-hand side, as in
#'     correlation or regression.}
#' }
#'
#' **Type detection**
#'
#' The type follows from the shape of the formula and from the class of the
#' right-hand side variable, not from `allowed`. `allowed` only decides
#' whether the detected type is accepted, with two exceptions worth knowing.
#' A grouping factor carrying a single level is reported as `one-sample` if
#' that type is allowed, and a two-group design is reported as
#' `n-sample-independent` if `"two-sample-independent"` is not among the
#' allowed types. Both are deliberate: a caller that treats every group count
#' alike needs to allow one type only.
#'
#' **Field naming contract (binding across all types)**
#'
#' \itemize{
#'   \item `group` is reserved for a categorical, factor-coercible variable of
#'     length `n` (the full sample) that splits the response into groups. It
#'     is never pre-split and never used for a continuous variable. `x` and
#'     `group` have an identical shape for `two-sample-independent` and for
#'     `n-sample-independent`, so that a caller can use
#'     `split(r$x, r$group)` uniformly, without branching on the number of
#'     groups.
#'   \item `predictor` is used for a continuous, numeric right-hand side
#'     variable (`numeric-numeric`), never `group`.
#'   \item `treatment` is used for the explanatory variable of a blocked
#'     design (`n-sample-dependent`), as distinct from `block`, the
#'     stratification factor. Neither is ever called `group`.
#'   \item `y`, where present, is a convenience field only, holding the second
#'     group of a two-sample design or the second paired vector. It is never
#'     needed for correct use: `x` and `group` (or `x` and `predictor`, or
#'     `treatment` and `block`) are always sufficient and are the canonical
#'     access path.
#' }
#'
#' **Missing values**
#'
#' Missing values are left to `na.action` and are not touched otherwise, so
#' with the default [na.pass()] they reach the caller untouched. The one
#' exception is the grouping factor of an independent design, where empty and
#' missing levels are dropped before the groups are counted. A grouping
#' variable that is missing throughout leaves no level at all and is an error.
#'
#' **subset handling**
#'
#' Because `subset` is both an argument name and a base R function, name
#' collisions can occur when forwarding to [model.frame()]. The calling
#' function must therefore capture `subset` as an unevaluated expression and
#' pass the resulting object on directly:
#'
#' \preformatted{
#' myFun <- function(formula, data, subset, na.action = na.pass, ...) {
#'   subsetExpr <- if (!missing(subset)) substitute(subset) else NULL
#'   resolveFormula(formula, data,
#'                  subset    = subsetExpr,
#'                  na.action = na.action)
#' }
#' }
#'
#' **Return components by type**
#'
#' Every return value contains `type`, `mf` and `dataName`. The remaining
#' components depend on the design:
#'
#' \describe{
#'   \item{`one-sample`}{`x`}
#'   \item{`two-sample-independent`}{`x`, `group`, `y` (convenience: the
#'     second group)}
#'   \item{`two-sample-dependent`}{`x`, `y`}
#'   \item{`n-sample-independent`}{`x`, `group`}
#'   \item{`n-sample-dependent`}{`response`, `treatment`, `block`}
#'   \item{`numeric-numeric`}{`x`, `predictor`}
#' }
#'
#' @return a named list containing at least:
#' \describe{
#'   \item{type}{character, one of the design types listed above.}
#'   \item{mf}{the [model.frame()] the design was read from.}
#'   \item{dataName}{character, the deparsed formula, for use as the
#'     `data.name` of an `htest` object.}
#' }
#' plus the design-specific components described under Details.
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
#' ## [1] "one-sample"
#'
#' # two-sample independent: x and group have full length, the same shape
#' # as for more than two groups
#' r2 <- resolveFormula(y ~ g2, data = df,
#'                      allowed = c("two-sample-independent",
#'                                  "n-sample-independent"))
#' r2$type
#' ## [1] "two-sample-independent"
#' length(r2$x) == length(r2$group)
#' ## [1] TRUE
#'
#' # n-sample independent
#' resolveFormula(y ~ g3, data = df,
#'                allowed = "n-sample-independent")$type
#' ## [1] "n-sample-independent"
#'
#' # two-sample dependent (paired)
#' df2 <- data.frame(pre = rnorm(15, 50, 10), post = rnorm(15, 55, 10))
#' resolveFormula(Pair(pre, post) ~ 1, data = df2,
#'                allowed = c("one-sample",
#'                            "two-sample-dependent"))$type
#' ## [1] "two-sample-dependent"
#'
#' # n-sample dependent (blocked): treatment, not group
#' r4 <- resolveFormula(y ~ trt | blk, data = df,
#'                      allowed = "n-sample-dependent")
#' names(r4)
#' ## [1] "type" "mf" "response" "treatment" "block" "dataName"
#'
#' # numeric-numeric: predictor, not group
#' df3 <- data.frame(y = rnorm(20), x = rnorm(20))
#' r5 <- resolveFormula(y ~ x, data = df3, allowed = "numeric-numeric")
#' is.numeric(r5$predictor)
#' ## [1] TRUE
#'
#' @seealso [model.frame()], [Pair()], [resolveGroups()]
#'
#' @family data.resolve
#' @concept programming
#' @concept data-resolution
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
  
  if (length(formula) < 3L)
    stop("'formula' must be two-sided, of the form response ~ terms")
  
  # the default is the single source of truth for the valid design types
  designTypes <- eval(formals(resolveFormula)$allowed)
  
  if (!is.character(allowed) || !length(allowed) || anyNA(allowed))
    stop("'allowed' must be a character vector of design types")
  
  if (!all(allowed %in% designTypes))
    stop(gettextf("invalid design type in 'allowed': %s",
                  paste(sQuote(setdiff(allowed, designTypes)), collapse = ", ")),
         call. = FALSE)
  
  
  # ── Coerce matrix data ────────────────────────────────────────────────────
  if (!missing(data) && is.matrix(data))
    data <- as.data.frame(data)
  
  # ── Capture environment and subset before any frame changes ───────────────
  env        <- parent.frame()
  subsetExpr <- if (!missing(subset)) substitute(subset) else NULL
  hasData    <- !missing(data)
  dname      <- deparse1(formula)
  
  # ── Helper: build model.frame via bquote/eval ─────────────────────────────
  # Using bquote + eval(envir=env) avoids match.call() manipulation and
  # correctly resolves variables in the caller's environment.
  .mf <- function(f) {
    args <- list(formula   = f,
                 na.action = na.action)
    
    if (hasData)
      args$data <- data
    
    if (!is.null(subsetExpr))
      args$subset <- eval(subsetExpr, envir = if (hasData) data else env,
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
      dataName  = dname
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
        type     = "two-sample-dependent",
        mf       = mf,
        x        = response[, 1L],
        y        = response[, 2L],
        dataName = dname
      ))
    }
    
    if (!"one-sample" %in% allowed)
      stop("'one-sample' design not allowed by 'allowed' argument")
    
    return(list(
      type     = "one-sample",
      mf       = mf,
      x        = response,
      dataName = dname
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
      dataName  = dname
    ))
  }
  
  # ── 2c. Grouped: two-sample or n-sample independent ──────────────────────
  g <- droplevels(factor(mf[[2L]], exclude = NA))
  k <- nlevels(g)
  
  # no level survives when the grouping variable is missing throughout, or
  # when 'subset' has filtered out every observation
  if (k == 0L)
    stop("grouping factor has no non-missing levels")
  
  # k == 1: Fallback to one-sample if allowed
  if (k == 1L) {
    if (!"one-sample" %in% allowed)
      stop("grouping factor has only 1 level")
    return(list(
      type     = "one-sample",
      mf       = mf,
      x        = response,
      dataName = dname
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
    type     = type,
    mf       = mf,
    x        = response,   # full response, length n - same shape for k=2 and k>2
    group    = g,          # full factor, length n - same shape for k=2 and k>2
    dataName = dname
  )
  
  # y is a convenience-only field for the binary case; x + group remains
  # the canonical access path and has identical shape across k.
  if (type == "two-sample-independent")
    out$y <- split(response, g, drop = TRUE)[[2L]]
  
  out
}
