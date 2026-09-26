
#' Parse and Classify a Model Formula
#'
#' Parses a model formula, builds the model frame and classifies the resulting
#' design into one of seven dependency structures. The pieces of the design are
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
#'     \item{`y ~ a:b`}{independent group comparison of the cells of several
#'       grouping variables, combined into one grouping factor. `y ~ a + b`
#'       is not a grouped design: it is `regression` if allowed, else an
#'       error.}
#'     \item{`y ~ x`, `x` numeric}{numeric-numeric (correlation, simple regression).}
#'     \item{`y ~ x1 + x2 + ...`}{general regression.}
#'     \item{`y ~ trt | block`}{n-sample dependent (blocked design).}
#'   }
#' @param data an optional data frame containing the variables in `formula`.
#'   A matrix is coerced to a data frame.
#' @param subset an optional expression indicating the observations to use,
#'   evaluated in `data` as in [model.frame()] (`subset = len > 10`), or an
#'   index vector. See Details.
#' @param na.action a function specifying how missing values are handled,
#'   defaults to [na.pass()].
#' @param allowed a character vector restricting which design types are
#'   accepted, any combination of `"one-sample"`,
#'   `"two-sample-independent"`, `"two-sample-dependent"`,
#'   `"n-sample-independent"`, `"n-sample-dependent"`,
#'   `"numeric-numeric"` and `"regression"`. The values are matched exactly,
#'   an unknown one is an
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
#'     correlation or simple regression.}
#'   \item{`regression`}{a general regression formula with one or more
#'     predictors.}
#' }
#'
#' **Type detection**
#'
#' The type follows from the shape of the formula and from the class of the
#' right-hand side variable, not from `allowed`. `allowed` only decides
#' whether the detected type is accepted, with four exceptions worth knowing.
#' \enumerate{
#'   \item A grouping factor carrying a single level is reported as
#'     `one-sample` if that type is allowed.
#'   \item A two-group design is reported as `n-sample-independent` if
#'     `"two-sample-independent"` is not among the allowed types. Together
#'     with the previous rule this lets a caller that treats every group count
#'     alike allow one type only.
#'   \item `allowed = "regression"` on its own forces the `regression` type
#'     for every formula, including `y ~ 1` and `y ~ g`, with the exception
#'     of the blocked syntax `y ~ trt | block`, which is always
#'     `n-sample-dependent` and therefore an error then. This is the entry
#'     point for model-fitting callers, which interpret the right-hand side
#'     themselves. Duplicates in `allowed` are ignored, so
#'     `c("regression", "regression")` behaves the same.
#'   \item Otherwise `regression` is reported only for more than one
#'     right-hand side variable. With `allowed` containing both `"regression"`
#'     and `"numeric-numeric"`, `y ~ x` is therefore `numeric-numeric` while
#'     `y ~ x1 + x2` is `regression`.
#' }
#'
#' **Cells of several grouping variables**
#'
#' A right-hand side consisting of a single interaction term, `y ~ a:b` (or
#' `y ~ a:b:c`), is the explicit request for the cells of these variables.
#' They are combined into one grouping factor via [interaction()], with
#' levels such as `"OJ:0.5"`; numeric components are treated as categorical.
#' The design is then classified like `y ~ g`, by the number of non-empty
#' cells.
#'
#' Unlike [boxplot()], `y ~ a + b` is *not* read as cells: additive terms are
#' not an interaction, as in [lm()]. Such a formula, like `y ~ a * b`, is
#' `regression` if that type is allowed, and an error otherwise. If
#' `"regression"` is allowed, it also takes precedence over the cell reading
#' of `y ~ a:b`, which a model-fitting caller interprets as an interaction
#' term.
#'
#' The distinction rests on the terms of the formula: the model frame
#' holds the variables `a` and `b` in both cases and cannot tell the two
#' apart.
#'
#' [offset()] is only accepted for the `regression` design and an error
#' otherwise: an offset column is part of the model frame without being a
#' term, and would pass for a grouping variable or a numeric predictor.
#'
#' **Field naming contract (binding across all types)**
#'
#' \itemize{
#'   \item `response` is present for every type and always holds the
#'     left-hand side of the formula. It is the one field a caller can rely on
#'     without branching on `type`.
#'   \item `x` is an alias of `response` for the types that are conventionally
#'     described in terms of a sample rather than a model (`one-sample`,
#'     `two-sample-independent`, `n-sample-independent`, `numeric-numeric`).
#'     The exception is `two-sample-dependent`: there `response` is the whole
#'     [Pair()] matrix, while `x` and `y` are its first and second column.
#'   \item `group` is reserved for a categorical, factor-coercible variable of
#'     length `n` (the full sample) that splits the response into groups. It
#'     is never pre-split and never used for a continuous variable. `x` and
#'     `group` have an identical shape for `two-sample-independent` and for
#'     `n-sample-independent`, so that a caller can use
#'     `split(r$x, r$group)` uniformly, without branching on the number of
#'     groups. For `y ~ a:b`, `group` holds the combined cell factor.
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
#'   \item `rows` is present for every type and holds the positions of the
#'     retained observations in the original data, after `subset` and after
#'     `na.action`. It is the handle for synchronising an external vector
#'     (an ordering variable, weights) with the model frame:
#'     `z <- z[r$rows]`. It is `NULL` in the rare case where the row names of
#'     the model frame cannot be matched back, e.g. when a numeric `subset`
#'     selects a row twice.
#'   \item `terms` is returned for the `regression` type. Build the design
#'     matrix from it, `model.matrix(r$terms, r$mf)`, never from the original
#'     formula: the columns of a model frame are named after the deparsed
#'     expressions (`"log(x)"`), so re-evaluating the formula against the
#'     model frame fails for every transformed term.
#' }
#'
#' **Missing values**
#'
#' Missing values are left to `na.action` and are not touched otherwise, so
#' with the default [na.pass()] they reach the caller untouched. The one
#' exception is the grouping factor of an independent design, where empty and
#' missing levels are dropped before the groups are counted. A grouping
#' variable that is missing throughout leaves no level at all and is an error.
#' A cell of `y ~ a:b` is missing as soon as one of its components is.
#'
#' Rows removed by `na.action` are recorded in `attr(r$mf, "na.action")`, but
#' those indices are relative to the already subsetted frame. To align an
#' external vector with the model frame use `rows`, which accounts for
#' `subset` and `na.action` at once.
#'
#' **subset handling**
#'
#' `subset` is evaluated as in base R: it is taken unevaluated, like
#' [lm()] does, by handing this function's own call on to [model.frame()],
#' which evaluates the expression in `data`, with `environment(formula)` as
#' the enclosure. `resolveFormula(y ~ g, df, subset = g == "A")` therefore
#' works exactly like `boxplot(y ~ g, df, subset = g == "A")`, and a quoted
#' expression fails in both.
#'
#' A wrapper function forwards its arguments the same way base R does (see
#' [boxplot.formula()]): it rebuilds its own call and evaluates it in its
#' caller's frame, so that `subset` reaches `resolveFormula()` unevaluated.
#' `resolveFormulaFromCall()` does exactly this and is the entry point to use
#' in a formula method:
#'
#' \preformatted{
#' myFun <- function(formula, data, subset, na.action = na.omit, ...) {
#'   r <- resolveFormulaFromCall(
#'          allowed   = c("two-sample-independent", "n-sample-independent"),
#'          na.action = na.action)
#'   ...
#' }
#' }
#'
#' Passing a captured expression on by value
#' (`resolveFormula(formula, data, subset = substitute(subset))`) does not
#' work, just as it does not for [model.frame()] itself.
#'
#' **Return components by type**
#'
#' Every return value starts with `type`, `mf`, `rows` and `response`, in that
#' order, followed by the design-specific components below, and ends with
#' `dataName`:
#'
#' \describe{
#'   \item{`one-sample`}{`x`}
#'   \item{`two-sample-independent`}{`x`, `group`, `y` (convenience: the
#'     second group)}
#'   \item{`two-sample-dependent`}{`x`, `y`}
#'   \item{`n-sample-independent`}{`x`, `group`}
#'   \item{`n-sample-dependent`}{`treatment`, `block`}
#'   \item{`numeric-numeric`}{`x`, `predictor`}
#'   \item{`regression`}{`terms`}
#' }
#'
#' @return a named list containing at least:
#' \describe{
#'   \item{type}{character, one of the design types listed above.}
#'   \item{mf}{the [model.frame()] the design was read from.}
#'   \item{rows}{integer, the positions of the retained observations in the
#'     original data, or `NULL` if they cannot be determined.}
#'   \item{response}{the left-hand side of the formula.}
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
#' # cells of two grouping variables: a:b, not a + b
#' r3 <- resolveFormula(y ~ g2:g3, data = df,
#'                      allowed = "n-sample-independent")
#' levels(r3$group)
#' ## [1] "A:A" "B:A" "A:B" "B:B" "A:C" "B:C"
#' try(resolveFormula(y ~ g2 + g3, data = df,
#'                    allowed = "n-sample-independent"))
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
#' ## [1] "type" "mf" "rows" "response" "treatment" "block" "dataName"
#'
#' # numeric-numeric: predictor, not group
#' df3 <- data.frame(y = rnorm(20), x = rnorm(20))
#' r5 <- resolveFormula(y ~ x, data = df3, allowed = "numeric-numeric")
#' is.numeric(r5$predictor)
#' ## [1] TRUE
#'
#' # regression: build the design matrix from 'terms', not from the formula
#' r6 <- resolveFormula(y ~ log(abs(x)) + I(x^2), data = df3,
#'                      allowed = "regression")
#' colnames(model.matrix(r6$terms, r6$mf))
#'
#' # subset, as in base R
#' resolveFormula(y ~ g3, data = df, subset = g3 != "C",
#'                allowed = "two-sample-independent")$type
#' ## [1] "two-sample-independent"
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
                  "numeric-numeric",
                  "regression")
) {

  # ── Validate ──────────────────────────────────────────────────────────────
  if (missing(formula))
    stop("'formula' is missing", call. = FALSE)

  if (!inherits(formula, "formula"))
    stop("'formula' must be a formula object", call. = FALSE)

  if (length(formula) < 3L)
    stop("'formula' must be two-sided, of the form response ~ terms",
         call. = FALSE)

  # the default is the single source of truth for the valid design types;
  # sys.function() keeps this independent of the binding this function is
  # reachable under
  designTypes <- eval(formals(sys.function())$allowed)

  if (!is.character(allowed) || !length(allowed) || anyNA(allowed))
    stop("'allowed' must be a character vector of design types", call. = FALSE)

  if (!all(allowed %in% designTypes))
    stop(gettextf("invalid design type in 'allowed': %s",
                  paste(sQuote(setdiff(allowed, designTypes)), collapse = ", ")),
         call. = FALSE)

  # duplicates are accepted, but must not defeat the checks below, which
  # compare 'allowed' as a whole (identical(allowed, "regression"))
  allowed <- unique(allowed)


  # ── Coerce matrix data ────────────────────────────────────────────────────
  hasData <- !missing(data) && !is.null(data)

  dataCoerced <- hasData && is.matrix(data)
  if (dataCoerced)
    data <- as.data.frame(data)

  dname <- deparse1(formula)

  # ── Helper: positions of the retained rows in the original data ───────────
  # subset is applied by model.frame() before na.action, and the indices in
  # attr(mf, "na.action") are relative to the subsetted frame. The row names
  # are the only representation that survives both steps.
  .rows <- function(mf) {
    rn <- rownames(mf)
    if (is.null(rn))
      return(NULL)

    idx <- if (hasData && !is.null(rownames(data)))
      match(rn, rownames(data))
    else
      suppressWarnings(as.integer(rn))

    if (anyNA(idx)) NULL else idx
  }

  # ── Helper: assemble the return value ─────────────────────────────────────
  # One place enforces the field naming contract: type, mf, rows and response
  # exist for every design, dataName closes every result.
  .result <- function(type, mf, response, ...)
    c(list(type     = type,
           mf       = mf,
           rows     = .rows(mf),
           response = response),
      list(...),
      list(dataName = dname))

  # ── Helper: build the model frame ─────────────────────────────────────────
  # As in lm(): this function's own call is turned into a model.frame() call
  # and evaluated in the caller's frame, so that 'subset' is taken
  # unevaluated and evaluated by model.frame() in 'data' - exactly the base R
  # semantics. formula and na.action are inlined as values (the formula may
  # have been rewritten, na.action carries this function's default), data
  # only if it had to be coerced from a matrix.
  mfCall <- match.call(expand.dots = FALSE)
  mfCall <- mfCall[c(1L, match(c("data", "subset"), names(mfCall), 0L))]
  mfCall[[1L]]     <- quote(stats::model.frame)
  mfCall$na.action <- na.action
  if (dataCoerced)
    mfCall$data <- data
  callerEnv <- parent.frame()

  .mf <- function(f) {
    mfCall$formula <- f
    eval(mfCall, callerEnv)
  }

  # ── Helper: reject offsets outside regression ────────────────────────────
  # An offset() column sits in the model frame without being a term, and
  # would pass for a grouping variable, a numeric predictor or a block.
  .noOffset <- function(mf) {
    if (!is.null(attr(attr(mf, "terms"), "offset")))
      stop("offset() is only supported for the 'regression' design",
           call. = FALSE)
  }

  # ── 1. n-sample-dependent: y ~ trt | block ───────────────────────────────
  rhs <- formula[[3L]]

  if (is.call(rhs) && identical(rhs[[1L]], as.name("|"))) {

    if (!"n-sample-dependent" %in% allowed)
      stop("'n-sample-dependent' design not allowed by 'allowed' argument",
           call. = FALSE)

    f2             <- formula
    f2[[3L]][[1L]] <- as.name("+")
    mf             <- .mf(f2)

    .noOffset(mf)

    if (ncol(mf) != 3L)
      stop("blocked formula must be of the form y ~ trt | block",
           call. = FALSE)

    return(.result("n-sample-dependent", mf, mf[[1L]],
                   treatment = mf[[2L]],
                   block     = mf[[3L]]))
  }

  # ── 2. All other ──────────────────────────────────────────────────────────
  mf       <- .mf(formula)
  response <- mf[[1L]]

  # ── 2a. General regression ────────────────────────────────────────────────
  # A regression caller needs the complete model frame and performs its own
  # interpretation of the right-hand side. 'terms' is handed out with it, since
  # model.matrix() must be built from the terms, not from the formula. This
  # takes precedence over the cell reading of y ~ a:b below, which a
  # model-fitting caller interprets as an interaction term.
  if (identical(allowed, "regression") ||
      (ncol(mf) > 2L && "regression" %in% allowed)) {
    return(.result("regression", mf, response,
                   terms = attr(mf, "terms")))
  }

  # ── Cells: y ~ a:b ────────────────────────────────────────────────────────
  # A single interaction term is the user's explicit request for the cells
  # of several grouping variables; it becomes one grouping factor. y ~ a + b
  # is not read the same way (unlike boxplot()): additive terms are not
  # cells, as in lm(). The model frame alone cannot tell the two apart - it
  # holds the variables a and b in both cases - only the term labels can.
  #
  # Offsets are rejected first: otherwise y ~ g + offset(x) is read as the
  # cells of g and x, and y ~ offset(x) as numeric-numeric.
  .noOffset(mf)
  tt <- attr(mf, "terms")

  # a single term of order > 1 is an interaction; its variables are then
  # exactly the right-hand side columns of the model frame
  gCells <- if (ncol(mf) > 2L && length(attr(tt, "term.labels")) == 1L &&
                attr(tt, "order") > 1L)
    interaction(mf[-1L], drop = TRUE, sep = ":")

  if (ncol(mf) > 2L && is.null(gCells))
    stop("'formula' should be of the form response ~ group; ",
         "use response ~ a:b for the cells of several grouping variables",
         call. = FALSE)

  # ── 2b. One-sample or two-sample dependent ────────────────────────────────
  if (ncol(mf) == 1L) {

    if (!any(c("one-sample", "two-sample-dependent") %in% allowed))
      stop("'one-sample' / 'two-sample-dependent' design not allowed by 'allowed' argument",
           call. = FALSE)

    if (inherits(response, "Pair")) {

      if (!"two-sample-dependent" %in% allowed)
        stop("'two-sample-dependent' design not allowed by 'allowed' argument",
             call. = FALSE)

      return(.result("two-sample-dependent", mf, response,
                     x = response[, 1L],
                     y = response[, 2L]))
    }

    if (!"one-sample" %in% allowed)
      stop("'one-sample' design not allowed by 'allowed' argument",
           call. = FALSE)

    return(.result("one-sample", mf, response, x = response))
  }

  # ── 2c. numeric ~ numeric ────────────────────────────────────────────────
  # not for cells: numeric components of y ~ a:b are categorical there
  if (is.null(gCells) && is.numeric(mf[[2L]])) {
    if (!"numeric-numeric" %in% allowed)
      stop("right-hand side of 'formula' is numeric, but a ",
           "'numeric-numeric' design is not allowed here; ",
           "supply a grouping factor instead", call. = FALSE)
    return(.result("numeric-numeric", mf, response,
                   x         = response,
                   predictor = mf[[2L]]))   # numeric, never called 'group'
  }

  # ── 2d. Grouped: two-sample or n-sample independent ──────────────────────
  # interaction() yields NA as soon as one component is missing, so a cell
  # with a missing component is excluded like a missing group
  g <- droplevels(factor(if (is.null(gCells)) mf[[2L]] else gCells,
                         exclude = NA))
  k <- nlevels(g)

  # no level survives when the grouping variable is missing throughout, or
  # when 'subset' has filtered out every observation
  if (k == 0L)
    stop("grouping factor has no non-missing levels", call. = FALSE)

  # k == 1: Fallback to one-sample if allowed
  if (k == 1L) {
    if (!"one-sample" %in% allowed)
      stop("grouping factor has only 1 level", call. = FALSE)
    return(.result("one-sample", mf, response, x = response))
  }

  if (k == 2L && !"two-sample-independent" %in% allowed &&
      !"n-sample-independent" %in% allowed)
    stop("grouped design not allowed by 'allowed' argument", call. = FALSE)

  if (k > 2L && !"n-sample-independent" %in% allowed)
    stop("'n-sample-independent' design not allowed by 'allowed' argument",
         call. = FALSE)

  type <- if (k == 2L && "two-sample-independent" %in% allowed)
    "two-sample-independent"
  else
    "n-sample-independent"

  # x and group have full length n and the same shape for k = 2 and k > 2.
  # y is a convenience-only field for the binary case; x + group remains the
  # canonical access path and has identical shape across k. Note that split()
  # drops observations whose group is NA, so y is shorter than x whenever the
  # grouping variable has missing values.
  if (type == "two-sample-independent")
    return(.result(type, mf, response,
                   x     = response,
                   group = g,
                   y     = split(response, g, drop = TRUE)[[2L]]))

  .result(type, mf, response,
          x     = response,
          group = g)
}



#' @description
#' `resolveFormulaFromCall()` is the entry point for a function offering a
#' formula interface: called from its body, it forwards the caller's
#' `formula`, `data` and `subset` to `resolveFormula()` exactly as they were
#' written, so that `subset` keeps its base R semantics (see Details).
#'
#' @details
#' **resolveFormulaFromCall()**
#'
#' Rebuilds the call of the function it is called from - via [match.call()]
#' against that function's definition, which works for an S3 method as well
#' - keeps its `formula`, `data` and `subset` arguments, and evaluates
#' `resolveFormula()` with them in the frame the calling function was called
#' from. This is the forwarding pattern of [boxplot.formula()] and [lm()],
#' written once:
#'
#' \preformatted{
#' plotBox.formula <- function(formula, data, subset, na.action = na.omit, ...) {
#'   r <- bedrock::resolveFormulaFromCall(
#'          allowed   = c("two-sample-independent", "n-sample-independent"),
#'          na.action = na.action)
#'   ...
#' }
#' }
#'
#' `allowed` is passed on as given (omitted: all types). `na.action` should
#' be the calling function's own `na.action` value; it is always passed on,
#' so that the caller's default (typically [na.omit()]) applies rather than
#' the [na.pass()] default of `resolveFormula()`.
#'
#' Requirements for the calling function: its arguments must be named
#' `formula`, `data` and `subset`, and `resolveFormulaFromCall()` must be
#' called directly in its body, not from a nested helper function, since the
#' call is looked up one frame up.
#'
#' @rdname resolveFormula
#' @export
resolveFormulaFromCall <- function(allowed, na.action = na.pass) {

  # the calling function's frame, and its call matched against its own
  # definition (for an S3 method: the method's formals, not the generic's)
  caller <- sys.parent()

  if (caller == 0L)
    stop("resolveFormulaFromCall() must be called from within a function",
         call. = FALSE)

  m <- match.call(definition  = sys.function(caller),
                  call        = sys.call(caller),
                  expand.dots = FALSE)
  m <- m[c(1L, match(c("formula", "data", "subset"), names(m), 0L))]

  if (is.null(m$formula))
    stop("the calling function has no 'formula' argument", call. = FALSE)

  m[[1L]]     <- quote(bedrock::resolveFormula)
  m$na.action <- na.action
  if (!missing(allowed))
    m$allowed <- allowed

  # the frame the calling function was called from: there the arguments were
  # written, and there resolveFormula() takes 'subset' unevaluated
  eval(m, parent.frame(2L))
}
