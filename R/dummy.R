
#' Generate Dummy Codes for a Factor
#'
#' Generate a matrix of dummy codes, also known as class indicators, for a
#' factor or class vector.
#'
#' The argument `method` controls the contrast coding. The option `"full"`
#' returns one indicator column for each level of `x`. This full-rank coding is
#' usually redundant for `lm()` and related modelling functions.
#'
#' The `base` argument is only used by `method = "treatment"`. The other
#' contrast types have no freely choosable baseline: `"sum"` implicitly uses
#' the last level as reference, `"helmert"` contrasts each level against the
#' preceding ones, and `"poly"` uses orthogonal polynomials.
#'
#' Column names reflect the semantics of the coding: level names for
#' `"treatment"` (without the baseline), `"full"` (all levels), `"sum"`
#' (without the last level) and `"helmert"` (without the first level);
#' `"poly"` keeps the standard degree labels (`.L`, `.Q`, ...).
#'
#' @param x factor or vector of classes.
#' @param method character string specifying the contrast method. One of
#'   `"treatment"`, `"sum"`, `"helmert"`, `"poly"`, or `"full"`.
#'   Abbreviations are accepted.
#' @param base integer or character string specifying the baseline group.
#'   Only used for `method = "treatment"` (see Details).
#' @param levels optional character vector specifying the possible levels of
#'   `x`. If `NULL`, levels are inferred by `factor(x)`.
#'
#' @return
#' a matrix with dummy codes. The number of rows equals `length(x)`.
#' For `method = "full"`, the number of columns equals the number of levels.
#' Otherwise, the number of columns equals the number of levels minus one.
#'
#' The returned matrix has an attribute `"base"` containing the baseline level
#' for treatment coding, and `NA` otherwise.
#'
#' @seealso [model.frame()], [contrasts()], [stats::contr.treatment()],
#'   [stats::contr.sum()], [stats::contr.helmert()], [stats::contr.poly()]
#'
#' @examples
#' x <- c("red", "blue", "green", "blue", "green", "red", "red", "blue")
#' dummy(x)
#' dummy(x, base = 2)
#' dummy(x, method = "sum")
#'
#' y <- c("Max", "Max", "Max", "Max", "Max", "Bill", "Bill", "Bill")
#' dummy(y)
#' dummy(y, base = "Max")
#' dummy(y, base = "Max", method = "full")
#'
#' # Revert full dummy coding
#' m <- dummy(y, method = "full")
#' apply(m, 1, function(z) colnames(m)[z == 1])
#'
#' # Revert treatment dummy coding
#' m <- dummy(y)
#' apply(
#'   m,
#'   1,
#'   function(z) ifelse(sum(z) == 0, attr(m, "base"), colnames(m)[z == 1])
#' )
#'
#' @family data.recode
#' @concept dummy-coding
#' @concept categorization
#' @export
dummy <- function(x,
                  method = c("treatment", "sum", "helmert", "poly", "full"),
                  base = 1,
                  levels = NULL) {

  x <- if (is.null(levels)) {
    factor(x)
  } else {
    factor(x, levels = levels)
  }

  method <- match.arg(method)

  if (!is.numeric(base)) {
    base <- match(base, levels(x))
  }

  if (length(base) != 1L || is.na(base) || base < 1L || base > nlevels(x)) {
    stop("Argument 'base' must identify exactly one valid level of 'x'.")
  }

  res <- switch(
    method,
    treatment = contr.treatment(n = nlevels(x), base = base)[x, , drop = FALSE],
    sum       = contr.sum(n = nlevels(x))[x, , drop = FALSE],
    helmert   = contr.helmert(n = nlevels(x))[x, , drop = FALSE],
    poly      = contr.poly(n = nlevels(x))[x, , drop = FALSE],
    full      = diag(nlevels(x))[x, , drop = FALSE]
  )

  res <- as.matrix(res)

  rownames(res) <- if (is.null(names(x))) seq_along(x) else names(x)

  # 'base' has a meaning for treatment coding only; the other contrast
  # types have fixed reference semantics (see Details)
  attr(res, "base") <- NA_character_

  switch(
    method,
    treatment = {
      colnames(res) <- levels(x)[-base]
      attr(res, "base") <- levels(x)[base]
    },
    full    = colnames(res) <- levels(x),
    sum     = colnames(res) <- levels(x)[-nlevels(x)],
    helmert = colnames(res) <- levels(x)[-1L]
    # poly: keep the standard .L/.Q/... column names from contr.poly()
  )

  res
}
