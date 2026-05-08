
#' Generate Dummy Codes for a Factor
#'
#' Generate a matrix of dummy codes, also known as class indicators, for a
#' factor or class vector.
#'
#' The argument `method` controls the contrast coding. The option `"full"`
#' returns one indicator column for each level of `x`. This full-rank coding is
#' usually redundant for `lm()` and related modelling functions.
#'
#' @param x Factor or vector of classes.
#' @param method Character string specifying the contrast method. One of
#'   `"treatment"`, `"sum"`, `"helmert"`, `"poly"`, or `"full"`.
#'   Abbreviations are accepted.
#' @param base Integer or character string specifying the baseline group for
#'   treatment contrasts.
#' @param levels Optional character vector specifying the possible levels of
#'   `x`. If `NULL`, levels are inferred by `factor(x)`.
#'
#' @return
#' A matrix with dummy codes. The number of rows equals `length(x)`.
#' For `method = "full"`, the number of columns equals the number of levels.
#' Otherwise, the number of columns equals the number of levels minus one.
#'
#' The returned matrix has an attribute `"base"` containing the baseline level
#' for non-full coding, and `NA` for full coding.
#'
#' @seealso `model.frame`, `contrasts`, `stats::contr.treatment`,
#'   `stats::contr.sum`, `stats::contr.helmert`, `stats::contr.poly`
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
#' @family topic.factorHandling
#' @concept factor-handling
#' @concept data-transformation
#' @concept table-manipulation


#' @export
dummy <- function(
    x,
    method = c("treatment", "sum", "helmert", "poly", "full"),
    base = 1,
    levels = NULL
) {
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
  
  rowNames <- if (is.null(names(x))) seq_along(x) else names(x)
  
  if (method == "full") {
    dimnames(res) <- list(rowNames, levels(x))
    attr(res, "base") <- NA_character_
  } else {
    dimnames(res) <- list(rowNames, levels(x)[-base])
    attr(res, "base") <- levels(x)[base]
  }
  
  res
}


