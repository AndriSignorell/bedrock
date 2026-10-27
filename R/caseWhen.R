#' Recode Values by Ordered Conditions
#'
#' Vectorized conditional selection: the first matching rule wins.
#'
#' @param ... two-sided formulas of the form `condition ~ value`.
#'   Conditions must be plain logical vectors. Values must be plain logical,
#'   integer, double, complex, or character vectors. Classed objects (including
#'   factors and dates), arrays, and lists are not supported.
#' @param default value for elements without a match. `NULL` (the default)
#'   supplies a missing value of the common result type.
#' @param na handling of missing conditions: `"propagate"` (default) resolves
#'   an element to a missing value when its first unresolved condition is NA;
#'   `"skip"` treats missing conditions as FALSE.
#'
#' @details
#' Unlike `dplyr::case_when()`, which treats missing conditions as FALSE,
#' this function defaults to propagating NA, as in nested [ifelse()] calls.
#' Use `na = "skip"` to skip missing conditions instead.
#'
#' All conditions and values are evaluated once in their formula environments,
#' including values of unused rules. This is ordered selection, not lazy or
#' elementwise short-circuit evaluation. Errors and warnings in unused
#' expressions therefore still occur.
#'
#' All inputs (including `default`) must have length one or the common output
#' length. Non-scalar lengths must agree; zero-length vectors are supported
#' but cannot be combined with lengths greater than one. All-scalar inputs
#' produce length one. Names on inputs are discarded.
#'
#' Numeric types are promoted in the order logical, integer, double, complex.
#' Character values cannot be mixed with numeric or non-missing logical
#' values. All-missing logical vectors are treated as unspecified missing
#' values and may accompany any supported type. Every rule participates in
#' length and type validation, even if it never matches.
#'
#' Factors, dates, and other classed vectors are deliberately unsupported
#' in this version; support may be added later with explicit class and
#' factor-level compatibility rules.
#'
#' @family data.recode
#' @concept categorization
#' @concept missing-value
#' @seealso [ifelse()], \code{dplyr::case_when()}
#' @return An unnamed atomic vector of the common length and type.
#' @export
#' @examples
#' x <- c(-1, 0, 2, NA_real_)
#' caseWhen(x < 0 ~ "negative", x == 0 ~ "zero", default = "positive")
#' caseWhen(x < 0 ~ "negative", default = "other", na = "skip")
#' caseWhen(c(TRUE, FALSE, NA) ~ 1L, default = 0L)
caseWhen <- function(..., default = NULL, na = c("propagate", "skip")) {
  na <- match.arg(na)
  rules <- list(...)
  if (!length(rules))
    stop("Supply at least one two-sided formula.", call. = FALSE)
  if (any(nzchar(names(rules))))
    stop("Rules in '...' must be unnamed; check argument names.", call. = FALSE)

  conditions <- values <- vector("list", length(rules))
  plain <- function(x) {
    is.atomic(x) && !is.null(x) && !is.object(x) &&
      all(names(attributes(x)) %in% "names")
  }
  supported <- c("logical", "integer", "double", "complex", "character")
  isSupported <- function(x) plain(x) && typeof(x) %in% supported
  for (i in seq_along(rules)) {
    rule <- rules[[i]]
    if (!inherits(rule, "formula") || length(rule) != 3L)
      stop(gettextf("Rule %d must be a two-sided formula.", i),
           domain = NA, call. = FALSE)
    env <- environment(rule)
    if (is.null(env))
      stop(gettextf("Rule %d has no formula environment.", i),
           domain = NA, call. = FALSE)
    condition <- eval(rule[[2L]], envir = env)
    if (!plain(condition) || !is.logical(condition))
      stop(gettextf("Condition %d must be a plain logical vector.", i),
           domain = NA, call. = FALSE)
    conditions[[i]] <- condition
    value <- eval(rule[[3L]], envir = env)
    if (!isSupported(value))
      stop(gettextf("Value %d must be a supported plain atomic vector.", i),
           domain = NA, call. = FALSE)
    values[[i]] <- value
  }
  if (!is.null(default)) {
    if (!isSupported(default))
      stop("'default' must be NULL or a supported plain atomic vector.",
           call. = FALSE)
    values[[length(values) + 1L]] <- default
  }

  sizes <- lengths(c(conditions, values))
  non_scalar <- unique(sizes[sizes != 1L])
  if (length(non_scalar) > 1L)
    stop("All non-scalar lengths must agree (no partial recycling).",
         call. = FALSE)
  n <- if (length(non_scalar)) non_scalar else 1L

  types <- vapply(values, typeof, character(1L))
  unspecified <- vapply(values, function(x) is.logical(x) && all(is.na(x)),
                        logical(1L))
  concrete <- types[!unspecified]
  if ("character" %in% concrete && any(concrete != "character"))
    stop("Character and numeric/logical values cannot be mixed.", call. = FALSE)
  type <- if (length(concrete)) supported[max(match(concrete, supported))]
          else "logical"
  # NA index yields the canonical missing value of the type; as.vector(NA,
  # "complex") gives NA+0i since R 4.4.0, not NA_complex_
  missing_value <- vector(type, 1L)[NA_integer_]
  cast <- function(x) {
    y <- as.vector(x, mode = type)
    # same reason: a missing input must become NA_complex_, not NA+0i
    y[is.na(x)] <- missing_value
    y
  }
  recycle <- function(x) if (length(x) == 1L) rep(x, length.out = n) else x
  out <- if (is.null(default)) rep(missing_value, length.out = n)
         else recycle(cast(default))
  unresolved <- rep(TRUE, length.out = n)
  for (i in seq_along(rules)) {
    condition <- recycle(conditions[[i]])
    if (na == "propagate") {
      unknown <- unresolved & is.na(condition)
      out[unknown] <- missing_value
      unresolved[unknown] <- FALSE
    }
    take <- unresolved & !is.na(condition) & condition
    value <- cast(values[[i]])
    if (length(value) == 1L) out[take] <- value
    else out[take] <- value[take]
    unresolved[take] <- FALSE
  }
  unname(out)
}
