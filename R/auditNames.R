## ============================================================
## Naming audit (design rules §3.1 - §3.5)
## ============================================================
##
## The naming rules are checkable from the namespace alone: exported
## names, formals, default values. This file holds the check, so that each
## package of the suite can run it as a test instead of relying on a
## review. It reads the installed package and never its sources.
##
## What it cannot see is spelled out in the help page: a word boundary
## that was never capitalised ("maxlen") is one word to a machine.


# names every R user reads without thinking (§3.4, rule 1)
#' @noRd
.auditSharedNames <- c(
  "na.rm", "na.action", "conf.level", "sig.level", "alternative", "mu",
  "paired", "var.equal", "lower.tail", "log.p", "ncp", "df",
  "decreasing", "na.last", "ties.method",
  # a covariance matrix or a function computing one: the trailing dot is
  # the convention of lmtest, sandwich and car, forwarded or not
  "vcov."
)

# inherited names, kept only where the value is passed on (§3.4, rule 2)
#' @noRd
.auditForwardedNames <- c(
  "use.names", "ignore.case", "strict.width", "dig.lab", "include.lowest",
  "ordered_result", "list.len", "all.x", "all.y", "by.x", "by.y",
  "p.adjust.method", "useNA", "MARGIN", "width.cutoff", "all.inside",
  "row.names", "check.names", "big.mark", "decimal.mark", "print.gap",
  "row.vars", "col.vars",
  # abind
  "rev.along", "new.names", "force.array", "make.names", "use.anon.names",
  "use.first.dimnames", "hier.names", "use.dnns",
  # rpart.plot
  "fallen.leaves", "clip.facs", "clip.right.labs", "box.palette",
  "shadow.col"
)

# formals that are capitals by convention (§3.3 H, §13.6): the function to
# apply, the number of resamples, and the single letters that are the
# mathematical symbol - population size, covariance and design matrix
#' @noRd
.auditUpperNames <- c("FUN", "R", "N", "S", "X")

# the closed list of §3.2; GCD and LCM count as whole names only
#' @noRd
.auditAbbreviations <- c("CI", "QQ", "XY", "ECDF", "SE", "AD")
#' @noRd
.auditWholeNames    <- c("GCD", "LCM")

# names the rules replace by another one (§13.13), with the name to use
#' @noRd
.auditReplaced <- c(
  alpha = "sig.level",
  seed  = "none - the caller uses set.seed()",
  pkg   = "package",
  fun   = "FUN",
  dat   = "data",
  grp   = "groups",
  w     = "weights",
  level = "another name - it reads like levels",
  color = "col",
  cols  = "col",
  obs   = "ref",
  resp  = "ref"
)

# enum values that come with the function they are passed to (§3.4)
#' @noRd
.auditInheritedValues <- c(
  "two.sided", "data.frame", "closest.topleft", "TM",
  "pairwise.complete.obs", "complete.obs", "all.obs", "na.or.complete",
  stats::p.adjust.methods
)

# exported with a leading dot on purpose (§3.1.1, §9.2)
#' @noRd
.auditFrameworkHelpers <- c(
  ".withGraphicsState", ".applyParFromDots", ".resolveTitle", ".marTop",
  ".marginLines", ".drawGrid", ".drawBox"
)

# The dplyr part of the collision set: its exports that are a single word,
# since no name of the suite contains an underscore. Kept as a list rather
# than read from the installed package, so that the audit gives the same
# answer on every machine, with or without dplyr and whatever its version.
#' @noRd
.auditDplyrNames <- c(
  "across", "arrange", "between", "coalesce", "collapse", "collect",
  "combine", "compute", "contains", "count", "cumall", "cumany",
  "cummean", "desc", "distinct", "do", "everything", "explain", "filter",
  "first", "funs", "glimpse", "groups", "id", "ident", "intersect", "lag",
  "last", "lead", "location", "lst", "matches", "mutate", "n", "near",
  "nth", "ntile", "pick", "pull", "recode", "reframe", "relocate",
  "rename", "rowwise", "select", "setdiff", "setequal", "slice", "src",
  "summarise", "summarize", "tally", "tbl", "tibble", "transmute",
  "tribble", "ungroup", "union", "vars", "where"
)


# camelCase words of a name; an abbreviation of the closed list is one
# word, any other run of capitals is returned as such and judged later
#' @noRd
.auditWords <- function(x) {

  pat <- paste0(
    "(", paste(.auditAbbreviations[order(-nchar(.auditAbbreviations))],
               collapse = "|"), ")(?![a-z]{2})",
    "|[A-Z]+(?![a-z])|[A-Z][a-z0-9]*|[a-z0-9]+"
  )

  regmatches(x, gregexpr(pat, x, perl = TRUE))[[1L]]
}


# Is the formal handed on somewhere in the body? Either under its own
# name, f(ignore.case = ...), or as the bare value of a named argument,
# p.adjust(p, method = p.adjust.method) - the form pairwise.t.test() uses
# for the very name it established.
#' @noRd
.auditForwards <- function(expr, name) {

  if (is.call(expr)) {

    nms <- names(expr)
    if (!is.null(nms)) {

      if (name %in% nms[-1L])
        return(TRUE)

      for (i in which(nzchar(nms)))
        if (i > 1L && is.symbol(expr[[i]]) &&
            identical(as.character(expr[[i]]), name))
          return(TRUE)
    }

    for (i in seq_along(expr)) {
      e <- expr[[i]]
      if (!missing(e) && is.language(e) && .auditForwards(e, name))
        return(TRUE)
    }
  }

  FALSE
}


# collision set of §3.2
#' @noRd
.auditCollisionSet <- function() {

  unique(c(unlist(lapply(c("base", "stats", "graphics", "utils"),
                         getNamespaceExports), use.names = FALSE),
           .auditDplyrNames))
}


# Is the name taken (§3.2)? Either exactly, or by a name that differs in
# nothing but the case of the whole word: 'iqr' is taken by IQR(). An
# internal capital makes a name of its own - combN() is not combn().
#' @noRd
.auditTaken <- function(name, set) {

  name %in% set ||
    ((name == tolower(name) || name == toupper(name)) &&
       tolower(name) %in% tolower(set))
}


#' Audit the Names of a Package Against the Design Rules
#'
#' Checks the exported functions of an installed package, their arguments
#' and the values of their enumerated arguments against the naming rules
#' of the suite. Meant to run as a test in every package, so that a name
#' which breaks a rule is found when it is introduced and not in a review.
#'
#' @param package character string, the name of an installed package.
#' @param exceptions character vector of findings to accept, each written
#'   as the `key` of the result: `"fun"` for a function name,
#'   `"fun(arg)"` for an argument and `"fun(arg = \"value\")"` for an
#'   enumerated value. An exception that matches nothing is reported in
#'   the attribute `"unused"`, so that the list cannot outlive its
#'   reasons.
#'
#' @details
#' The rules checked, with the label used in the column `rule`:
#'
#' \describe{
#'   \item{`camelCase`}{exported functions and arguments are written in
#'     lowerCamelCase. A dot is kept in S3 methods, in coercion generics
#'     `as.<Class>` that really dispatch, and in argument names that are
#'     part of the shared vocabulary (`na.rm`, `conf.level`,
#'     `sig.level`, ...).}
#'   \item{`acronym`}{only `CI`, `QQ`, `XY`, `ECDF`, `SE` and `AD` keep
#'     their capitals inside a name, `GCD` and `LCM` as whole names. Every
#'     other acronym is written like a word: `Rgb`, `Url`, `Html`. As
#'     arguments, `FUN`, `R`, `N`, `S` and `X` are capitals by
#'     convention.}
#'   \item{`collision`}{a function whose name is taken by `base`,
#'     `stats`, `graphics`, `utils` or `dplyr` carries the suffix `X`.
#'     A name counts as taken if it exists there, or if it differs from
#'     one only in the case of the whole word (`iqr` against `IQR()`).
#'     An internal capital makes a name of its own: `combN()` is not
#'     `combn()`.}
#'   \item{`suffixX`}{and no function carries it without such a
#'     collision.}
#'   \item{`dotExport`}{no function is exported with a leading dot,
#'     the graphics helpers of the plotting framework excepted.}
#'   \item{`forwarded`}{an argument name taken over from another package
#'     (`ignore.case`, `useNA`, `all.inside`, ...) is kept only where the
#'     value is passed on to another function.}
#'   \item{`replaced`}{names the rules have replaced: `alpha`
#'     (`sig.level`), `seed` (the caller uses [set.seed()]), `pkg`
#'     (`package`), `fun` in an exported function (`FUN`), `dat`
#'     (`data`), `grp` (`groups`), `w` (`weights`), `level`, `color` and
#'     `cols` (`col`), `obs` and `resp` (`ref`), the prefix `num` for a
#'     count (`n`), the suffix `Args` for a list of arguments, and `g` and
#'     `horizontal` in plot functions (`groups`, `horiz`).}
#'   \item{`enumValue`}{the values of an enumerated argument are lower
#'     case, words joined by a hyphen (`"wald-cc"`). Values that belong to
#'     the function they are passed on to, such as `"two.sided"` or the
#'     names in [p.adjust.methods], are left alone. A value that names an
#'     element of the result (`which = "tauB"` returning `$tauB`) follows
#'     the rule for results instead; the audit cannot see that and
#'     reports it, so such values are listed as exceptions.}
#' }
#'
#' Arguments of an S3 method that its generic defines are not checked:
#' `print.foo(x, ...)` and `predict.foo(object, newdata, ...)` are given
#' by the generic. Re-exported functions are skipped altogether.
#'
#' What the audit cannot see is a word boundary that was never written:
#' `maxlen` and `nlow` are single lower case words to it. Such names are
#' found by reading, not by this function.
#'
#' @return a data frame with one row per finding and the columns
#'   `package`, `fun`, `arg` (`NA` for a finding on the function name),
#'   `rule`, `detail` and `key`, the form in which the finding is named
#'   in `exceptions`. It has no rows if the package complies. The
#'   attribute `"unused"` holds the exceptions that matched no finding.
#'
#' @examples
#' res <- auditNames("bedrock")
#' res[, c("fun", "arg", "rule", "detail")]
#'
#' # as a test: no findings beyond the documented exceptions, and no
#' # exception without a finding
#' accepted <- c("isNA" = "NA is R's own constant, not an acronym")
#' res <- auditNames("bedrock", exceptions = names(accepted))
#' attr(res, "unused")
#'
#' @seealso [funArgs()], [funList()]
#' @family pkg.funinfo
#' @concept introspection
#' @concept programming
#' @export
auditNames <- function(package, exceptions = NULL) {

  checkString(package)

  if (!is.null(exceptions) && !is.character(exceptions))
    stop("'exceptions' must be a character vector or NULL", call. = FALSE)

  ns        <- asNamespace(package)
  exports   <- sort(getNamespaceExports(ns))
  s3        <- getNamespaceInfo(ns, "S3methods")
  s3Names   <- if (length(s3)) paste(s3[, 1L], s3[, 2L], sep = ".")
               else character(0)
  collision <- .auditCollisionSet()

  rows <- list()
  add <- function(fun, arg, rule, detail, value = NULL) {
    key <- if (is.na(arg)) fun
           else if (is.null(value)) sprintf("%s(%s)", fun, arg)
           else sprintf("%s(%s = \"%s\")", fun, arg, value)
    rows[[length(rows) + 1L]] <<- data.frame(
      package = package, fun = fun, arg = arg, rule = rule,
      detail = detail, key = key, stringsAsFactors = FALSE)
  }

  own <- function(f)
    is.function(f) && !is.primitive(f) &&
      identical(environmentName(topenv(environment(f))), package)


  # --- function names -------------------------------------------------

  funs <- list()

  for (nm in exports) {

    f <- get0(nm, envir = ns, inherits = FALSE)

    # data, re-exports and operators carry no name of ours to check
    if (!own(f) || grepl("^%.*%$", nm))
      next

    isMethod <- nm %in% s3Names
    funs[[nm]] <- list(f = f, generic = if (isMethod) s3[match(nm, s3Names), 1L])

    if (isMethod)
      next

    base <- sub("<-$", "", nm)

    if (startsWith(base, ".")) {
      if (!base %in% .auditFrameworkHelpers)
        add(nm, NA, "dotExport",
            "exported with a leading dot; only the graphics framework helpers are")
      next
    }

    if (base %in% .auditWholeNames)
      next

    if (grepl(".", base, fixed = TRUE)) {

      generic <- any(vapply(
        all.names(body(f)), identical, logical(1L), "UseMethod"))

      if (!(startsWith(base, "as.") && generic))
        add(nm, NA, "camelCase",
            if (startsWith(base, "is."))
              "a predicate is written isXxx, without the dot"
            else if (startsWith(base, "as."))
              "not a generic: a plain coercion function is written asXxx"
            else
              "a dot in an exported name is reserved for S3 methods")

    } else if (!grepl("^[a-z][A-Za-z0-9]*$", base)) {

      add(nm, NA, "camelCase", "not lowerCamelCase")

    } else {

      bad <- grep("^[A-Z]{2,}$", .auditWords(base), value = TRUE)
      bad <- setdiff(bad, .auditAbbreviations)
      if (length(bad))
        add(nm, NA, "acronym",
            sprintf("%s is not on the list of abbreviations; write it as a word",
                    paste(bad, collapse = ", ")))
    }

    hasX <- grepl("[a-z0-9]X$", base)

    if (!hasX && .auditTaken(base, collision) &&
        !(startsWith(base, "as.") || startsWith(base, "is.")))
      add(nm, NA, "collision",
          "the name is taken by base, stats, graphics, utils or dplyr; add the suffix X")

    if (hasX && !.auditTaken(sub("X$", "", base), collision))
      add(nm, NA, "suffixX",
          "carries the suffix X although the name without it is free")
  }

  # methods registered but not exported belong to the public interface too
  for (nm in setdiff(s3Names, names(funs))) {
    f <- get0(nm, envir = ns, inherits = FALSE)
    if (own(f))
      funs[[nm]] <- list(f = f, generic = s3[match(nm, s3Names), 1L])
  }


  # --- arguments and enum values --------------------------------------

  for (nm in names(funs)) {

    f     <- funs[[nm]]$f
    fmls  <- formals(f)
    given <- character(0)

    # what the generic defines is not the method's choice
    if (!is.null(funs[[nm]]$generic)) {
      # args() is NULL for the primitives without a closure form ("[")
      g <- get0(funs[[nm]]$generic, envir = ns, mode = "function")
      g <- if (!is.null(g)) args(g)
      if (is.function(g))
        given <- names(formals(g))
    }

    isPlot <- grepl("^plot[A-Z]", nm)

    # a formal without a default holds the empty symbol, which must not
    # be evaluated
    noDefault <- vapply(fmls, function(z)
      is.symbol(z) && !nzchar(as.character(z)), logical(1L))

    for (a in setdiff(names(fmls), c("...", given))) {

      if (a %in% .auditSharedNames) {

        # shared vocabulary: nothing to check

      } else if (a %in% .auditForwardedNames) {

        if (!.auditForwards(body(f), a))
          add(nm, a, "forwarded",
              "inherited name, but the value is not passed on; use lowerCamelCase")

      } else if (a %in% names(.auditReplaced) &&
                 !(a == "fun" && startsWith(nm, "."))) {

        add(nm, a, "replaced",
            sprintf("use %s", .auditReplaced[[a]]))

      } else if (grepl("^num[A-Z]", a)) {

        add(nm, a, "replaced", "a count takes the prefix n, not num")

      } else if (grepl("[a-z]Args$", a)) {

        add(nm, a, "replaced",
            "a list of arguments is named after what it controls, without the suffix Args")

      } else if (isPlot && a %in% c("g", "horizontal")) {

        add(nm, a, "replaced",
            sprintf("use %s", c(g = "groups", horizontal = "horiz")[[a]]))

      } else if (a %in% .auditUpperNames) {

        # capitals by convention

      } else if (!grepl("^[a-z][A-Za-z0-9]*$", a)) {

        add(nm, a, "camelCase", "not lowerCamelCase")

      } else {

        bad <- grep("^[A-Z]{2,}$", .auditWords(a), value = TRUE)
        bad <- setdiff(bad, .auditAbbreviations)
        if (length(bad))
          add(nm, a, "acronym",
              sprintf("%s is not on the list of abbreviations; write it as a word",
                      paste(bad, collapse = ", ")))
      }

      # enumerated values: c("a", "b", ...) as the default
      if (noDefault[[a]])
        next

      d <- fmls[[a]]
      if (is.call(d) && identical(d[[1L]], quote(c)) && length(d) > 2L &&
          all(vapply(as.list(d)[-1L], is.character, logical(1L)))) {

        vals <- unlist(as.list(d)[-1L])
        bad  <- vals[!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", vals) &
                       !vals %in% .auditInheritedValues & nzchar(vals)]

        for (v in bad)
          add(nm, a, "enumValue",
              "enumerated values are lower case, words joined by a hyphen",
              value = v)
      }
    }
  }


  # --- result ----------------------------------------------------------

  res <- if (length(rows)) do.call(rbind, rows)
         else data.frame(package = character(0), fun = character(0),
                         arg = character(0), rule = character(0),
                         detail = character(0), key = character(0),
                         stringsAsFactors = FALSE)

  # as.character(): setdiff(NULL, ...) is NULL, not character(0)
  unused <- setdiff(as.character(exceptions), res$key)
  res    <- res[!res$key %in% exceptions, , drop = FALSE]
  rownames(res) <- NULL

  attr(res, "unused") <- unused

  res
}
