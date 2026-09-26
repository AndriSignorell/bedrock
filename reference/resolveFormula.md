# Parse and Classify a Model Formula

Parses a model formula, builds the model frame and classifies the
resulting design into one of seven dependency structures. The pieces of
the design are returned under a fixed set of names, so that every
function offering a formula interface can share one entry point instead
of re-implementing the parsing, the `subset` handling and the
distinction between a grouping factor, a numeric predictor and a
blocking variable.

`resolveFormulaFromCall()` is the entry point for a function offering a
formula interface: called from its body, it forwards the caller's
`formula`, `data` and `subset` to `resolveFormula()` exactly as they
were written, so that `subset` keeps its base R semantics (see Details).

## Usage

``` r
resolveFormula(
  formula,
  data,
  subset,
  na.action = na.pass,
  allowed = c("one-sample", "two-sample-independent", "two-sample-dependent",
    "n-sample-independent", "n-sample-dependent", "numeric-numeric", "regression")
)

resolveFormulaFromCall(allowed, na.action = na.pass)
```

## Arguments

- formula:

  a two-sided model formula. Supported forms are:

  `y ~ 1`

  :   one-sample design.

  `Pair(x, y) ~ 1`

  :   two-sample dependent (paired).
      [`Pair()`](https://rdrr.io/r/stats/Pair.html) constructs a
      two-column matrix of paired observations.

  `y ~ g`

  :   two-sample or n-sample independent group comparison.

  `y ~ a:b`

  :   independent group comparison of the cells of several grouping
      variables, combined into one grouping factor. `y ~ a + b` is not a
      grouped design: it is `regression` if allowed, else an error.

  `y ~ x`, `x` numeric

  :   numeric-numeric (correlation, simple regression).

  `y ~ x1 + x2 + ...`

  :   general regression.

  `y ~ trt | block`

  :   n-sample dependent (blocked design).

- data:

  an optional data frame containing the variables in `formula`. A matrix
  is coerced to a data frame.

- subset:

  an optional expression indicating the observations to use, evaluated
  in `data` as in
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html)
  (`subset = len > 10`), or an index vector. See Details.

- na.action:

  a function specifying how missing values are handled, defaults to
  [`na.pass()`](https://rdrr.io/r/stats/na.fail.html).

- allowed:

  a character vector restricting which design types are accepted, any
  combination of `"one-sample"`, `"two-sample-independent"`,
  `"two-sample-dependent"`, `"n-sample-independent"`,
  `"n-sample-dependent"`, `"numeric-numeric"` and `"regression"`. The
  values are matched exactly, an unknown one is an error rather than
  being ignored. A further error is raised if the detected type is not
  among the allowed ones. Defaults to all types.

## Value

a named list containing at least:

- type:

  character, one of the design types listed above.

- mf:

  the [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) the
  design was read from.

- rows:

  integer, the positions of the retained observations in the original
  data, or `NULL` if they cannot be determined.

- response:

  the left-hand side of the formula.

- dataName:

  character, the deparsed formula, for use as the `data.name` of an
  `htest` object.

plus the design-specific components described under Details.

## Details

**Design types**

- `one-sample`:

  `y ~ 1`, as in the one-sample t-test or the one-sample Wilcoxon test.

- `two-sample-independent`:

  `y ~ g` with two groups, as in the two-sample t-test or the Wilcoxon
  rank-sum test.

- `two-sample-dependent`:

  `Pair(x, y) ~ 1`, as in the paired t-test or the Wilcoxon signed-rank
  test.

- `n-sample-independent`:

  `y ~ g` with more than two groups, as in the analysis of variance or
  the Kruskal-Wallis test.

- `n-sample-dependent`:

  `y ~ trt | block`, as in a repeated-measures analysis of variance or
  the Friedman test.

- `numeric-numeric`:

  `y ~ x` with a numeric right-hand side, as in correlation or simple
  regression.

- `regression`:

  a general regression formula with one or more predictors.

**Type detection**

The type follows from the shape of the formula and from the class of the
right-hand side variable, not from `allowed`. `allowed` only decides
whether the detected type is accepted, with four exceptions worth
knowing.

1.  A grouping factor carrying a single level is reported as
    `one-sample` if that type is allowed.

2.  A two-group design is reported as `n-sample-independent` if
    `"two-sample-independent"` is not among the allowed types. Together
    with the previous rule this lets a caller that treats every group
    count alike allow one type only.

3.  `allowed = "regression"` on its own forces the `regression` type for
    every formula, including `y ~ 1` and `y ~ g`, with the exception of
    the blocked syntax `y ~ trt | block`, which is always
    `n-sample-dependent` and therefore an error then. This is the entry
    point for model-fitting callers, which interpret the right-hand side
    themselves. Duplicates in `allowed` are ignored, so
    `c("regression", "regression")` behaves the same.

4.  Otherwise `regression` is reported only for more than one right-hand
    side variable. With `allowed` containing both `"regression"` and
    `"numeric-numeric"`, `y ~ x` is therefore `numeric-numeric` while
    `y ~ x1 + x2` is `regression`.

**Cells of several grouping variables**

A right-hand side consisting of a single interaction term, `y ~ a:b` (or
`y ~ a:b:c`), is the explicit request for the cells of these variables.
They are combined into one grouping factor via
[`interaction()`](https://rdrr.io/r/base/interaction.html), with levels
such as `"OJ:0.5"`; numeric components are treated as categorical. The
design is then classified like `y ~ g`, by the number of non-empty
cells.

Unlike [`boxplot()`](https://rdrr.io/r/graphics/boxplot.html),
`y ~ a + b` is *not* read as cells: additive terms are not an
interaction, as in [`lm()`](https://rdrr.io/r/stats/lm.html). Such a
formula, like `y ~ a * b`, is `regression` if that type is allowed, and
an error otherwise. If `"regression"` is allowed, it also takes
precedence over the cell reading of `y ~ a:b`, which a model-fitting
caller interprets as an interaction term.

The distinction rests on the terms of the formula: the model frame holds
the variables `a` and `b` in both cases and cannot tell the two apart.

[`offset()`](https://rdrr.io/r/stats/offset.html) is only accepted for
the `regression` design and an error otherwise: an offset column is part
of the model frame without being a term, and would pass for a grouping
variable or a numeric predictor.

**Field naming contract (binding across all types)**

- `response` is present for every type and always holds the left-hand
  side of the formula. It is the one field a caller can rely on without
  branching on `type`.

- `x` is an alias of `response` for the types that are conventionally
  described in terms of a sample rather than a model (`one-sample`,
  `two-sample-independent`, `n-sample-independent`, `numeric-numeric`).
  The exception is `two-sample-dependent`: there `response` is the whole
  [`Pair()`](https://rdrr.io/r/stats/Pair.html) matrix, while `x` and
  `y` are its first and second column.

- `group` is reserved for a categorical, factor-coercible variable of
  length `n` (the full sample) that splits the response into groups. It
  is never pre-split and never used for a continuous variable. `x` and
  `group` have an identical shape for `two-sample-independent` and for
  `n-sample-independent`, so that a caller can use `split(r$x, r$group)`
  uniformly, without branching on the number of groups. For `y ~ a:b`,
  `group` holds the combined cell factor.

- `predictor` is used for a continuous, numeric right-hand side variable
  (`numeric-numeric`), never `group`.

- `treatment` is used for the explanatory variable of a blocked design
  (`n-sample-dependent`), as distinct from `block`, the stratification
  factor. Neither is ever called `group`.

- `y`, where present, is a convenience field only, holding the second
  group of a two-sample design or the second paired vector. It is never
  needed for correct use: `x` and `group` (or `x` and `predictor`, or
  `treatment` and `block`) are always sufficient and are the canonical
  access path.

- `rows` is present for every type and holds the positions of the
  retained observations in the original data, after `subset` and after
  `na.action`. It is the handle for synchronising an external vector (an
  ordering variable, weights) with the model frame: `z <- z[r$rows]`. It
  is `NULL` in the rare case where the row names of the model frame
  cannot be matched back, e.g. when a numeric `subset` selects a row
  twice.

- `terms` is returned for the `regression` type. Build the design matrix
  from it, `model.matrix(r$terms, r$mf)`, never from the original
  formula: the columns of a model frame are named after the deparsed
  expressions (`"log(x)"`), so re-evaluating the formula against the
  model frame fails for every transformed term.

**Missing values**

Missing values are left to `na.action` and are not touched otherwise, so
with the default [`na.pass()`](https://rdrr.io/r/stats/na.fail.html)
they reach the caller untouched. The one exception is the grouping
factor of an independent design, where empty and missing levels are
dropped before the groups are counted. A grouping variable that is
missing throughout leaves no level at all and is an error. A cell of
`y ~ a:b` is missing as soon as one of its components is.

Rows removed by `na.action` are recorded in `attr(r$mf, "na.action")`,
but those indices are relative to the already subsetted frame. To align
an external vector with the model frame use `rows`, which accounts for
`subset` and `na.action` at once.

**subset handling**

`subset` is evaluated as in base R: it is taken unevaluated, like
[`lm()`](https://rdrr.io/r/stats/lm.html) does, by handing this
function's own call on to
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html), which
evaluates the expression in `data`, with `environment(formula)` as the
enclosure. `resolveFormula(y ~ g, df, subset = g == "A")` therefore
works exactly like `boxplot(y ~ g, df, subset = g == "A")`, and a quoted
expression fails in both.

A wrapper function forwards its arguments the same way base R does (see
[`boxplot.formula()`](https://rdrr.io/r/graphics/boxplot.html)): it
rebuilds its own call and evaluates it in its caller's frame, so that
`subset` reaches `resolveFormula()` unevaluated.
`resolveFormulaFromCall()` does exactly this and is the entry point to
use in a formula method:


    myFun <- function(formula, data, subset, na.action = na.omit, ...) {
      r <- resolveFormulaFromCall(
             allowed   = c("two-sample-independent", "n-sample-independent"),
             na.action = na.action)
      ...
    }

Passing a captured expression on by value
(`resolveFormula(formula, data, subset = substitute(subset))`) does not
work, just as it does not for
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html) itself.

**Return components by type**

Every return value starts with `type`, `mf`, `rows` and `response`, in
that order, followed by the design-specific components below, and ends
with `dataName`:

- `one-sample`:

  `x`

- `two-sample-independent`:

  `x`, `group`, `y` (convenience: the second group)

- `two-sample-dependent`:

  `x`, `y`

- `n-sample-independent`:

  `x`, `group`

- `n-sample-dependent`:

  `treatment`, `block`

- `numeric-numeric`:

  `x`, `predictor`

- `regression`:

  `terms`

**resolveFormulaFromCall()**

Rebuilds the call of the function it is called from - via
[`match.call()`](https://rdrr.io/r/base/match.call.html) against that
function's definition, which works for an S3 method as well

- keeps its `formula`, `data` and `subset` arguments, and evaluates
  `resolveFormula()` with them in the frame the calling function was
  called from. This is the forwarding pattern of
  [`boxplot.formula()`](https://rdrr.io/r/graphics/boxplot.html) and
  [`lm()`](https://rdrr.io/r/stats/lm.html), written once:


    plotBox.formula <- function(formula, data, subset, na.action = na.omit, ...) {
      r <- bedrock::resolveFormulaFromCall(
             allowed   = c("two-sample-independent", "n-sample-independent"),
             na.action = na.action)
      ...
    }

`allowed` is passed on as given (omitted: all types). `na.action` should
be the calling function's own `na.action` value; it is always passed on,
so that the caller's default (typically
[`na.omit()`](https://rdrr.io/r/stats/na.fail.html)) applies rather than
the [`na.pass()`](https://rdrr.io/r/stats/na.fail.html) default of
`resolveFormula()`.

Requirements for the calling function: its arguments must be named
`formula`, `data` and `subset`, and `resolveFormulaFromCall()` must be
called directly in its body, not from a nested helper function, since
the call is looked up one frame up.

## See also

[`model.frame()`](https://rdrr.io/r/stats/model.frame.html),
[`Pair()`](https://rdrr.io/r/stats/Pair.html),
[`resolveGroups()`](resolveGroups.md)

Other data.resolve: [`resolveContingency()`](resolveContingency.md),
[`resolveGroups()`](resolveGroups.md)

## Examples

``` r
set.seed(1)
df <- data.frame(
  y   = rnorm(30, 50, 10),
  g2  = rep(c("A", "B"), 15),
  g3  = rep(c("A", "B", "C"), 10),
  trt = rep(c("T1", "T2", "T3"), 10),
  blk = rep(1:10, 3)
)

# one-sample
resolveFormula(y ~ 1, data = df)$type
#> [1] "one-sample"
## [1] "one-sample"

# two-sample independent: x and group have full length, the same shape
# as for more than two groups
r2 <- resolveFormula(y ~ g2, data = df,
                     allowed = c("two-sample-independent",
                                 "n-sample-independent"))
r2$type
#> [1] "two-sample-independent"
## [1] "two-sample-independent"
length(r2$x) == length(r2$group)
#> [1] TRUE
## [1] TRUE

# n-sample independent
resolveFormula(y ~ g3, data = df,
               allowed = "n-sample-independent")$type
#> [1] "n-sample-independent"
## [1] "n-sample-independent"

# cells of two grouping variables: a:b, not a + b
r3 <- resolveFormula(y ~ g2:g3, data = df,
                     allowed = "n-sample-independent")
levels(r3$group)
#> [1] "A:A" "B:A" "A:B" "B:B" "A:C" "B:C"
## [1] "A:A" "B:A" "A:B" "B:B" "A:C" "B:C"
try(resolveFormula(y ~ g2 + g3, data = df,
                   allowed = "n-sample-independent"))
#> Error : 'formula' should be of the form response ~ group; use response ~ a:b for the cells of several grouping variables

# two-sample dependent (paired)
df2 <- data.frame(pre = rnorm(15, 50, 10), post = rnorm(15, 55, 10))
resolveFormula(Pair(pre, post) ~ 1, data = df2,
               allowed = c("one-sample",
                           "two-sample-dependent"))$type
#> [1] "two-sample-dependent"
## [1] "two-sample-dependent"

# n-sample dependent (blocked): treatment, not group
r4 <- resolveFormula(y ~ trt | blk, data = df,
                     allowed = "n-sample-dependent")
names(r4)
#> [1] "type"      "mf"        "rows"      "response"  "treatment" "block"    
#> [7] "dataName" 
## [1] "type" "mf" "rows" "response" "treatment" "block" "dataName"

# numeric-numeric: predictor, not group
df3 <- data.frame(y = rnorm(20), x = rnorm(20))
r5 <- resolveFormula(y ~ x, data = df3, allowed = "numeric-numeric")
is.numeric(r5$predictor)
#> [1] TRUE
## [1] TRUE

# regression: build the design matrix from 'terms', not from the formula
r6 <- resolveFormula(y ~ log(abs(x)) + I(x^2), data = df3,
                     allowed = "regression")
colnames(model.matrix(r6$terms, r6$mf))
#> [1] "(Intercept)" "log(abs(x))" "I(x^2)"     

# subset, as in base R
resolveFormula(y ~ g3, data = df, subset = g3 != "C",
               allowed = "two-sample-independent")$type
#> [1] "two-sample-independent"
## [1] "two-sample-independent"
```
