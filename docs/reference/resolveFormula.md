# Parse and Classify a Model Formula

Parses a model formula, builds the model frame and classifies the
resulting design into one of six dependency structures. The pieces of
the design are returned under a fixed set of names, so that every
function offering a formula interface can share one entry point instead
of re-implementing the parsing, the `subset` handling and the
distinction between a grouping factor, a numeric predictor and a
blocking variable.

## Usage

``` r
resolveFormula(
  formula,
  data,
  subset,
  na.action = na.pass,
  allowed = c("one-sample", "two-sample-independent", "two-sample-dependent",
    "n-sample-independent", "n-sample-dependent", "numeric-numeric")
)
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

  `y ~ x`, `x` numeric

  :   numeric-numeric (correlation, regression).

  `y ~ trt | block`

  :   n-sample dependent (blocked design).

- data:

  an optional data frame containing the variables in `formula`. A matrix
  is coerced to a data frame.

- subset:

  an optional expression indicating which observations to use. Must be
  captured via [`substitute()`](https://rdrr.io/r/base/substitute.html)
  in the calling function to avoid collision with
  [`subset()`](https://rdrr.io/r/base/subset.html). See Details.

- na.action:

  a function specifying how missing values are handled, defaults to
  [`na.pass()`](https://rdrr.io/r/stats/na.fail.html).

- allowed:

  a character vector restricting which design types are accepted, any
  combination of `"one-sample"`, `"two-sample-independent"`,
  `"two-sample-dependent"`, `"n-sample-independent"`,
  `"n-sample-dependent"` and `"numeric-numeric"`. The values are matched
  exactly, an unknown one is an error rather than being ignored. A
  further error is raised if the detected type is not among the allowed
  ones. Defaults to all types.

## Value

a named list containing at least:

- type:

  character, one of the design types listed above.

- mf:

  the [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) the
  design was read from.

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

  `y ~ x` with a numeric right-hand side, as in correlation or
  regression.

**Type detection**

The type follows from the shape of the formula and from the class of the
right-hand side variable, not from `allowed`. `allowed` only decides
whether the detected type is accepted, with two exceptions worth
knowing. A grouping factor carrying a single level is reported as
`one-sample` if that type is allowed, and a two-group design is reported
as `n-sample-independent` if `"two-sample-independent"` is not among the
allowed types. Both are deliberate: a caller that treats every group
count alike needs to allow one type only.

**Field naming contract (binding across all types)**

- `group` is reserved for a categorical, factor-coercible variable of
  length `n` (the full sample) that splits the response into groups. It
  is never pre-split and never used for a continuous variable. `x` and
  `group` have an identical shape for `two-sample-independent` and for
  `n-sample-independent`, so that a caller can use `split(r$x, r$group)`
  uniformly, without branching on the number of groups.

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

**Missing values**

Missing values are left to `na.action` and are not touched otherwise, so
with the default [`na.pass()`](https://rdrr.io/r/stats/na.fail.html)
they reach the caller untouched. The one exception is the grouping
factor of an independent design, where empty and missing levels are
dropped before the groups are counted. A grouping variable that is
missing throughout leaves no level at all and is an error.

**subset handling**

Because `subset` is both an argument name and a base R function, name
collisions can occur when forwarding to
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html). The calling
function must therefore capture `subset` as an unevaluated expression
and pass the resulting object on directly:


    myFun <- function(formula, data, subset, na.action = na.pass, ...) {
      subsetExpr <- if (!missing(subset)) substitute(subset) else NULL
      resolveFormula(formula, data,
                     subset    = subsetExpr,
                     na.action = na.action)
    }

**Return components by type**

Every return value contains `type`, `mf` and `dataName`. The remaining
components depend on the design:

- `one-sample`:

  `x`

- `two-sample-independent`:

  `x`, `group`, `y` (convenience: the second group)

- `two-sample-dependent`:

  `x`, `y`

- `n-sample-independent`:

  `x`, `group`

- `n-sample-dependent`:

  `response`, `treatment`, `block`

- `numeric-numeric`:

  `x`, `predictor`

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
#> [1] "type"      "mf"        "response"  "treatment" "block"     "data.name"
## [1] "type" "mf" "response" "treatment" "block" "dataName"

# numeric-numeric: predictor, not group
df3 <- data.frame(y = rnorm(20), x = rnorm(20))
r5 <- resolveFormula(y ~ x, data = df3, allowed = "numeric-numeric")
is.numeric(r5$predictor)
#> [1] TRUE
## [1] TRUE
```
