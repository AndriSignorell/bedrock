# Parse and Classify a Model Formula

Parses a model formula, constructs a model frame, and classifies the
resulting design into one of five dependency structures. The function
serves as a unified entry point for functions that accept a formula
interface.

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

  a model formula. Supported forms are:

  `y ~ 1` or `y`

  :   one-sample design

  `Pair(x, y) ~ 1`

  :   two-sample dependent (paired).
      [`Pair`](https://rdrr.io/r/stats/Pair.html) constructs a
      two-column matrix of paired observations.

  `y ~ g`

  :   two-sample or n-sample independent group comparison

  `y ~ x`, `x` numeric

  :   numeric-numeric (correlation, regression)

  `y ~ trt | block`

  :   n-sample dependent (blocked design)

- data:

  an optional data frame containing the variables in `formula`. A matrix
  is coerced to a data frame.

- subset:

  an optional expression indicating which observations to use. Must be
  captured via [`substitute()`](https://rdrr.io/r/base/substitute.html)
  in the calling function to avoid collision with
  [`base::subset()`](https://rdrr.io/r/base/subset.html). See Details.

- na.action:

  a function specifying how missing values are handled. Defaults to
  [`na.pass`](https://rdrr.io/r/stats/na.fail.html).

- allowed:

  a character vector restricting which design types are accepted. Any
  combination of: `"one-sample"`, `"two-sample-independent"`,
  `"two-sample-dependent"`, `"n-sample-independent"`,
  `"n-sample-dependent"`, `"numeric-numeric"`. An error is raised if the
  detected type is not in `allowed`. Default allows all types.

## Value

a named list with at minimum:

- `type`:

  character, one of the design types listed above

- `mf`:

  the [`model.frame`](https://rdrr.io/r/stats/model.frame.html)

- `data.name`:

  the deparsed formula string

Plus design-specific components as described in Details.

## Details

**Design types:**

|  |  |  |
|----|----|----|
| **type** | **Formula** | **Examples** |
| `one-sample` | `y ~ 1` | t-test, Wilcoxon one-sample |
| `two-sample-independent` | `y ~ g` (k=2) | t-test, Wilcoxon rank-sum |
| `two-sample-dependent` | `Pair(x,y) ~ 1` | paired t-test, Wilcoxon signed-rank |
| `n-sample-independent` | `y ~ g` (k\>2) | ANOVA, Kruskal-Wallis |
| `n-sample-dependent` | `y ~ trt | block` | repeated measures ANOVA, Friedman |
| `numeric-numeric` | `y ~ x` (x numeric) | correlation, regression |

**Field naming contract (binding across all types):**

- `group` is reserved exclusively for a categorical, factor-coercible
  variable of length `n` (the full sample) that splits the response into
  groups. It is never pre-split and never used for a continuous
  variable. `x` + `group` have an *identical shape* for both
  `two-sample-independent` and `n-sample-independent` - callers can use
  `split(r$x, r$group)` uniformly, without branching on `k`.

- `predictor` is used for a continuous, numeric right-hand side variable
  (`numeric-numeric`). Never called `group`.

- `treatment` is used for the treated/explanatory variable in a blocked
  design (`n-sample-dependent`), distinct from `block`, the
  stratification factor. Never called `group`.

- `y`, where present, is always a *convenience* field (e.g. group 2 of a
  two-sample design, or the second paired vector). It is never required
  for correct use - `x` + `group` (or `x` + `predictor` / `treatment` +
  `block`) is always sufficient and is the canonical access path.

**subset handling:**

Because `subset` is both an argument name and a base R function, name
collisions can occur when forwarding to
[`stats::model.frame`](https://rdrr.io/r/stats/model.frame.html). The
calling function must capture `subset` as an unevaluated expression:


    myFun <- function(formula, data, subset, na.action = na.pass, ...) {
      subset_expr <- if (!missing(subset)) substitute(subset) else NULL
      resolveFormula(formula, data,
                     subset    = subset_expr,
                     na.action = na.action)
    }

**Return value components by type:**

All return values contain `type`, `mf` and `data.name`. Additional
components depend on the design:

|                          |                                          |
|--------------------------|------------------------------------------|
| **type**                 | **Additional components**                |
| `one-sample`             | `x`                                      |
| `two-sample-independent` | `x`, `group`, `y` (convenience: group 2) |
| `two-sample-dependent`   | `x`, `y`                                 |
| `n-sample-independent`   | `x`, `group`                             |
| `n-sample-dependent`     | `response`, `treatment`, `block`         |
| `numeric-numeric`        | `x`, `predictor`                         |

## See also

[`model.frame`](https://rdrr.io/r/stats/model.frame.html),
[`Pair`](https://rdrr.io/r/stats/Pair.html), `desc`

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
#> [1] "one-sample"

# two-sample independent: x + group have full length, same shape as k>2
r2 <- resolveFormula(y ~ g2, data = df,
                     allowed = c("two-sample-independent",
                                 "n-sample-independent"))
r2$type
#> [1] "two-sample-independent"
#> [1] "two-sample-independent"
length(r2$x) == length(r2$group)
#> [1] TRUE
#> [1] TRUE

# n-sample independent
resolveFormula(y ~ g3, data = df,
               allowed = "n-sample-independent")$type
#> [1] "n-sample-independent"
#> [1] "n-sample-independent"

# two-sample dependent (paired)
df2 <- data.frame(pre = rnorm(15, 50, 10), post = rnorm(15, 55, 10))
resolveFormula(Pair(pre, post) ~ 1, data = df2,
               allowed = c("one-sample",
                           "two-sample-dependent"))$type
#> [1] "two-sample-dependent"
#> [1] "two-sample-dependent"

# n-sample dependent (blocked): treatment, not group
r4 <- resolveFormula(y ~ trt | blk, data = df,
                     allowed = "n-sample-dependent")
r4$type
#> [1] "n-sample-dependent"
#> [1] "n-sample-dependent"
names(r4)
#> [1] "type"      "mf"        "response"  "treatment" "block"     "data.name"

# numeric-numeric: predictor, not group
df3 <- data.frame(y = rnorm(20), x = rnorm(20))
r5 <- resolveFormula(y ~ x, data = df3, allowed = "numeric-numeric")
r5$type
#> [1] "numeric-numeric"
#> [1] "numeric-numeric"
is.numeric(r5$predictor)
#> [1] TRUE
#> [1] TRUE
```
