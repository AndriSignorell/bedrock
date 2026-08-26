# Open One Side of a Confidence Interval

Clamps a confidence interval to the range of the parameter and opens the
side that a one-sided interval leaves free. One implementation for the
whole suite, so that every function reports a one-sided bound the same
way.

## Usage

``` r
applySides(ci, sides = "two.sided", lo = -Inf, hi = Inf)
```

## Arguments

- ci:

  numeric vector of length two, the lower and upper bound in that order.
  `NA` bounds are passed through, and `c(NA, NA)` is accepted although
  it is logical rather than numeric - that is how an interval which
  could not be computed is usually written.

- sides:

  character string, one of `"two.sided"` (default), `"left"` or
  `"right"`. It names the side carrying the *finite* bound, so `"left"`
  corresponds to `alternative = "greater"` in a test. Callers are
  expected to have resolved the value with
  [`match.arg`](https://rdrr.io/r/base/match.arg.html) already; an
  unmatched value is an error rather than a partial match.

- lo, hi:

  the range of the parameter, not infinities by default in spirit but in
  signature. See Details.

## Value

a named numeric vector with the elements `lci` and `uci`.

## Details

`sides` names the side carrying the finite bound:

- `"left"`:

  the informative bound is the lower one; the upper one is opened to
  `hi`.

- `"right"`:

  the informative bound is the upper one; the lower one is opened to
  `lo`.

`lo` and `hi` are the parameter's range, not infinities. Most statistics
are bounded, so reporting the open side at the boundary is the ordinary
case rather than an exception: a correlation opens to \\\pm 1\\, an
association measure in \\\[0, 1\]\\ to 0 or 1, Pearson's \\C\\ to
\\\sqrt{(m-1)/m}\\. Where the parameter really is unbounded,
\\\pm\\`Inf` is passed and the usual half-line comes back. Some
statistics need one of each: Cronbach's alpha takes `lo = -Inf` and
`hi = 1`, a relative risk `lo = 0` and `hi = Inf`.

The two-sided interval is clamped to \\\[lo, hi\]\\ as well, so an
interval can never claim a value the statistic cannot take.

## Why this is not written out per function

Five hand-written copies of the same three lines produced four different
defects across one review: two functions had the sides inverted, one
ignored them after adjusting the level, and one returned `NA` where a
boundary belonged. The operation is short enough to retype and just
subtle enough to retype wrongly.

## See also

[checkConfLevel](https://andrisignorell.github.io/bedrock/reference/checkConfLevel.md),
[checkFlag](https://andrisignorell.github.io/bedrock/reference/checkFlag.md)

## Examples

``` r
ci <- c(0.12, 0.58)

applySides(ci, "two.sided", lo = 0, hi = 1)
#>  lci  uci 
#> 0.12 0.58 
applySides(ci, "left",      lo = 0, hi = 1)   # uci opens to 1
#>  lci  uci 
#> 0.12 1.00 
applySides(ci, "right",     lo = 0, hi = 1)   # lci opens to 0
#>  lci  uci 
#> 0.00 0.58 

# an unbounded parameter opens to infinity
applySides(c(-1.4, 2.6), "left", lo = -Inf, hi = Inf)
#>  lci  uci 
#> -1.4  Inf 

# and one of each: Cronbach's alpha is bounded above only
applySides(c(0.61, 0.94), "right", lo = -Inf, hi = 1)
#>  lci  uci 
#> -Inf 0.94 

# the two-sided interval is clamped too
applySides(c(-0.2, 1.3), "two.sided", lo = 0, hi = 1)
#> lci uci 
#>   0   1 

# NA bounds survive
applySides(c(NA, NA), "left", lo = -1, hi = 1)
#> lci uci 
#>  NA   1 
```
