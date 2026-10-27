# Recode Values by Ordered Conditions

Vectorized conditional selection: the first matching rule wins.

## Usage

``` r
caseWhen(..., default = NULL, na = c("propagate", "skip"))
```

## Arguments

- ...:

  two-sided formulas of the form `condition ~ value`. Conditions must be
  plain logical vectors. Values must be plain logical, integer, double,
  complex, or character vectors. Classed objects (including factors and
  dates), arrays, and lists are not supported.

- default:

  value for elements without a match. `NULL` (the default) supplies a
  missing value of the common result type.

- na:

  handling of missing conditions: `"propagate"` (default) resolves an
  element to a missing value when its first unresolved condition is NA;
  `"skip"` treats missing conditions as FALSE.

## Value

An unnamed atomic vector of the common length and type.

## Details

Unlike `dplyr::case_when()`, which treats missing conditions as FALSE,
this function defaults to propagating NA, as in nested
[`ifelse()`](https://rdrr.io/r/base/ifelse.html) calls. Use
`na = "skip"` to skip missing conditions instead.

All conditions and values are evaluated once in their formula
environments, including values of unused rules. This is ordered
selection, not lazy or elementwise short-circuit evaluation. Errors and
warnings in unused expressions therefore still occur.

All inputs (including `default`) must have length one or the common
output length. Non-scalar lengths must agree; zero-length vectors are
supported but cannot be combined with lengths greater than one.
All-scalar inputs produce length one. Names on inputs are discarded.

Numeric types are promoted in the order logical, integer, double,
complex. Character values cannot be mixed with numeric or non-missing
logical values. All-missing logical vectors are treated as unspecified
missing values and may accompany any supported type. Every rule
participates in length and type validation, even if it never matches.

Factors, dates, and other classed vectors are deliberately unsupported
in this version; support may be added later with explicit class and
factor-level compatibility rules.

## See also

[`ifelse()`](https://rdrr.io/r/base/ifelse.html), `dplyr::case_when()`

Other data.recode: [`asBinary()`](asBinary.md),
[`combLevels()`](combLevels.md), [`dummy()`](dummy.md),
[`mReplace()`](mReplace.md), [`nf()`](nf.md), [`recodeX()`](recodeX.md),
[`revCode()`](revCode.md), [`stringsAsFactors()`](stringsAsFactors.md)

## Examples

``` r
x <- c(-1, 0, 2, NA_real_)
caseWhen(x < 0 ~ "negative", x == 0 ~ "zero", default = "positive")
#> [1] "negative" "zero"     "positive" NA        
caseWhen(x < 0 ~ "negative", default = "other", na = "skip")
#> [1] "negative" "other"    "other"    "other"   
caseWhen(c(TRUE, FALSE, NA) ~ 1L, default = 0L)
#> [1]  1  0 NA
```
