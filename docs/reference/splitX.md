# Split Data into Groups (Extended Interface)

Splits a vector or object into groups defined by a factor or grouping
variables. This is a wrapper around
[`split`](https://rdrr.io/r/base/split.html) with an additional formula
interface.

## Usage

``` r
splitX(x, ...)

# Default S3 method
splitX(x, f, drop = FALSE, ...)

# S3 method for class 'formula'
splitX(formula, data, subset, na.action, drop = FALSE, ...)
```

## Arguments

- x:

  Object to be split (typically a vector).

- ...:

  Further arguments passed to
  [`split`](https://rdrr.io/r/base/split.html).

- f:

  A factor or list of factors defining the groups (default method).

- drop:

  Logical; if `TRUE`, unused factor levels are dropped.

- formula:

  A formula of the form `y ~ group` or `y ~ g1 + g2` specifying the
  variable to split (`y`) and the grouping variables.

- data:

  A data frame containing the variables in the formula.

- subset:

  Optional logical expression indicating rows to include.

- na.action:

  A function specifying how to handle missing values (e.g.,
  [`na.omit`](https://rdrr.io/r/stats/na.fail.html)).

## Value

A list of subsets of `x`, grouped according to `f` or the grouping
variables in the formula.

## Details

`splitX` extends [`split`](https://rdrr.io/r/base/split.html) by
providing:

- An S3 interface

- A formula method for convenient specification of variables

- Support for multiple grouping variables via formula

The formula interface evaluates a
[`model.frame`](https://rdrr.io/r/stats/model.frame.html) and splits the
response variable by one or more grouping variables.

If multiple grouping variables are provided, the data are split by their
interaction (similar to `split(..., interaction(...))`).

## See also

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md),
[`columnWrap()`](columnWrap.md), [`combLevels()`](combLevels.md),
[`nf()`](nf.md), [`parseSASDatalines()`](parseSASDatalines.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`revCode()`](revCode.md), [`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md),
[`untable()`](untable.md)

## Examples

``` r
# Default usage
x <- 1:10
g <- rep(letters[1:2], each = 5)
splitX(x, g)
#> $a
#> [1] 1 2 3 4 5
#> 
#> $b
#> [1]  6  7  8  9 10
#> 

# Formula interface
df <- data.frame(
  y = rnorm(10),
  g1 = rep(letters[1:2], each = 5),
  g2 = rep(1:2, times = 5)
)

splitX(y ~ g1, data = df)
#> $a
#> [1]  0.5350384 -0.5836362 -2.1614605 -1.3202132  0.8104089
#> 
#> $b
#> [1]  1.3416652  0.6925525 -0.3231760 -0.1173097 -0.4226757
#> 
#> attr(,"data.name")
#> [1] "y by g1 (rows)"
splitX(y ~ g1 + g2, data = df)
#> $a.1
#> [1]  0.5350384 -2.1614605  0.8104089
#> 
#> $b.1
#> [1]  0.6925525 -0.1173097
#> 
#> $a.2
#> [1] -0.5836362 -1.3202132
#> 
#> $b.2
#> [1]  1.3416652 -0.3231760 -0.4226757
#> 
#> attr(,"data.name")
#> [1] "y by g1 : g2 (rows)"
```
