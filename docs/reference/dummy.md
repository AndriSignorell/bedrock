# Generate Dummy Codes for a Factor

Generate a matrix of dummy codes, also known as class indicators, for a
factor or class vector.

## Usage

``` r
dummy(
  x,
  method = c("treatment", "sum", "helmert", "poly", "full"),
  base = 1,
  levels = NULL
)
```

## Arguments

- x:

  Factor or vector of classes.

- method:

  Character string specifying the contrast method. One of `"treatment"`,
  `"sum"`, `"helmert"`, `"poly"`, or `"full"`. Abbreviations are
  accepted.

- base:

  Integer or character string specifying the baseline group for
  treatment contrasts.

- levels:

  Optional character vector specifying the possible levels of `x`. If
  `NULL`, levels are inferred by `factor(x)`.

## Value

A matrix with dummy codes. The number of rows equals `length(x)`. For
`method = "full"`, the number of columns equals the number of levels.
Otherwise, the number of columns equals the number of levels minus one.

The returned matrix has an attribute `"base"` containing the baseline
level for non-full coding, and `NA` for full coding.

## Details

The argument `method` controls the contrast coding. The option `"full"`
returns one indicator column for each level of `x`. This full-rank
coding is usually redundant for
[`lm()`](https://rdrr.io/r/stats/lm.html) and related modelling
functions.

## See also

`model.frame`, `contrasts`,
[`stats::contr.treatment`](https://rdrr.io/r/stats/contrast.html),
[`stats::contr.sum`](https://rdrr.io/r/stats/contrast.html),
[`stats::contr.helmert`](https://rdrr.io/r/stats/contrast.html),
[`stats::contr.poly`](https://rdrr.io/r/stats/contrast.html)

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md), [`asBinary()`](asBinary.md),
[`asCDateFmt()`](asCDateFmt.md), [`columnWrap()`](columnWrap.md),
[`combLevels()`](combLevels.md),
[`compareDataFrames()`](compareDataFrames.md), [`nf()`](nf.md),
[`recodeX()`](recodeX.md), [`recycle()`](recycle.md),
[`renameX()`](renameX.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitAt()`](splitAt.md), [`splitX()`](splitX.md),
[`stringsAsFactors()`](stringsAsFactors.md), [`toBaseR()`](toBaseR.md)

## Examples

``` r
x <- c("red", "blue", "green", "blue", "green", "red", "red", "blue")
dummy(x)
#>   green red
#> 1     0   1
#> 2     0   0
#> 3     1   0
#> 4     0   0
#> 5     1   0
#> 6     0   1
#> 7     0   1
#> 8     0   0
#> attr(,"base")
#> [1] "blue"
dummy(x, base = 2)
#>   blue red
#> 1    0   1
#> 2    1   0
#> 3    0   0
#> 4    1   0
#> 5    0   0
#> 6    0   1
#> 7    0   1
#> 8    1   0
#> attr(,"base")
#> [1] "green"
dummy(x, method = "sum")
#>   green red
#> 1    -1  -1
#> 2     1   0
#> 3     0   1
#> 4     1   0
#> 5     0   1
#> 6    -1  -1
#> 7    -1  -1
#> 8     1   0
#> attr(,"base")
#> [1] "blue"

y <- c("Max", "Max", "Max", "Max", "Max", "Bill", "Bill", "Bill")
dummy(y)
#>   Max
#> 1   1
#> 2   1
#> 3   1
#> 4   1
#> 5   1
#> 6   0
#> 7   0
#> 8   0
#> attr(,"base")
#> [1] "Bill"
dummy(y, base = "Max")
#>   Bill
#> 1    0
#> 2    0
#> 3    0
#> 4    0
#> 5    0
#> 6    1
#> 7    1
#> 8    1
#> attr(,"base")
#> [1] "Max"
dummy(y, base = "Max", method = "full")
#>   Bill Max
#> 1    0   1
#> 2    0   1
#> 3    0   1
#> 4    0   1
#> 5    0   1
#> 6    1   0
#> 7    1   0
#> 8    1   0
#> attr(,"base")
#> [1] NA

# Revert full dummy coding
m <- dummy(y, method = "full")
apply(m, 1, function(z) colnames(m)[z == 1])
#>      1      2      3      4      5      6      7      8 
#>  "Max"  "Max"  "Max"  "Max"  "Max" "Bill" "Bill" "Bill" 

# Revert treatment dummy coding
m <- dummy(y)
apply(
  m,
  1,
  function(z) ifelse(sum(z) == 0, attr(m, "base"), colnames(m)[z == 1])
)
#>      1      2      3      4      5      6      7      8 
#>  "Max"  "Max"  "Max"  "Max"  "Max" "Bill" "Bill" "Bill" 
```
