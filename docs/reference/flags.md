# Extract Dichotomous (Binary) Variables

Identify and extract dichotomous (binary) variables from a data frame or
matrix using
[`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md).

## Usage

``` r
flags(
  x,
  strict = FALSE,
  na.rm = FALSE,
  output = c("data", "names", "index", "logical")
)
```

## Arguments

- x:

  a data frame or matrix.

- strict:

  logical. If `TRUE`, only variables with exactly two distinct values
  are considered dichotomous. If `FALSE` (default), variables with one
  or two distinct values are allowed.

- na.rm:

  logical. Should missing values be ignored when checking for
  dichotomous variables? Default is `FALSE`.

- output:

  character string specifying the output representation. One of `"data"`
  (subset of `x` containing only dichotomous variables, the default),
  `"names"` (names of dichotomous variables), `"index"` (column indices)
  or `"logical"` (logical vector indicating dichotomous variables).

## Value

depending on `output`:

- `"data"`: data frame or matrix.

- `"names"`: character vector.

- `"index"`: integer vector.

- `"logical"`: logical vector.

## Details

Variables with only missing values are not considered dichotomous when
`na.rm = FALSE`.

When `na.rm = TRUE`, such variables are treated as empty vectors and are
considered dichotomous only if `strict = FALSE`.

Internally, variables with indeterminate dichotomous status (i.e. `NA`
returned by
[`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md))
are treated as non-dichotomous for filtering purposes.

## See also

[`isDichotomous`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md)

Other data.predicate:
[`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md),
[`isEuclid()`](https://andrisignorell.github.io/bedrock/reference/isEuclid.md),
[`isLowCardinality()`](https://andrisignorell.github.io/bedrock/reference/isLowCardinality.md),
[`isNumeric()`](https://andrisignorell.github.io/bedrock/reference/isNumeric.md),
[`isWholeLike()`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md),
[`isZero()`](https://andrisignorell.github.io/bedrock/reference/isZero.md),
[`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nUnique.md)

## Examples

``` r
dat <- data.frame(
  a = c(0, 1, 1, 0),
  b = c(1, 2, 3, 4),
  c = c(TRUE, FALSE, TRUE, TRUE),
  d = c(NA, NA, NA, NA)
)

flags(dat)
#>   a     c
#> 1 0  TRUE
#> 2 1 FALSE
#> 3 1  TRUE
#> 4 0  TRUE

# effect of na.rm
flags(dat, na.rm = TRUE)
#>   a     c  d
#> 1 0  TRUE NA
#> 2 1 FALSE NA
#> 3 1  TRUE NA
#> 4 0  TRUE NA

# return variable names
flags(dat, output = "names")
#> [1] "a" "c"

# return column indices
flags(dat, output = "index")
#> a c 
#> 1 3 
```
