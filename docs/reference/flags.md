# Extract Dichotomous (Binary) Variables

Identify and extract dichotomous (binary) variables from a data frame or
matrix using [`isDichotomous()`](isDichotomous.md).

## Usage

``` r
flags(x, strict = FALSE, na.rm = FALSE, output = "data")
```

## Arguments

- x:

  A data frame or matrix.

- strict:

  Logical.

  If `TRUE`, only variables with exactly two distinct values are
  considered dichotomous.

  If `FALSE`, variables with one or two distinct values are allowed.

  Default is `FALSE`.

- na.rm:

  Logical.

  Should missing values be ignored when checking for dichotomous
  variables?

  Default is `FALSE`.

- output:

  Character string specifying the output representation.

  One of:

  `\"data\"`

  :   Subset of `x` containing only dichotomous variables.

  Default is `\"data\"`.

## Value

Depending on `output`:

- `\"data\"`: data frame or matrix

- `\"names\"`: character vector

- `\"index\"`: integer vector

- `\"logical\"`: logical vector

## Details

Variables with only missing values are not considered dichotomous when
`na.rm = FALSE`.

When `na.rm = TRUE`, such variables are treated as empty vectors and are
considered dichotomous only if `strict = FALSE`.

Internally, variables with indeterminate dichotomous status (i.e. `NA`
returned by [`isDichotomous()`](isDichotomous.md)) are treated as
non-dichotomous for filtering purposes.

## See also

[`isDichotomous`](isDichotomous.md)

Other data.inspection: [`allDuplicated()`](allDuplicated.md),
[`allIdentical()`](allIdentical.md),
[`completeColumns()`](completeColumns.md),
[`countCompCases()`](countCompCases.md),
[`isDichotomous()`](isDichotomous.md), [`isEuclid()`](isEuclid.md),
[`isNumeric()`](isNumeric.md), [`isURL()`](isURL.md),
[`isWholeLike()`](isWholeLike.md), [`isZero()`](isZero.md)

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
