# Combine Levels from Multiple Inputs

Extracts and combines the levels from one or more vectors or factors.
Non-factor inputs are coerced to factors before extracting levels.

## Usage

``` r
combLevels(..., sorted = FALSE, na = FALSE)
```

## Arguments

- ...:

  one or more vectors or factors

- sorted:

  logical; if `TRUE`, the resulting levels are sorted

- na:

  logical; if `TRUE`, `NA` is treated as a valid level (i.e., included
  in the result)

## Value

A character vector containing the unique levels across all inputs.

## Details

Each input is coerced to a factor (if not already one), and its levels
are extracted. The union of all levels is returned. Unused levels of
factor inputs are preserved.

By default, missing values (`NA`) are not included as a level. Set
`na = TRUE` to include them; `NA` is then placed last when sorting.

The order of levels follows their first occurrence unless
`sorted = TRUE`.

## See also

Other data.recode: [`asBinary()`](asBinary.md), [`dummy()`](dummy.md),
[`mReplace()`](mReplace.md), [`nf()`](nf.md), [`recodeX()`](recodeX.md),
[`revCode()`](revCode.md), [`stringsAsFactors()`](stringsAsFactors.md)

## Examples

``` r
x <- factor(c("A", "B"))
y <- c("B", "C")

combLevels(x, y)
#> [1] "A" "B" "C"

# Sorted levels
combLevels(x, y, sorted = TRUE)
#> [1] "A" "B" "C"

# Including NA as a level
x <- c("A", NA)
y <- c("B", NA)
combLevels(x, y, na = TRUE)
#> [1] "A" NA  "B"


```
