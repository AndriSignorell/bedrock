# Validate a Confidence Level

Checks that `conf.level` is a single number in \\(0, 1)\\, or `NA`.
Intended for the confidence-interval functions across the suite, so that
all of them accept the same values and refuse the rest with the same
message.

## Usage

``` r
checkConfLevel(conf.level)
```

## Arguments

- conf.level:

  the value to check.

## Value

`conf.level`, invisibly,  
so the check can be used in an assignment:
`conf.level <- checkConfLevel(conf.level)`.

## Details

The order of the tests is the point of this function. `NA` is *logical*,
so a check that leads with `!is.numeric()` rejects the very default most
of these functions carry. And
[`is.na()`](https://rdrr.io/r/base/NA.html) on a vector of length other
than one turns the surrounding `if` into the error message, which then
talks about the condition instead of the argument. Length first, then
type, then range.

`NaN` is excluded explicitly: `is.na(NaN)` is `TRUE`, so without that
test a `NaN` would be silently accepted as "no interval wanted".

## See also

[checkFlag](checkFlag.md)

## Examples

``` r
checkConfLevel(0.95)
checkConfLevel(NA)

if (FALSE) { # \dontrun{
checkConfLevel(c(0.9, 0.95))   # length
checkConfLevel(NULL)           # length
checkConfLevel(NaN)            # not a level, and not NA either
checkConfLevel(0)              # range is open
} # }
```
