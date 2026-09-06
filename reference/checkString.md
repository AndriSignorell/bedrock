# Validate a Character String

Checks that an argument is a single non-missing character string. Meant
for the labelling arguments across the suite - `dataName`, captions,
axis titles - where a vector or an `NA` would otherwise travel unnoticed
into printed output.

## Usage

``` r
checkString(x, name = deparse(substitute(x)))
```

## Arguments

- x:

  the value to check.

- name:

  the argument name to use in the message. Defaults to the expression
  that was passed.

## Value

`x`, invisibly.

## Details

An optional argument that may also be `NULL` is guarded by the caller,
`if (!is.null(dataName)) checkString(dataName)`, rather than by a
further argument here: whether the absence of a label is admissible is a
decision of the function, not of the check.

The empty string is accepted. It is a legitimate label, and a caller
that needs a non-empty one says so itself.

## See also

[`checkConfLevel()`](checkConfLevel.md), [`checkFlag()`](checkFlag.md),
[`checkCount()`](checkCount.md)

## Examples

``` r
dataName <- "smoking by sex"
checkString(dataName)

if (FALSE) { # \dontrun{
checkString(NA_character_)     # a missing label is not a label
checkString(c("a", "b"))       # length
checkString(42)                # type
} # }
```
