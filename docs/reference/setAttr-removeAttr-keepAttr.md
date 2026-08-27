# Set and Remove Object Attributes

Convenience helpers to add, remove, or selectively retain attributes of
an object.

## Usage

``` r
setAttr(x, attrNames, attrValues)

removeAttr(x, attrNames = NULL)

keepAttr(x, attrNames)
```

## Arguments

- x:

  object to modify.

- attrNames:

  character vector of attribute names.

- attrValues:

  values for the attributes (only for setting). For a single attribute
  name, `attrValues` is taken as the value itself (which may be a
  vector). For several names, supply one value per name; use a list for
  non-scalar or mixed-type values.

## Value

modified object.

## See also

[stats::setNames](https://rdrr.io/r/stats/setNames.html),
[base::unname](https://rdrr.io/r/base/unname.html)

Other label.attrs: [`label()`](label.md), [`renameX()`](renameX.md),
[`setNamesX()`](setNamesX.md)

## Examples

``` r
x <- runif(10)

x <- setAttr(
  x,
  attrNames = c("some_attr", "other_attr"),
  attrValues = c("First attribute", "Second attribute")
)

# a single attribute can take a vector value
setAttr(1:10, "dim", c(2, 5))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    3    5    7    9
#> [2,]    2    4    6    8   10

# several non-scalar values via list
setAttr(1:10, c("dim", "myattr"), list(c(2, 5), "abc"))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    3    5    7    9
#> [2,]    2    4    6    8   10
#> attr(,"myattr")
#> [1] "abc"

# remove single attribute
removeAttr(x, "other_attr")
#>  [1] 0.27775593 0.21269952 0.28479048 0.89509410 0.44623532 0.77998489
#>  [7] 0.88061903 0.41312421 0.06380848 0.33548749
#> attr(,"some_attr")
#> [1] "First attribute"

# remove all attributes
removeAttr(x)
#>  [1] 0.27775593 0.21269952 0.28479048 0.89509410 0.44623532 0.77998489
#>  [7] 0.88061903 0.41312421 0.06380848 0.33548749

# keep only selected attributes, remove all others
r.lm <- lm(Fertility ~ ., swiss)
keepAttr(r.lm$terms, "class")
#> Fertility ~ Agriculture + Examination + Education + Catholic + 
#>     Infant.Mortality
```
