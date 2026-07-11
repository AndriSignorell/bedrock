# Get or Set Object and Variable Labels

Retrieve or assign a label to an object, or to variables (columns) of a
data frame.

## Usage

``` r
label(x, vars = NULL)

label(x, vars = NULL) <- value
```

## Arguments

- x:

  an object. Typically an atomic vector or a data.frame.

- vars:

  optional specification of variables (columns) in a data.frame. Can be:

  - `NULL`: operate on the object label (default)

  - `TRUE`: all columns

  - numeric indices or character names of columns

- value:

  a character vector of labels, or `NULL` to remove them. For object
  labels, must be of length 1. For variable labels, must have length 1
  or the same length as `vars`.

## Value

- Getter: A character scalar (object label) or a named character vector
  (variable labels).

- Setter: The modified object `x`.

## Details

For atomic objects, a single label can be stored as an attribute
`"label"`. For data frames, a label can be assigned either to the whole
dataset or to individual columns.

The function provides a unified interface for working with labels:

- `label(x)` returns the label of an object

- `label(x) <- "text"` sets the label of an object

- `label(x, vars = ...)` returns labels of selected variables

- `label(x, vars = ...) <- value` sets variable labels

Variable labels are stored as attribute `"label"` on each column.
Assigning `NULL` removes the label(s).

## See also

Other label.attrs: [`renameX()`](renameX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md)

## Examples

``` r
df <- data.frame(age = 1:3, sex = c("m", "f", "m"))

# Set dataset label
label(df) <- "Example dataset"
label(df)
#> [1] "Example dataset"

# Set variable labels
label(df, vars = TRUE) <- c("Age in years", "Sex")
label(df, vars = TRUE)
#>            age            sex 
#> "Age in years"          "Sex" 

# Set single variable label
label(df, vars = "age") <- "Age"
label(df, vars = "age")
#>   age 
#> "Age" 

# Remove variable labels
label(df, vars = TRUE) <- NULL

# Atomic vector
x <- 1:5
label(x) <- "Simple vector"
label(x)
#> [1] "Simple vector"
```
