# Select a Range Between Two Elements (Lazy)

Returns the elements of `x` from the first occurrence of `rng[1]` to the
first occurrence of `rng[2]`. Equivalent to a non-greedy ("lazy") match,
analogous to lazy quantifiers in regular expressions.

Returns the elements of `x` from the first occurrence of `rng[1]` to the
*last* occurrence of `rng[2]`. Equivalent to a greedy match, analogous
to greedy quantifiers in regular expressions.

## Usage

``` r
x %:% rng

x %::% rng
```

## Arguments

- x:

  a vector.

- rng:

  a vector of length 2: `c(from, to)`. May contain `NA` to match missing
  values in `x`.

## Value

a subset of `x`, from the first occurrence of `rng[1]` to the first
occurrence of `rng[2]`.

a subset of `x`, from the first occurrence of `rng[1]` to the last
occurrence of `rng[2]`.

## See also

the `%::%` operator for the greedy variant, which uses the *last*
occurrence of `rng[2]`.

the `%::%`. operator for the lazy variant, which uses the *first*
occurrence of `rng[2]`.

## Examples

``` r
letters %:% c("c", "g")
#> [1] "c" "d" "e" "f" "g"

x <- c("a", "b", "c", "d", "c", "e", "f", "c")
x %:% c("c", "e")
#> [1] "c" "d" "c" "e"

colnames(Pizza) %:% c("price", "nps")
#>  [1] "price"          "operator"       "driver"         "delivery_min"  
#>  [5] "temperature"    "wine_ordered"   "wine_delivered" "wrongpizza"    
#>  [9] "quality"        "vegetarian"     "nps"           

letters %::% c("c", "g")
#> [1] "c" "d" "e" "f" "g"

x <- c("a", "b", "c", "d", "c", "e", "f", "c")
x %::% c("c", "e")
#> [1] "c" "d" "c" "e"
```
