# Select a Range Between Two Elements

Returns the elements of `x` from the first occurrence of `rng[1]` up to
an occurrence of `rng[2]`. The two operators differ in which occurrence
of the end value terminates the range, analogous to lazy and greedy
quantifiers in regular expressions:

## Usage

``` r
x %:% rng

x %::% rng
```

## Arguments

- x:

  a vector

- rng:

  a vector of length 2: `c(from, to)`. May contain `NA` to match missing
  values in `x`.

## Value

a subset of `x`, from the first occurrence of `rng[1]` to the first
(`%:%`) or last (`%::%`) occurrence of `rng[2]`.

## Details

- `%:%` (lazy): up to the *first* occurrence of `rng[2]`.

- `%::%` (greedy): up to the *last* occurrence of `rng[2]`.

## See also

Other data.interval: [`between-operators`](between-operators.md),
[`intervals`](intervals.md)

## Examples

``` r
letters %:% c("c", "g")
#> [1] "c" "d" "e" "f" "g"

x <- c("a", "b", "c", "d", "c", "e", "f", "c")
x %:%  c("c", "e")
#> [1] "c" "d" "c" "e"
x %::% c("c", "c")     # greedy: up to the last "c"
#> [1] "c" "d" "c" "e" "f" "c"

# select a column range by name
colnames(mtcars) %:% c("hp", "vs")
#> [1] "hp"   "drat" "wt"   "qsec" "vs"  
```
