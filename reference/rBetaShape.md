# Generate Beta-Distributed Random Values by Shape

Generates beta-distributed random values using predefined distributional
shapes and transforms them to a specified interval.

## Usage

``` r
rBetaShape(
  n,
  shape = c("norm", "left", "right", "unif", "u", "j", "inv-j"),
  bounds = c(0, 1)
)
```

## Arguments

- n:

  non-negative integer giving the number of values to generate.

- shape:

  distributional shape: either one of the predefined names listed under
  **Details**, or a numeric vector of length 2 giving `shape1` and
  `shape2` directly.

- bounds:

  numeric vector containing the lower and upper bound.

## Value

a numeric vector of length `n` with values within `bounds`.

## Details

The following predefined shapes and beta parameters are available:

|  |  |  |  |
|----|----|----|----|
| **Shape** | **shape1** | **shape2** | **Description** |
| `"norm"` | 5.0 | 5.0 | symmetric and bell-shaped |
| `"left"` | 5.0 | 2.0 | left-skewed with values concentrated near the upper bound |
| `"right"` | 2.0 | 5.0 | right-skewed with values concentrated near the lower bound |
| `"unif"` | 1.0 | 1.0 | uniform |
| `"u"` | 0.5 | 0.5 | U-shaped with values concentrated near both bounds |
| `"j"` | 2.0 | 0.5 | J-shaped with values concentrated near the upper bound |
| `"inv-j"` | 0.5 | 2.0 | inverse J-shaped with values concentrated near the lower bound |

Note that `"left"` and `"right"` name the direction of the *skew*, i.e.
of the long tail, so `"right"` places the bulk of the values near the
lower bound. This is the standard convention, but it is the opposite of
what the names suggest at first reading - and unrelated to the meaning
of `"left"`/`"right"` in the `sides` argument of the interval functions,
where they name the side carrying the finite bound.

The `"norm"` shape is symmetric and bell-shaped but is not a normal
distribution. Unlike the normal distribution, all generated values are
bounded.

Values from the standard beta distribution on the interval \\\[0,1\]\\
are transformed to the interval specified by `bounds` as

\$\$a + (b-a)X\$\$

where \\a\\ and \\b\\ are the lower and upper bounds, respectively.

## Random number generation

The values are drawn with
[`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html) and therefore
depend on the state of R's global random number generator. No seed is
set internally; call
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) beforehand, or
wrap the call in [`withSeed()`](withSeed.md), for reproducible results.

## See also

[`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html),
[`stats::runif()`](https://rdrr.io/r/stats/Uniform.html)

Other random.numbers: [`rSum21()`](rSum21.md)

## Examples

``` r
set.seed(42)

x <- rBetaShape(
  1000,
  shape = "right",
  bounds = c(10, 90)
)

summary(x)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   10.24   22.48   30.91   32.45   40.47   81.73 
range(x)
#> [1] 10.23887 81.73250

# shape parameters can also be given directly
rBetaShape(5, shape = c(3, 1.5), bounds = c(0, 100))
#> [1] 71.50221 70.35936 69.16962 61.70411 19.40427
```
