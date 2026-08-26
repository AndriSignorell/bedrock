# Evaluate an expression under a temporary random seed

Sets the random seed for the duration of `expr` and restores the
caller's random state afterwards. This makes a single result
reproducible without hijacking the random stream of the surrounding
script: two calls with the same seed give the same result, and whatever
is drawn after them is unaffected by either.

## Usage

``` r
withSeed(seed, expr)
```

## Arguments

- seed:

  a single number, or `NULL` to leave the random stream untouched and
  simply evaluate `expr`

- expr:

  the expression to evaluate; evaluated lazily, in the caller's
  environment

## Value

the value of `expr`

## Details

The plain idiom `set.seed(s); expr` lacks the second half. In a script
that generates a series of random objects, seeding one of them shifts
every draw that follows, so results that were correct before the seed
was added silently change.

## Examples

``` r
set.seed(1)
a <- runif(1)

set.seed(1)
withSeed(99, runif(1))     # unrelated draw in between
#> [1] 0.5847119
identical(runif(1), a)     # the stream continued as if it never happened
#> [1] TRUE

identical(withSeed(7, runif(3)), withSeed(7, runif(3)))
#> [1] TRUE
```
