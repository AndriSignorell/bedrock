# European Roulette Wheel

The numbers on a single-zero (European) Roulette wheel and their
associated properties: colour, betting categories and the traditional
sectors of the wheel. Each row represents one of the 37 numbers (0–36).

## Usage

``` r
Roulette
```

## Format

A data frame with 37 rows and 7 variables:

- num:

  integer, the number in the pocket (0–36).

- col:

  factor, colour of the pocket: `red`, `black` or `green`.

- parity:

  factor, `even` or `odd`.

- highlow:

  factor, `low` (1–18) or `high` (19–36).

- dozens:

  factor, dozen on the table: `1` (1–12), `2` (13–24), `3` (25–36).

- column:

  factor, column on the table: `1`, `2` or `3`, counted from the one
  containing 1.

- pocketrange:

  factor, sector of the wheel: `jeu zero`, `voisins du zero`,
  `tiers du cylindre` or `orphelins`.

## Source

The standard layout of a single-zero Roulette wheel.

## Details

The dataset can be used for teaching probability and categorical data
analysis, as well as for simulating Roulette betting strategies.

The rows are ordered as the pockets follow each other on the wheel,
starting at zero, and not by `num`. The sectors in `pocketrange` are
therefore contiguous blocks of rows.

Zero takes part in none of the even/odd, high/low, dozen and column
bets, so these variables are `NA` for zero. Note that this follows the
rules of the game and not arithmetic, where zero would count as even.

The seven numbers of the *jeu zero* are part of the *voisins du zero* in
the usual reading of the terms. As every number appears exactly once
here, they are reported as a separate level and `voisins du zero` covers
the remaining ten numbers of that sector.

Accents are dropped in the factor levels (`voisins du zero` for *voisins
du zéro*).

## See also

Other datasets: [`Cards`](cards.md), [`Pizza`](Pizza.md),
[`Tarot`](tarot.md), [`courseData()`](courseData.md)

## Examples

``` r
head(Roulette)
#>   num   col parity highlow dozens column     pocketrange
#> 1   0 green   <NA>    <NA>   <NA>   <NA>        jeu zero
#> 2  26 black   even    high      3      2        jeu zero
#> 3   3   red    odd     low      1      3        jeu zero
#> 4  35 black    odd    high      3      2        jeu zero
#> 5  12   red   even     low      1      3        jeu zero
#> 6  28 black   even    high      3      1 voisins du zero

table(Roulette$col)
#> 
#>   red black green 
#>    18    18     1 
table(Roulette$parity, Roulette$highlow, useNA = "ifany")
#>       
#>        low high <NA>
#>   even   9    9    0
#>   odd    9    9    0
#>   <NA>   0    0    1

# the sectors of the wheel are blocks of neighbouring pockets
table(Roulette$pocketrange)
#> 
#>          jeu zero   voisins du zero tiers du cylindre         orphelins 
#>                 7                10                12                 8 
```
