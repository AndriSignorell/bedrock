# Roulette wheel dataset

A dataset representing the numbers on a European Roulette wheel and
their associated properties such as color, parity and positional
groupings.

## Usage

``` r
Roulette
```

## Format

A data frame with 37 observations and 8 variables:

- num:

  Roulette number (0–36).

- col:

  Color of the number (red, black, green).

- parity:

  Parity of the number (even or odd, NA for zero).

- highlow:

  Indicator for low (1–18) or high (19–36), NA for zero.

- dozens:

  Dozen grouping (1 = 1–12, 2 = 13–24, 3 = 25–36, NA for zero).

- column:

  Column grouping on the Roulette table (1, 2, 3, NA for zero).

- pocketrange:

  Wheel sector grouping (e.g. jeu zero, voisins du zero, orphelins,
  tiers du cylindre).

## Source

Simulated data based on a European Roulette wheel.

## Details

Each row corresponds to a single number on the wheel (0–36), including
commonly used betting categories.

The dataset is suitable for teaching probability, categorical data
analysis and simulating betting strategies.

The dataset represents a European Roulette wheel (single zero). The
number zero is treated as a special case and has missing values for
several derived variables such as parity, high/low, dozens and column.

The variable `pocketrange` reflects traditional French wheel sectors
used in Roulette betting systems.

## See also

Other datasets: [`Cards`](Cards.md), [`Tarot`](Tarot.md),
[`courseData()`](courseData.md)

## Examples

``` r
head(Roulette)
#>   num   col parity highlow dozens column     pocketrange
#> 1   0 green   <NA>    <NA>     NA     NA        jeu zero
#> 2  26 black   even    high      3     35        jeu zero
#> 3   3   red    odd     low      1     36        jeu zero
#> 4  35 black    odd    high      3     35        jeu zero
#> 5  12   red   even     low      1     36        jeu zero
#> 6  28 black   even    high      3     34 voisins du zero
table(Roulette$col)
#> 
#> black green   red 
#>    18     1    18 
table(Roulette$parity, useNA = "ifany")
#> 
#> even  odd <NA> 
#>   18   18    1 
```
