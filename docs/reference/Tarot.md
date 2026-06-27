# Tarot Cards dataset

A dataset representing a standard Tarot deck, including both Major and
Minor Arcana Cards. Each row corresponds to a single card with
attributes describing its type, suit and rank.

## Usage

``` r
Tarot
```

## Format

A data frame with 78 observations and 6 variables:

- card:

  Name of the Tarot card.

- rank:

  Raw rank of the card as character.

- suit:

  Suit of the card (wand, coin, cup, sword, or trump).

- arcana:

  Type of arcana (minor or major).

- rank_minor:

  Ordered factor indicating the rank within the minor arcana (NA for
  major arcana).

- rank_major:

  Integer indicating the rank within the major arcana (0–21, NA for
  minor arcana).

## Source

Simulated data.

## Details

The dataset is designed for teaching, simulation and modelling purposes,
illustrating how heterogeneous ordinal structures can be represented in
a statistically consistent way.

The Tarot deck consists of 56 Minor Arcana Cards and 22 Major Arcana
Cards. Since both groups follow different ranking systems, two separate
variables are provided: `rank_minor` for the ordered structure within
the minor arcana and `rank_major` for the numeric ordering of the major
arcana.

This separation avoids mixing incompatible ordinal scales and makes the
dataset suitable for statistical modelling and machine learning
applications.

## See also

Other datasets: [`Cards`](Cards.md), [`Roulette`](Roulette.md),
[`courseData()`](courseData.md)

## Examples

``` r
head(Tarot)
#>     card rank suit arcana rank_minor rank_major
#> 1 1-wand    1 wand  minor          1         NA
#> 2 2-wand    2 wand  minor          2         NA
#> 3 3-wand    3 wand  minor          3         NA
#> 4 4-wand    4 wand  minor          4         NA
#> 5 5-wand    5 wand  minor          5         NA
#> 6 6-wand    6 wand  minor          6         NA
table(Tarot$arcana)
#> 
#> minor major 
#>    56    22 
summary(Tarot$rank_minor)
#>      1      2      3      4      5      6      7      8      9     10   page 
#>      4      4      4      4      4      4      4      4      4      4      4 
#> knight  queen   king    NAs 
#>      4      4      4     22 
```
