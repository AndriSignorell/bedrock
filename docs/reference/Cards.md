# Playing Cards dataset

A dataset representing a standard deck of playing Cards. Each row
corresponds to a single card and includes information such as suit, rank
and numerical value.

## Usage

``` r
Cards
```

## Format

A data frame with 52 observations and X variables:

- card:

  Name of the card.

- rank:

  Rank of the card (e.g. 2–10, Jack, Queen, King, Ace).

- suit:

  Suit of the card (e.g. hearts, diamonds, clubs, spades).

## Source

Simulated data.

## Details

The dataset can be used for simulations, probability calculations and
teaching examples involving discrete outcomes and combinatorics.

The dataset represents a complete deck without jokers. It is suitable
for probability experiments, simulations and demonstrations of
categorical data.

## See also

Other datasets: [`Pizza`](Pizza.md), [`Roulette`](Roulette.md),
[`Tarot`](Tarot.md), [`courseData()`](courseData.md)

## Examples

``` r
head(Cards)
#>     card rank suit
#> 1 2-club    2 club
#> 2 3-club    3 club
#> 3 4-club    4 club
#> 4 5-club    5 club
#> 5 6-club    6 club
#> 6 7-club    7 club
table(Cards$suit)
#> 
#>    club diamond   heart   spade 
#>      13      13      13      13 
```
