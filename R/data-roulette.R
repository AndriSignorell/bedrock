
#' European Roulette Wheel
#'
#' The numbers on a single-zero (European) Roulette wheel and their
#' associated properties: colour, betting categories and the traditional
#' sectors of the wheel. Each row represents one of the 37 numbers (0--36).
#'
#' The dataset can be used for teaching probability and categorical data
#' analysis, as well as for simulating Roulette betting strategies.
#'
#' @format A data frame with 37 rows and 7 variables:
#' \describe{
#' \item{num}{integer, the number in the pocket (0--36).}
#' \item{col}{factor, colour of the pocket: `red`, `black` or `green`.}
#' \item{parity}{factor, `even` or `odd`.}
#' \item{highlow}{factor, `low` (1--18) or `high` (19--36).}
#' \item{dozens}{factor, dozen on the table: `1` (1--12), `2` (13--24),
#' `3` (25--36).}
#' \item{column}{factor, column on the table: `1`, `2` or `3`, counted
#' from the one containing 1.}
#' \item{pocketrange}{factor, sector of the wheel: `jeu zero`,
#' `voisins du zero`, `tiers du cylindre` or `orphelins`.}
#' }
#'
#' @details
#' The rows are ordered as the pockets follow each other on the wheel,
#' starting at zero, and not by `num`. The sectors in `pocketrange` are
#' therefore contiguous blocks of rows.
#'
#' Zero takes part in none of the even/odd, high/low, dozen and column
#' bets, so these variables are `NA` for zero. Note that this follows the
#' rules of the game and not arithmetic, where zero would count as even.
#'
#' The seven numbers of the *jeu zero* are part of the *voisins du zero*
#' in the usual reading of the terms. As every number appears exactly once
#' here, they are reported as a separate level and `voisins du zero`
#' covers the remaining ten numbers of that sector.
#'
#' Accents are dropped in the factor levels (`voisins du zero` for
#' *voisins du zéro*).
#'
#' @examples
#' head(Roulette)
#'
#' table(Roulette$col)
#' table(Roulette$parity, Roulette$highlow, useNA = "ifany")
#'
#' # the sectors of the wheel are blocks of neighbouring pockets
#' table(Roulette$pocketrange)
#'
#' @family datasets
#' @concept probability
#' @concept simulation
#'
#' @source The standard layout of a single-zero Roulette wheel.
"Roulette"

