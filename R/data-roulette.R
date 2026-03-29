

#' Roulette wheel dataset
#'
#' A dataset representing the numbers on a European roulette wheel and their
#' associated properties such as color, parity and positional groupings.
#'
#' Each row corresponds to a single number on the wheel (0–36), including
#' commonly used betting categories.
#'
#' The dataset is suitable for teaching probability, categorical data analysis
#' and simulating betting strategies.
#'
#' @format A data frame with 37 observations and 8 variables:
#' \describe{
#'   \item{num}{Roulette number (0–36).}
#'   \item{col}{Color of the number (red, black, green).}
#'   \item{parity}{Parity of the number (even or odd, NA for zero).}
#'   \item{highlow}{Indicator for low (1–18) or high (19–36), NA for zero.}
#'   \item{dozens}{Dozen grouping (1 = 1–12, 2 = 13–24, 3 = 25–36, NA for zero).}
#'   \item{column}{Column grouping on the roulette table (1, 2, 3, NA for zero).}
#'   \item{pocketrange}{Wheel sector grouping (e.g. jeu zero, voisins du zero, orphelins, tiers du cylindre).}
#' }
#'
#' @details
#' The dataset represents a European roulette wheel (single zero). The number
#' zero is treated as a special case and has missing values for several derived
#' variables such as parity, high/low, dozens and column.
#'
#' The variable \code{pocketrange} reflects traditional French wheel sectors
#' used in roulette betting systems.
#'
#' @examples
#' head(roulette)
#' table(roulette$col)
#' table(roulette$parity, useNA = "ifany")
#'
#' @family datasets
#' @concept probability
#' @concept simulation
#'
#' @source Simulated data based on a European roulette wheel.
"roulette"

