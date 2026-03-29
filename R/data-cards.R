
#' Playing cards dataset
#'
#' A dataset representing a standard deck of playing cards. Each row corresponds
#' to a single card and includes information such as suit, rank and numerical value.
#'
#' The dataset can be used for simulations, probability calculations and teaching
#' examples involving discrete outcomes and combinatorics.
#'
#' @format A data frame with 52 observations and X variables:
#' \describe{
#'   \item{card}{Name of the card.}
#'   \item{rank}{Rank of the card (e.g. 2–10, Jack, Queen, King, Ace).}
#'   \item{suit}{Suit of the card (e.g. hearts, diamonds, clubs, spades).}
#' }
#'
#' @details
#' The dataset represents a complete deck without jokers. It is suitable for
#' probability experiments, simulations and demonstrations of categorical data.
#'
#' @examples
#' head(cards)
#' table(cards$suit)
#'
#' @family datasets
#' @concept simulation
#' @concept probability
#'
#' @source Simulated data.
"cards"