
#' Compare Two Data Frames by Key Column
#'
#' Compares two data frames row-by-row based on a key column, identifying rows
#' present in only one of the two frames and columns that differ in matched rows.
#'
#' @param x A data frame.
#' @param y A data frame to compare against \code{x}.
#' @param key Character string. Name of the column used as row identifier.
#'   Must be present in both \code{x} and \code{y}. Default is \code{"strat_x"}.
#'
#' @details
#' Only columns present in both data frames are compared. Rows are matched by
#' the \code{key} column using \code{\link{identical}} for element-wise comparison,
#' so type differences (e.g., \code{integer} vs. \code{double}) will be flagged
#' as differences.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{\code{identical}}{Logical. \code{TRUE} if the two data frames are
#'     identical with respect to the common columns and key.}
#'   \item{\code{onlyInX}}{Data frame of rows whose key value appears in
#'     \code{x} but not in \code{y}.}
#'   \item{\code{onlyInY}}{Data frame of rows whose key value appears in
#'     \code{y} but not in \code{x}.}
#'   \item{\code{diffs}}{Data frame with columns named after the \code{key}
#'     argument (the key value) and \code{diffCols} (a list column of character
#'     vectors naming the differing columns for that key).}
#' }
#'
#' @examples
#' x <- data.frame(strat_x = c("A", "B", "C"), v1 = 1:3, v2 = c(10, 20, 30))
#' y <- data.frame(strat_x = c("A", "B", "D"), v1 = c(1L, 9L, 4L), v2 = c(10, 20, 40))
#' compareDataFrames(x, y)
#'



#' @family data.manipulation  
#' @concept table
#'
#'
#' @export
compareDataFrames <- function(x, y, key = "strat_x") {
  
  # gemeinsame Spalten
  commonCols <- intersect(names(x), names(y))
  x <- x[commonCols]
  y <- y[commonCols]
  
  # sicherstellen: key vorhanden
  stopifnot(key %in% names(x), key %in% names(y))
  
  # fehlende Keys
  onlyInX <- x[!(x[[key]] %in% y[[key]]), , drop = FALSE]
  onlyInY <- y[!(y[[key]] %in% x[[key]]), , drop = FALSE]
  
  # auf gemeinsame Keys reduzieren
  commonKeys <- intersect(x[[key]], y[[key]])
  x2 <- x[match(commonKeys, x[[key]]), , drop = FALSE]
  y2 <- y[match(commonKeys, y[[key]]), , drop = FALSE]
  
  # Vergleich ohne key
  cmpCols <- setdiff(commonCols, key)
  diffsList <- vector("list", length(commonKeys))
  
  for (i in seq_along(commonKeys)) {
    diffCols <- cmpCols[!mapply(identical,
                                x2[i, cmpCols, drop = TRUE],
                                y2[i, cmpCols, drop = TRUE])]
    if (length(diffCols) > 0) {
      diffsList[[i]] <- list(
        key      = commonKeys[i],
        diffCols = diffCols
      )
    }
  }
  
  diffsList <- Filter(Negate(is.null), diffsList)
  
  # in data.frame umwandeln
  if (length(diffsList) > 0) {
    diffs <- data.frame(
      key      = sapply(diffsList, `[[`, "key"),
      diffCols = I(lapply(diffsList, `[[`, "diffCols"))
    )
    names(diffs)[1] <- key
  } else {
    diffs <- data.frame(
      key      = character(0),
      diffCols = I(list())
    )
    names(diffs)[1] <- key
  }
  
  list(
    identical = (nrow(onlyInX) == 0 &&
                   nrow(onlyInY) == 0 &&
                   nrow(diffs)  == 0),
    onlyInX = onlyInX,
    onlyInY = onlyInY,
    diffs   = diffs
  )
}
