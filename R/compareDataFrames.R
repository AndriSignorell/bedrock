
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
#'   \item{\code{only_in_x}}{Data frame of rows whose key value appears in
#'     \code{x} but not in \code{y}.}
#'   \item{\code{only_in_y}}{Data frame of rows whose key value appears in
#'     \code{y} but not in \code{x}.}
#'   \item{\code{diffs}}{Data frame with columns \code{strat_x} (the key value)
#'     and \code{diff_cols} (a list column of character vectors naming the
#'     differing columns for that key).}
#' }
#'
#' @examples
#' x <- data.frame(strat_x = c("A", "B", "C"), v1 = 1:3, v2 = c(10, 20, 30))
#' y <- data.frame(strat_x = c("A", "B", "D"), v1 = c(1L, 9L, 4L), v2 = c(10, 20, 40))
#' compareDataFrames(x, y)
#'


#' @export
compareDataFrames <- function(x, y, key = "strat_x") {
  
  # gemeinsame Spalten
  common_cols <- intersect(names(x), names(y))
  x <- x[common_cols]
  y <- y[common_cols]
  
  # sicherstellen: key vorhanden
  stopifnot(key %in% names(x), key %in% names(y))
  
  # fehlende Keys
  only_in_x <- x[!(x[[key]] %in% y[[key]]), , drop = FALSE]
  only_in_y <- y[!(y[[key]] %in% x[[key]]), , drop = FALSE]
  
  # auf gemeinsame Keys reduzieren
  common_keys <- intersect(x[[key]], y[[key]])
  x2 <- x[match(common_keys, x[[key]]), , drop = FALSE]
  y2 <- y[match(common_keys, y[[key]]), , drop = FALSE]
  
  # Vergleich ohne key
  cmp_cols <- setdiff(common_cols, key)
  diffs_list <- vector("list", length(common_keys))
  
  for (i in seq_along(common_keys)) {
    diffs_cols <- cmp_cols[!mapply(identical,
                                   x2[i, cmp_cols, drop = TRUE],
                                   y2[i, cmp_cols, drop = TRUE])]
    if (length(diffs_cols) > 0) {
      diffs_list[[i]] <- list(
        key = common_keys[i],
        diff_cols = diffs_cols
      )
    }
  }
  
  diffs_list <- Filter(Negate(is.null), diffs_list)
  
  # in data.frame umwandeln
  if (length(diffs_list) > 0) {
    diffs <- data.frame(
      strat_x = sapply(diffs_list, `[[`, "key"),
      diff_cols = I(lapply(diffs_list, `[[`, "diff_cols"))
    )
  } else {
    diffs <- data.frame(strat_x = character(0), diff_cols = I(list()))
  }
  
  list(
    identical = (nrow(only_in_x) == 0 &&
                   nrow(only_in_y) == 0 &&
                   nrow(diffs) == 0),
    only_in_x = only_in_x,
    only_in_y = only_in_y,
    diffs = diffs
  )
}

