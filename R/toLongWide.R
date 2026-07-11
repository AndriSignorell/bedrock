
#' Reshape Between Long and Wide Format
#'
#' Reshape data between long and wide format using a grouping variable.
#'
#' `toLong()` expects `x` to be a matrix, table, data frame, or list and
#' reshapes it to a long data frame representation. `toWide()` expects a vector
#' `x` and a grouping vector `groups`, and reshapes the values into one column
#' per group.
#'
#' @name long-wide-reshape
#'
#' @param x object to reshape. For `toLong()`, a matrix, table, data frame, or
#'   list. For `toWide()`, a vector.
#' @param groups grouping vector used to define the columns in the wide result
#' @param by optional vector used to align values row-wise when reshaping to
#'   wide format. If `NULL`, values are aligned by their order within each group.
#' @param varNames optional character vector of column names for the result
#' @param includeRowNames logical. If `TRUE`, append a column containing the row
#'   names of `x` when reshaping to long format.
#'
#' @return
#' A reshaped object of class `data.frame`.
#'
#' @seealso `reshape`, `stack`, `unstack`
#'
#' @examples
#' d.x <- read.table(header = TRUE, text = "
#' AA BB CC DD EE FF GG
#' 7.9 18.1 13.3 6.2 9.3 8.3 10.6
#' 9.8 14.0 13.6 7.9 2.9 9.1 13.0
#' 6.4 17.4 16.0 10.9 8.6 11.7 17.5
#' ")
#'
#' toLong(d.x)
#'
#' # to wide by row order
#' toWide(PlantGrowth$weight, PlantGrowth$group)
#'
#' # to wide aligned by key
#' set.seed(41)
#' PlantGrowth$nr <- c(sample(12, 10), sample(12, 10), sample(12, 10))
#' toWide(PlantGrowth$weight, PlantGrowth$group, by = PlantGrowth$nr)
#'


#' @family data.reshape
#' @concept reshape
#' @concept table
#' @export
toLong <- function(x, varNames = NULL, includeRowNames = FALSE) {
  if (!is.list(x)) {
    if (is.table(x)) {
      # as.data.frame() would return the long frequency form (Var1,
      # Var2, Freq) -- we need the rectangular structure
      if (length(dim(x)) != 2L)
        stop("only 2-dimensional tables are supported")
      x <- as.data.frame.matrix(x)
    } else if (is.matrix(x)) {
      x <- as.data.frame(x)
    }
    lst <- as.list(x)
  } else {
    lst <- x
  }
  
  groupNames <- names(lst)
  if (is.null(groupNames)) {
    groupNames <- paste0("X", seq_along(lst))
  }
  
  res <- data.frame(
    rep(groupNames, lengths(lst)),
    unlist(lst),
    stringsAsFactors = FALSE
  )
  
  rownames(res) <- NULL
  
  if (!is.null(rownames(x))) {
    rownames(res) <- do.call(
      paste,
      c(expand.grid(rownames(x), groupNames), sep = ".")
    )
  }
  
  if (includeRowNames && !is.null(rownames(x))) {
    res <- appendX(
      res,
      rep(rownames(x), times = length(lst)),
      after = 2
    )
  }
  
  if (is.null(varNames)) {
    varNames <- c("groups", "x", "rowNames")
  }
  
  colnames(res) <- varNames[seq_len(ncol(res))]
  
  res
}


#' @rdname long-wide-reshape
#' @family data.reshape
#' @concept reshape
#' @concept table
#' @export
toWide <- function(x, groups, by = NULL, varNames = NULL) {
  groups <- factor(groups)
  
  if (is.null(varNames)) {
    varNames <- levels(groups)
  }
  
  if (is.null(by)) {
    by <- "row.names"
  } else {
    x <- data.frame(x = x, idx = by)
    by <- "idx"
    varNames <- c("by", varNames)
  }
  
  s <- split(x, groups)
  
  if (by != "row.names") {
    for (i in seq_along(s)) {
      colnames(s[[i]])[1] <- names(s)[i]
    }
  }
  
  res <- Reduce(function(x, y) {
    if (inherits(x, "data.frame")) {
      if (colnames(x)[ncol(x)] != "idx") {
        colnames(x)[ncol(x)] <- paste0(colnames(x)[ncol(x) - 1L], "y")
      }
    }

    z <- merge(x, y, by = by, all.x = TRUE, all.y = TRUE)

    if (by == "row.names") {
      # keep the keys as rownames: merge() sorts them as character
      # ("1", "10", "2", ...) and dropping them would re-key the rows
      # 1:n in that order, misaligning every subsequent merge
      rownames(z) <- z[["Row.names"]]
      z <- z[, -grep("Row.names", names(z)), drop = FALSE]
    }

    z
  }, s)

  if (by == "row.names")
    res <- res[order(as.integer(rownames(res))), , drop = FALSE]

  colnames(res) <- varNames
  res
}

