
#' Sort Vectors, Matrices, Tables, and Data Frames
#'
#' \code{sortX} extends the base \code{\link{sort}} function by providing
#' a consistent interface for sorting not only vectors, but also matrices,
#' tables, and data frames. For 2-dimensional objects, rows are sorted based
#' on one or more columns.
#'
#' By default, sorting follows the behavior of base R. In addition,
#' \code{method = "mixed"} enables natural ("human-friendly") sorting of
#' character data, e.g. \code{"A2"} < \code{"A10"}.
#'
#' For \code{method = "mixed"}, sorting is applied column-wise using
#' \code{.orderMixed()}. Each column's tokens are ordered independently
#' (numeric runs numerically, text runs lexicographically) before the
#' results are combined via stable right-to-left ordering.
#'
#' The sort order for factors depends on \code{factorsAsCharacter}:
#' if \code{TRUE} (default), factors are sorted by their labels
#' (alphabetically or by natural order when \code{method = "mixed"});
#' if \code{FALSE}, they are sorted by their level order, which is
#' appropriate for ordered factors but may be unintuitive for unordered ones.
#'
#' @name sortX
#' @aliases sortX sortX.default sortX.data.frame sortX.matrix sortX.table
#'
#' @param x A numeric, complex, character or logical vector, factor,
#'   matrix, table, or data frame to be sorted.
#' @param decreasing Logical scalar or vector. Should the sort be in
#'   decreasing order? For 2-dimensional objects a vector of the same length
#'   as \code{ord} may be supplied to control the direction per column;
#'   a scalar is recycled.
#' @param na.last Logical or \code{NA}. Should missing values be placed last
#'   (\code{TRUE}), first (\code{FALSE}), or removed (\code{NA})?
#'   See \code{\link{order}}.
#' @param method Sorting method. Either \code{"default"} (base R behavior)
#'   or \code{"mixed"} for natural sorting of character data
#'   (e.g. \code{"A2"} < \code{"A10"}).
#' @param factorsAsCharacter Logical. If \code{TRUE} (default), factors are
#'   converted to character before sorting so that labels are used instead of
#'   level codes. Set to \code{FALSE} to sort by level order (useful for
#'   ordered factors).
#' @param ord Integer or character vector specifying the columns to sort by,
#'   and their priority (first element = primary key). Column names and
#'   positive integer indices (\code{1:ncol(x)}) refer to columns.
#'   The special value \code{0L} (integer zero, always numeric) sorts by row
#'   names. For \code{table} and \code{matrix} objects, \code{ncol(x) + 1L}
#'   sorts by row marginal sums. This argument is not available for
#'   \code{sortX.default}. Default: \code{NULL} (all columns, left to right).
#' @param \dots Further arguments passed to \code{\link{sort}} in
#'   \code{sortX.default}.
#'
#' @return The sorted object, of the same class as \code{x}.
#'
#' @seealso \code{\link{sort}}, \code{\link{order}}
#'
#' @examples
#' set.seed(3)
#' d.frm <- iris[sample(nrow(iris), 10),
#'               c("Species", "Sepal.Length", "Sepal.Width")]
#'
#' # Vector sorting
#' sortX(d.frm[, 1])
#'
#' # Data frame: sort by column name
#' sortX(d.frm, ord = "Species")
#' sortX(d.frm, ord = c("Species", "Sepal.Length"))
#'
#' # Data frame: sort by column index
#' sortX(d.frm, ord = c(1L, 2L))
#'
#' # Decreasing order (per-column control)
#' sortX(d.frm, ord = c("Species", "Sepal.Length"),
#'       decreasing = c(FALSE, TRUE))
#'
#' # Natural sorting of character vectors
#' x <- c("A1", "A10", "A2")
#' sortX(x, method = "mixed")
#'
#' # Factor: sort by label (default) vs. level order
#' sortX(d.frm, ord = "Species")                          # by label
#' sortX(d.frm, ord = "Species", factorsAsCharacter = FALSE)  # by level
#'
#' # Tables: sort by column 2 descending
#' tab <- HairEyeColor[, , 1]
#' sortX(tab, ord = 2L, decreasing = TRUE)
#'
#' # Tables: sort by marginal row sums
#' sortX(tab, ord = ncol(tab) + 1L, decreasing = TRUE)
#'
#' # Sort by row names (always pass 0 as integer)
#' sortX(tab, ord = 0L)
#'


#' @rdname sortX
#' @export
sortX <- function(x, ...) {
  UseMethod("sortX")
}


# ----------------------------------------------------------------------
#  sortX.default  --  vectors and any class without a specific method
# ----------------------------------------------------------------------

#' @rdname sortX
#' @export
sortX.default <- function(x,
                          decreasing         = FALSE,
                          na.last            = NA,
                          method             = c("default", "mixed"),
                          factorsAsCharacter = TRUE,
                          ...) {
  method <- match.arg(method)
  
  # Optionally convert factor to character before sorting
  y <- if (is.factor(x) && factorsAsCharacter) as.character(x) else x
  
  if (method == "mixed") {
    return(x[.orderMixed(y, decreasing = decreasing, na.last = na.last)])
  }
  
  sort(x = x, decreasing = decreasing, na.last = na.last, ...)
}


# ----------------------------------------------------------------------
#  sortX.table
# ----------------------------------------------------------------------

#' @rdname sortX
#' @export
sortX.table <- function(x,
                        ord                = NULL,
                        decreasing         = FALSE,
                        na.last            = TRUE,
                        method             = c("default", "mixed"),
                        factorsAsCharacter = TRUE,
                        ...) {
  method    <- match.arg(method)
  row_order <- .sortX_core(x, ord, decreasing, na.last, method,
                           factorsAsCharacter = factorsAsCharacter,
                           allow_marginal     = TRUE)
  x[row_order, , drop = FALSE]
}


# ----------------------------------------------------------------------
#  sortX.matrix
# ----------------------------------------------------------------------

#' @rdname sortX
#' @export
sortX.matrix <- function(x,
                         ord                = NULL,
                         decreasing         = FALSE,
                         na.last            = TRUE,
                         method             = c("default", "mixed"),
                         factorsAsCharacter = TRUE,
                         ...) {
  method    <- match.arg(method)
  row_order <- .sortX_core(x, ord, decreasing, na.last, method,
                           factorsAsCharacter = factorsAsCharacter,
                           allow_marginal     = TRUE)
  x[row_order, , drop = FALSE]
}


# ----------------------------------------------------------------------
#  sortX.data.frame
# ----------------------------------------------------------------------

#' @rdname sortX
#' @export
sortX.data.frame <- function(x,
                             ord                = NULL,
                             decreasing         = FALSE,
                             na.last            = TRUE,
                             method             = c("default", "mixed"),
                             factorsAsCharacter = TRUE,
                             ...) {
  method    <- match.arg(method)
  row_order <- .sortX_core(x, ord, decreasing, na.last, method,
                           factorsAsCharacter = factorsAsCharacter,
                           allow_marginal     = FALSE)  # row sums not supported
  x[row_order, , drop = FALSE]
}


# == internal helper functions  ==============================================

# ----------------------------------------------------------------------
#  .orderMixed  --  natural / alphanumeric ordering
#
#  Returns an integer index vector analogous to order().
#  Non-character input is delegated directly to order().
#  Character strings are split into alternating numeric and text tokens;
#  numeric runs are sorted numerically, text runs lexicographically
#  (case-insensitive).
# ----------------------------------------------------------------------
.orderMixed <- function(x,
                        decreasing = FALSE,
                        na.last    = TRUE) {
  
  n <- length(x)
  if (n <= 1L) return(seq_len(n))
  
  # Non-character: delegate to base order()
  if (!is.character(x)) {
    return(order(x, decreasing = decreasing, na.last = na.last))
  }
  
  # Split each string into alternating text/digit tokens
  split_fun <- function(s) {
    parts <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1]]
    lapply(parts, function(p) {
      if (grepl("^[0-9]+$", p)) as.numeric(p) else tolower(p)
    })
  }
  
  parsed  <- lapply(x, split_fun)
  max_len <- max(lengths(parsed))
  
  # Pad all token lists to the same length (missing slots become NULL)
  parsed <- lapply(parsed, function(p) {
    length(p) <- max_len
    p
  })
  
  # Build one sort key per token position
  cols <- vector("list", max_len)
  
  for (j in seq_len(max_len)) {
    # lapply keeps length n; NULL marks a padded (missing) slot
    col <- lapply(parsed, function(p) p[[j]])
    
    if (all(vapply(col,
                   function(v) is.null(v) || (length(v) == 1L && is.numeric(v)),
                   logical(1)))) {
      # All present values are numeric -> numeric key; NULL -> NA
      cols[[j]] <- as.numeric(vapply(col,
                                     function(v) if (is.null(v)) NA_real_ else v,
                                     numeric(1)))
    } else {
      # Mixed or text column -> factor key; NULL -> NA
      cols[[j]] <- as.factor(vapply(col,
                                    function(v) if (is.null(v)) NA_character_
                                    else as.character(v),
                                    character(1)))
    }
  }
  
  do.call(order, c(cols, list(decreasing = decreasing, na.last = na.last)))
}


# ----------------------------------------------------------------------
#  .sortX_core  --  shared row-ordering logic for 2-d objects
#
#  Returns an integer index vector of length nrow(x).
#
#  Parameters
#    x                 matrix, table, or data.frame
#    ord               column selector (NULL, integer, or character)
#    decreasing        logical scalar or vector (recycled to length(ord))
#    na.last           passed to order() / .orderMixed()
#    method            "default" or "mixed"
#    factorsAsCharacter convert factor columns to character before sorting
#    allow_marginal    if FALSE, ord = ncol(x)+1 raises an error
# ----------------------------------------------------------------------
.sortX_core <- function(x,
                        ord,
                        decreasing,
                        na.last,
                        method,
                        factorsAsCharacter = TRUE,
                        allow_marginal     = TRUE) {
  
  nc <- ncol(x)
  nr <- nrow(x)
  
  # --- resolve column names to integer indices --------------------------
  # ord = 0L (row names) must always be passed as integer and is never
  # a column name, so name resolution only touches non-zero values.
  if (is.character(ord)) {
    idx <- match(ord, colnames(x))
    unknown <- ord[is.na(idx)]
    if (length(unknown) > 0L)
      stop("Unknown column name(s) in 'ord': ",
           paste(unknown, collapse = ", "))
    ord <- idx
  }
  
  # --- default ord: all columns left to right ---------------------------
  if (is.null(ord)) ord <- seq_len(nc)
  
  # --- recycle decreasing -----------------------------------------------
  if (length(decreasing) == 1L) {
    decreasing <- rep(decreasing, length(ord))
  }
  if (length(decreasing) != length(ord))
    stop("'decreasing' must have length 1 or length(ord).")
  
  # --- validate ord values ----------------------------------------------
  valid <- (ord >= 0L) & (ord <= nc + 1L)
  if (!all(valid))
    stop(sprintf("Invalid value(s) in 'ord': %s. Allowed: 0L..%dL.",
                 paste(ord[!valid], collapse = ", "), nc + 1L))
  
  # --- guard marginal sums for data.frame --------------------------------
  if (!allow_marginal && any(ord == nc + 1L))
    stop("ord = ncol(x)+1L (marginal row sums) is not supported for ",
         "data.frame because not all columns need to be numeric.")
  
  # --- build sort keys --------------------------------------------------
  keys <- vector("list", length(ord))
  
  for (i in seq_along(ord)) {
    col_i <- ord[i]
    dec_i <- decreasing[i]
    
    if (col_i == 0L) {
      # Sort by row names
      rn <- rownames(x)
      if (is.null(rn)) {
        warning("x has no rownames; ord = 0L falls back to natural row order.")
        rn <- as.character(seq_len(nr))
      }
      keys[[i]] <- rn
      
    } else if (col_i == nc + 1L) {
      # Sort by marginal row sums (table / matrix only)
      keys[[i]] <- rowSums(x, na.rm = TRUE)
      
    } else {
      # Regular column
      col_val <- x[, col_i]
      # Optionally treat factors as character
      if (is.factor(col_val) && factorsAsCharacter)
        col_val <- as.character(col_val)
      keys[[i]] <- col_val
    }
    
    # For "default": invert key to achieve decreasing order, because
    # do.call(order, ...) does not support per-key decreasing.
    # For "mixed": decreasing is passed directly to .orderMixed().
    if (method == "default" && dec_i) {
      if (is.numeric(keys[[i]])) {
        keys[[i]] <- -keys[[i]]
      } else {
        keys[[i]] <- -xtfrm(keys[[i]])
      }
    }
  }
  
  # --- compute row order ------------------------------------------------
  switch(
    method,
    
    "default" = {
      do.call(order, c(keys, list(na.last = na.last)))
    },
    
    "mixed" = {
      # Stable sort: apply .orderMixed() from last key to first key.
      # Keys have NOT been negated in this branch; decreasing is forwarded.
      idx <- seq_len(nr)
      for (k in rev(seq_along(keys))) {
        idx <- idx[.orderMixed(keys[[k]][idx],
                               decreasing = decreasing[k],
                               na.last    = na.last)]
      }
      idx
    },
    
    stop(sprintf("Unknown method '%s'. Use 'default' or 'mixed'.", method))
  )
}


