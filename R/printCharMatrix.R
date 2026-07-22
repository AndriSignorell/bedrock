
#' Pretty-print a character matrix with alignment, spacing and column splitting
#'
#' Prints a character matrix to the console with configurable alignment,
#' column spacing, optional row/column names, optional \pkg{cli}-based styling,
#' and automatic splitting into column blocks if the output exceeds the console
#' width.
#'
#' @param m a matrix (or object coercible to a matrix) containing values that
#'   will be converted to character for display.
#' @param align character vector specifying alignment of cell contents,
#'   either \code{"right"} (default) or \code{"left"}. A single value is
#'   recycled across all columns; alternatively a vector of length
#'   \code{ncol(m)} sets the alignment per column.
#' @param sep integer. Number of spaces between columns. Default is \code{2}.
#' @param showRownames logical. Should row names be printed? Default is \code{TRUE}.
#' @param showColnames logical. Should column names be printed? Default is \code{TRUE}.
#' @param useCliStyle logical. If \code{TRUE}, column names and row names are styled
#'   using \code{cli::style_bold()}. Default is \code{FALSE}.
#' @param width integer. Maximum output width (in characters). Defaults to
#'   \code{getOption("width")}. If the table exceeds this width, it is split into
#'   column blocks that are printed one after another.
#'
#' @details
#' The function formats all entries as character strings and computes column widths
#' dynamically. \code{NA} entries are shown as \code{"NA"}. If the full table does
#' not fit into the specified \code{width}, it is split column-wise into multiple
#' blocks (cell contents themselves are never wrapped). In this case, row names and
#' column headers are repeated for each block.
#'
#' If a single column is wider than \code{width}, that column is printed on its own
#' and the requested \code{width} is deliberately exceeded, since a column cannot be
#' split further.
#'
#' Alignment is applied per column, and spacing between columns is controlled via
#' \code{sep}. The function is designed as a lightweight alternative to
#' \code{print.data.frame()} with more control over formatting, making it suitable
#' for reporting outputs in packages.
#'
#' @return
#' invisibly returns \code{NULL}. The formatted table is printed to the console.
#'
#' @examples
#' m <- matrix(c(
#'   "50.575","50.543","45.207",
#'   "49.900","51.400","44.300",
#'   "5.106","8.192","10.197"
#' ), nrow = 3, byrow = TRUE)
#'
#' rownames(m) <- c("mean","median","sd")
#' colnames(m) <- c("Brent","Camden","Westminster")
#'
#' # Default (right-aligned)
#' printCharMatrix(m)
#'
#' # Left-aligned with custom spacing
#' printCharMatrix(m, align = "left", sep = 4)
#'
#' # With CLI styling (requires cli package)
#' if (requireNamespace("cli", quietly = TRUE)) {
#'   printCharMatrix(m, useCliStyle = TRUE)
#' }
#'
#' # Force splitting into column blocks by reducing width
#' printCharMatrix(m, width = 20)
#'
#' @family data.print
#' @concept formatting
#' @concept table
#' @export
printCharMatrix <- function(
    m,
    align = "right",
    sep = 2,
    showRownames = TRUE,
    showColnames = TRUE,
    useCliStyle = FALSE,
    width = getOption("width")
) {
  # --- argument validation ---
  if (!is.numeric(sep) || length(sep) != 1L || !is.finite(sep) ||
      sep < 0 || sep != floor(sep)) {
    stop("'sep' must be a single non-negative integer.", call. = FALSE)
  }
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) ||
      width < 1 || width != floor(width)) {
    stop("'width' must be a single positive integer.", call. = FALSE)
  }
  for (arg in c("showRownames", "showColnames", "useCliStyle")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("'%s' must be TRUE or FALSE.", arg), call. = FALSE)
    }
  }

  # align may be a single value (recycled over all columns) or a per-column
  # vector of length ncol(m); defaults to "right". Validated element-wise
  # with partial matching, preserving the given order.
  align <- vapply(align, match.arg,
                  FUN.VALUE = character(1),
                  choices = c("right", "left"),
                  USE.NAMES = FALSE)

  if (useCliStyle && !requireNamespace("cli", quietly = TRUE)) {
    warning("Package 'cli' is not installed, falling back to plain output.",
            call. = FALSE)
    useCliStyle <- FALSE
  }

  m <- as.matrix(m)
  m[] <- as.character(m)
  
  # recycle align to one entry per column
  if (length(align) == 1L) {
    align <- rep(align, ncol(m))
  } else if (length(align) != ncol(m)) {
    stop("'align' must be length 1 or equal to the number of columns.",
         call. = FALSE)
  }
  
  rn <- rownames(m)
  cn <- colnames(m)
  
  # only print names that actually exist
  hasRownames <- !is.null(rn)
  hasColnames <- !is.null(cn)
  showRownames <- showRownames && hasRownames
  showColnames <- showColnames && hasColnames
  
  # fallback placeholders for absent names
  if (is.null(rn)) rn <- rep("", nrow(m))
  if (is.null(cn)) cn <- rep("", ncol(m))
  
  # NA entries in the dimnames themselves are shown as "NA" too
  rn[is.na(rn)] <- "NA"
  cn[is.na(cn)] <- "NA"
  
  # NA cells are displayed as "NA"; work off this copy from here on so a
  # fully-NA column has a defined width instead of -Inf
  displayMatrix <- m
  displayMatrix[is.na(displayMatrix)] <- "NA"
  
  # compute widths (nchar type = "width" is Unicode-aware); the leading 0L
  # guards empty columns / zero-row matrices against max(numeric(0)) = -Inf
  colWidths <- apply(displayMatrix, 2L, function(x)
    max(c(0L, nchar(x, type = "width"))))
  if (showColnames) {
    colWidths <- pmax(colWidths, nchar(cn, type = "width"))
  }
  
  rownameWidth <- if (showRownames) {
    max(c(0L, nchar(rn, type = "width")))
  } else {
    0L
  }
  
  padFun <- function(x, width, align = "right") {
    if (align == "right") {
      formatC(x, width = width, format = "s")
    } else {
      sprintf(paste0("%-", width, "s"), x)
    }
  }
  
  # --- cli styling ---
  styleHeader <- function(x) {
    if (useCliStyle) cli::style_bold(x) else x
  }
  
  # --- how many columns fit? ---
  sepStr <- strrep(" ", sep)
  
  calcBlock <- function(startCol) {
    total <- if (showRownames) rownameWidth + sep else 0
    cols <- c()
    
    for (j in startCol:ncol(m)) {
      w <- colWidths[j]
      needed <- if (length(cols) == 0) w else w + sep
      
      if ((total + needed) > width) break
      
      cols <- c(cols, j)
      total <- total + needed
    }

    # a single column wider than `width` would yield an empty block and
    # send the outer loop into max(cols) = -Inf; print it overwide instead
    if (length(cols) == 0L)
      cols <- startCol

    cols
  }
  
  # --- output ---
  colStart <- 1
  
  while (colStart <= ncol(m)) {
    
    cols <- calcBlock(colStart)
    
    # Header
    if (showColnames) {
      header <- c()
      
      if (showRownames) {
        header <- c(header, padFun("", rownameWidth, "left"))
      }
      
      header <- c(header, mapply(padFun, cn[cols], colWidths[cols], align[cols]))
      
      headerLine <- paste(header, collapse = sepStr)
      cat(styleHeader(headerLine), "\n", sep = "")
    }
    
    # Rows
    for (i in seq_len(nrow(displayMatrix))) {
      # format the data cells once
      cells <- mapply(padFun, displayMatrix[i, cols], colWidths[cols], align[cols])
      dataStr <- paste(cells, collapse = sepStr)
      
      if (showRownames) {
        rnPart <- padFun(rn[i], rownameWidth, "left")
        if (useCliStyle) rnPart <- cli::style_bold(rnPart)
        line <- paste(c(rnPart, dataStr), collapse = sepStr)
      } else {
        line <- dataStr
      }
      
      cat(line, "\n", sep = "")
    }
    
    colStart <- max(cols) + 1
    
    # blank line only between blocks, not after the last one
    if (colStart <= ncol(m)) cat("\n")
  }

  invisible(NULL)
}



