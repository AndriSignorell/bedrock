
#' Pretty-print a character matrix with alignment, spacing and wrapping
#'
#' Prints a character matrix to the console with configurable alignment,
#' column spacing, optional row/column names, optional \pkg{cli}-based styling,
#' and automatic wrapping if the output exceeds the console width.
#'
#' @param m a matrix (or object coercible to a matrix) containing values that
#'   will be converted to character for display
#' @param align character string specifying alignment of cell contents.
#'   Either \code{"right"} (default) or \code{"left"}.
#' @param sep integer. Number of spaces between columns. Default is \code{2}.
#' @param showRownames logical. Should row names be printed? Default is \code{TRUE}.
#' @param showColnames logical. Should column names be printed? Default is \code{TRUE}.
#' @param useCliStyle logical. If \code{TRUE}, column names and row names are styled
#'   using \code{cli::style_bold()}. Default is \code{FALSE}.
#' @param width integer. Maximum output width (in characters). Defaults to
#'   \code{getOption("width")}. If the table exceeds this width, it is split into
#'   column blocks and printed in multiple sections.
#'
#' @details
#' The function formats all entries as character strings and computes column widths
#' dynamically. If the full table does not fit into the specified \code{width}, it is
#' split column-wise into multiple blocks. In this case, row names and column headers
#' are repeated for each block.
#'
#' Alignment is applied per column, and spacing between columns is controlled via
#' \code{sep}. The function is designed as a lightweight alternative to
#' \code{print.data.frame()} with more control over formatting, making it suitable
#' for reporting outputs in packages.
#'
#' @return
#' Invisibly returns \code{NULL}. The formatted table is printed to the console.
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
#' # Force wrapping by reducing width
#' printCharMatrix(m, width = 20)
#'
#' @family data.print
#' @concept formatting
#' @concept table
#' @export
printCharMatrix <- function(
    m,
    align = c("right", "left"),
    sep = 2,
    showRownames = TRUE,
    showColnames = TRUE,
    useCliStyle = FALSE,
    width = getOption("width")
) {
  align <- match.arg(align)

  if (useCliStyle && !requireNamespace("cli", quietly = TRUE)) {
    warning("Package 'cli' is not installed, falling back to plain output.",
            call. = FALSE)
    useCliStyle <- FALSE
  }

  m <- as.matrix(m)
  m[] <- as.character(m)
  
  rn <- rownames(m)
  cn <- colnames(m)
  
  # fallback if no names present
  if (is.null(rn)) rn <- rep("", nrow(m))
  if (is.null(cn)) cn <- rep("", ncol(m))
  
  # compute widths
  colWidths <- apply(m, 2, function(col) max(nchar(col), na.rm = TRUE))
  if (showColnames) {
    colWidths <- pmax(colWidths, nchar(cn))
  }
  
  rownameWidth <- if (showRownames) max(nchar(rn)) else 0
  
  padFun <- function(x, width) {
    if (align == "right") {
      formatC(x, width = width, format = "s")
    } else {
      sprintf(paste0("%-", width, "s"), x)
    }
  }
  
  # --- cli styling ---
  styleHeader <- function(x) {
    if (useCliStyle) cli::style_bold(x) else x
    # if (useCliStyle) cli::col_blue(x) else x
  }
  
  styleRowname <- function(x) {
  if (useCliStyle) cli::style_bold(x) else x
    #  if (useCliStyle) cli::col_blue(x) else x
  }
  
  # --- how many columns fit? ---
  sepStr <- paste(rep(" ", sep), collapse = "")
  
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
        header <- c(header, padFun("", rownameWidth))
      }
      
      header <- c(header, mapply(padFun, cn[cols], colWidths[cols]))
      
      headerLine <- paste(header, collapse = sepStr)
      cat(styleHeader(headerLine), "\n")
    }
    
    # Rows
    for (i in seq_len(nrow(m))) {
      row <- c()
      
      if (showRownames) {
        row <- c(row, padFun(rn[i], rownameWidth))
      }
      
      row <- c(row, mapply(padFun, m[i, cols], colWidths[cols]))
      
      line <- paste(row, collapse = sepStr)
      
      if (showRownames) {
        # style only the rowname
        if (useCliStyle) {
          rnPart <- padFun(rn[i], rownameWidth)
          rnPart <- cli::style_bold(rnPart)
          rest <- paste(mapply(padFun, m[i, cols], colWidths[cols]), 
                        collapse = sepStr)
          line <- paste(c(rnPart, rest), collapse = sepStr)
        }
      }
      
      cat(line, "\n")
    }
    
    cat("\n")
    
    colStart <- max(cols) + 1
  }

  invisible(NULL)
}



