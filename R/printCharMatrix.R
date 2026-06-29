
#' Pretty-print a character matrix with alignment, spacing and wrapping
#'
#' Prints a character matrix to the console with configurable alignment,
#' column spacing, optional row/column names, optional \pkg{cli}-based styling,
#' and automatic wrapping if the output exceeds the console width.
#'
#' @param m A matrix (or object coercible to a matrix) containing values that
#'   will be converted to character for display.
#' @param align Character string specifying alignment of cell contents.
#'   Either \code{"right"} (default) or \code{"left"}.
#' @param sep Integer. Number of spaces between columns. Default is \code{2}.
#' @param showRownames Logical. Should row names be printed? Default is \code{TRUE}.
#' @param showColnames Logical. Should column names be printed? Default is \code{TRUE}.
#' @param useCliStyle Logical. If \code{TRUE}, column names and row names are styled
#'   using \code{cli::style_bold()}. Default is \code{FALSE}.
#' @param width Integer. Maximum output width (in characters). Defaults to
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



#' @family table.utils  
#' @concept table
#'
#'
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
  
  m <- as.matrix(m)
  m[] <- as.character(m)
  
  rn <- rownames(m)
  cn <- colnames(m)
  
  # Fallback wenn keine Namen vorhanden
  if (is.null(rn)) rn <- rep("", nrow(m))
  if (is.null(cn)) cn <- rep("", ncol(m))
  
  # Breiten berechnen
  col_widths <- apply(m, 2, function(col) max(nchar(col), na.rm = TRUE))
  if (showColnames) {
    col_widths <- pmax(col_widths, nchar(cn))
  }
  
  rowname_width <- if (showRownames) max(nchar(rn)) else 0
  
  pad_fun <- function(x, width) {
    if (align == "right") {
      formatC(x, width = width, format = "s")
    } else {
      sprintf(paste0("%-", width, "s"), x)
    }
  }
  
  # --- CLI Styling ---
  style_header <- function(x) {
    if (useCliStyle) cli::style_bold(x) else x
    # if (useCliStyle) cli::col_blue(x) else x
  }
  
  style_rowname <- function(x) {
  if (useCliStyle) cli::style_bold(x) else x
    #  if (useCliStyle) cli::col_blue(x) else x
  }
  
  # --- Wie viele Spalten passen? ---
  sep_str <- paste(rep(" ", sep), collapse = "")
  
  calc_block <- function(start_col) {
    total <- if (showRownames) rowname_width + sep else 0
    cols <- c()
    
    for (j in start_col:ncol(m)) {
      w <- col_widths[j]
      needed <- if (length(cols) == 0) w else w + sep
      
      if ((total + needed) > width) break
      
      cols <- c(cols, j)
      total <- total + needed
    }
    cols
  }
  
  # --- Ausgabe ---
  col_start <- 1
  
  while (col_start <= ncol(m)) {
    
    cols <- calc_block(col_start)
    
    # Header
    if (showColnames) {
      header <- c()
      
      if (showRownames) {
        header <- c(header, pad_fun("", rowname_width))
      }
      
      header <- c(header, mapply(pad_fun, cn[cols], col_widths[cols]))
      
      header_line <- paste(header, collapse = sep_str)
      cat(style_header(header_line), "\n")
    }
    
    # Rows
    for (i in seq_len(nrow(m))) {
      row <- c()
      
      if (showRownames) {
        row <- c(row, pad_fun(rn[i], rowname_width))
      }
      
      row <- c(row, mapply(pad_fun, m[i, cols], col_widths[cols]))
      
      line <- paste(row, collapse = sep_str)
      
      if (showRownames) {
        # nur rowname fett machen
        if (useCliStyle) {
          rn_part <- pad_fun(rn[i], rowname_width)
          rn_part <- cli::style_bold(rn_part)
          rest <- paste(mapply(pad_fun, m[i, cols], col_widths[cols]), 
                        collapse = sep_str)
          line <- paste(c(rn_part, rest), collapse = sep_str)
        }
      }
      
      cat(line, "\n")
    }
    
    cat("\n")
    
    col_start <- max(cols) + 1
  }
}



