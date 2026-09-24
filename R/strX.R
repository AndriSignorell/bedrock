

#' Extended str() with numbered variables
#'
#' Wrapper around [str()] that optionally numbers variables in
#' lists and data frames. Useful for large objects where variables should
#' be referenced by position.
#'
#' By default, only top-level elements are numbered. Recursive numbering
#' of nested list elements can be enabled with `recursive = TRUE`; nested
#' elements then get hierarchical labels such as `2.1`, which give their
#' position (`x[[2]][[1]]`), and the top-level numbers stay the same in both
#' views.
#'
#' @param object any R object.
#' @param ... additional arguments passed to [str()].
#' @param enumerate logical; whether variables or elements are numbered.
#'   Default is `TRUE`.
#' @param recursive logical; whether nested list elements are also numbered.
#'   Default is `FALSE`.
#' @param strict.width character string passed to [str()].
#'   Default is `"cut"`.
#'
#' @return
#' invisibly returns the character vector produced by [str()].
#'
#' @examples
#' # Data frame
#' strX(mtcars)
#'
#' # Nested list
#' x <- list(
#'   a = 1,
#'   b = list(
#'     c = 2,
#'     d = 3
#'   )
#' )
#'
#' strX(x)
#'
#' # Recursive numbering
#' strX(x, recursive = TRUE)
#' @seealso [str()]
#' @concept data-inspection
#' @concept introspection
#' @export
strX <- function(
    object,
    ...,
    enumerate = TRUE,
    recursive = FALSE,
    strict.width = "cut"
) {
  
  # ---- checks --------------------------------------------------------------
  
  # stopifnot(is.logical(), length() == 1) let NA through, which then failed
  # in if() with "missing value where TRUE/FALSE needed"
  for (nm in c("enumerate", "recursive")) {
    v <- get(nm)
    if (!is.logical(v) || length(v) != 1L || is.na(v))
      stop(gettextf("'%s' must be TRUE or FALSE", nm), call. = FALSE)
  }
  
  # ---- capture str output --------------------------------------------------
  
  out <- capture.output(
    str(
      object,
      ...,
      strict.width = strict.width
    )
  )
  
  # ---- numbering -----------------------------------------------------------
  
  if (
    enumerate &&
    (is.list(object) || is.data.frame(object))
  ) {
    
    # str() marks nesting with dots: top level " $ ", nested " ..$ ",
    # deeper "  .. ..$ " -- so the recursive pattern must allow dots
    idx <- if (recursive) {
      grep("^[ .]*\\$ ", out)
    } else {
      grep("^ \\$ ", out)
    }
    
    if (length(idx)) {
      
      # hierarchical labels 2, 2.1, 2.1.3 ... give the position of the element
      # (x[[2]][[1]][[3]]), a running number across levels would not: the
      # third top-level element became 6 in the recursive view. The nesting
      # depth is the number of ".." groups in the prefix.
      prefix <- sub("^([ .]*)\\$ .*$", "\\1", out[idx])
      depth  <- lengths(regmatches(prefix, gregexpr("..", prefix,
                                                   fixed = TRUE))) + 1L
      
      counter <- integer(max(depth))
      label   <- character(length(idx))
      for (i in seq_along(idx)) {
        d <- depth[i]
        counter[d] <- counter[d] + 1L
        counter[-seq_len(d)] <- 0L          # a new branch restarts below
        label[i] <- paste(counter[seq_len(d)], collapse = ".")
      }
      
      # right-align the labels of each level to a common width
      label <- ave(label, depth, FUN = function(s) formatC(s, width = max(nchar(s))))
      
      out[idx] <- mapply(
        FUN = function(line, lab) sub("^([ .]*)\\$", paste0("\\1", lab, " $"), line),
        line = out[idx],
        lab = label,
        USE.NAMES = FALSE
      )
    }
  }
  
  # ---- print ---------------------------------------------------------------
  
  cat(out, sep = "\n")
  
  invisible(out)
}

