
#' Collapse Table Dimensions by Remapping Factor Levels
#'
#' Aggregates a \code{table} or \code{ftable} object by reassigning the levels
#' of one or more dimensions according to user-supplied mappings, and summing
#' the frequencies within each resulting level combination.
#'
#' @param x A \code{table} or \code{ftable} object with named dimensions.
#' @param ... Named or unnamed mapping vectors specifying how levels of each
#'   dimension should be collapsed. Each mapping vector must have length equal
#'   to the number of levels in the corresponding dimension.
#' @param strict Logical (default \code{TRUE}). Controls error handling.
#'
#' @details
#' Mapping vectors define how factor levels are reassigned. Element \code{i}
#' specifies the new label for the \code{i}-th original level. Repeated values
#' in a mapping vector cause the corresponding levels to be merged.
#'
#' \strong{Argument matching}
#' \itemize{
#'   \item Named arguments are matched to dimensions by name
#'   (e.g., \code{age = c("young", "adult")}).
#'   \item Unnamed arguments are assigned to dimensions in order.
#'   \item Mixed usage assigns named arguments first, then remaining unnamed
#'   arguments in order.
#' }
#'
#' \strong{Constraints}
#' \itemize{
#'   \item Each dimension may be specified at most once.
#'   \item Mapping vectors must have the same length as the number of levels
#'   of the corresponding dimension.
#'   \item \code{NA} values in mapping vectors are not allowed.
#' }
#'
#' \strong{Level ordering}
#' The order of the resulting levels follows the first occurrence of each value
#' in the mapping vector.
#'
#' \strong{Error handling}
#' \itemize{
#'   \item If \code{strict = TRUE}, unknown dimension names result in an error,
#'   and positional assignment of unnamed arguments produces a warning.
#'   \item If \code{strict = FALSE}, unknown dimensions produce a warning
#'   (and are ignored), and positional assignment is silent.
#' }
#'
#' @return A collapsed \code{table} object with updated dimensions and
#'   aggregated frequencies.
#'
#' @examples
#' # Example usage (assuming suitable table 'tab')
#' # collapseTable(tab, age = c("young", "adult", "adult"))
#'
#' @family table.utils
#' @concept table-manipulation
#' @concept factor-handling
#'


#' @export
collapseTable <- function(x, ..., strict = TRUE) {
  
  # --- 1. Input checks ---
  if (inherits(x, "ftable")) {
    x <- as.table(x)
  }
  
  if (!inherits(x, "table")) {
    stop("Argument 'x' must be a table or ftable object")
  }
  
  args   <- list(...)
  n_args <- length(args)
  
  if (n_args == 0L) {
    return(x)
  }
  
  tvars <- names(dimnames(x))
  if (is.null(tvars)) {
    stop("Table must have named dimensions")
  }
  
  if (n_args > length(tvars)) {
    stop(sprintf(
      "%d argument(s) passed but table has only %d dimension(s)",
      n_args, length(tvars)
    ))
  }
  
  # --- 2. Resolve argument names ---
  arg_names <- names(args)
  
  if (is.null(arg_names)) {
    # All unnamed: assign positionally; warn only in strict mode
    if (strict) warning("All arguments are unnamed; assigning to dimensions by position")
    names(args) <- tvars[seq_len(n_args)]
  } else {
    missing_idx <- which(!nzchar(arg_names))
    
    if (length(missing_idx) > 0L) {
      unused <- setdiff(tvars, arg_names[nzchar(arg_names)])
      
      if (length(unused) < length(missing_idx)) {
        stop("Not enough table dimensions for unnamed arguments")
      }
      
      names(args)[missing_idx] <- unused[seq_along(missing_idx)]
      
      warning("Unnamed argument(s) assigned to dimension(s) by position: ",
              paste(names(args)[missing_idx], collapse = ", "))
    }
  }
  
  # --- 3. Convert table to long data.frame ---
  df <- as.data.frame.table(x, responseName = "Freq", stringsAsFactors = TRUE)
  
  # as.data.frame.table() sanitizes column names via make.names()
  # (e.g. "age group" -> "age.group"). We keep a mapping so all subsequent
  # df operations use the sanitized names, while tvars retains the originals
  # for the final dimnames restoration.
  df_tvars <- make.names(tvars)                          # sanitized names in df
  tvar_to_df <- setNamesX(df_tvars, tvars)                # original -> sanitized
  
  # Snapshot original levels BEFORE any modifications (keyed by original name)
  orig_levels <- setNamesX(
    lapply(df_tvars, function(v) levels(df[[v]])),
    tvars
  )
  
  # --- 4. Apply level remapping ---
  level_map <- list()   # keyed by original (tvars) name
  
  for (i in seq_len(n_args)) {
    
    var  <- names(args)[i]    # original dimension name
    vals <- args[[i]]
    
    if (!(var %in% tvars)) {
      msg <- sprintf("Dimension '%s' not found in table", var)
      if (strict) stop(msg) else { warning(msg); next }
    }
    
    dvar <- tvar_to_df[[var]] # sanitized column name in df (safe after check)
    
    f <- df[[dvar]]
    
    if (!is.factor(f)) {
      stop(sprintf("Column '%s' is unexpectedly not a factor", dvar))
    }
    
    nlev <- nlevels(f)
    
    if (length(vals) != nlev) {
      stop(sprintf(
        "Mapping for '%s' has length %d but dimension has %d level(s)",
        var, length(vals), nlev
      ))
    }
    
    if (anyNA(vals)) {
      stop(sprintf("NA values in mapping for '%s' are not allowed", var))
    }
    
    # Map old levels -> new labels (explicit integer index, no match() ambiguity)
    new_vals   <- vals[as.integer(f)]
    new_levels <- vals[!duplicated(vals)]   # first-occurrence order
    
    df[[dvar]]       <- factor(new_vals, levels = new_levels)
    level_map[[var]] <- new_levels
  }
  
  # --- 5. Aggregate ---
  res_df <- aggregate(
    Freq ~ .,
    data = df[, c(df_tvars, "Freq"), drop = FALSE],
    FUN  = sum
  )
  
  # --- 6. Restore correct level ordering ---
  # aggregate() drops factor information; re-apply from level_map (collapsed
  # dims) or orig_levels (untouched dims), both keyed by original name.
  for (i in seq_along(tvars)) {
    v    <- tvars[i]
    dv   <- df_tvars[i]
    lvls <- if (!is.null(level_map[[v]])) level_map[[v]] else orig_levels[[v]]
    res_df[[dv]] <- factor(res_df[[dv]], levels = lvls)
  }
  
  # --- 7. Reconstruct table via xtabs ---
  # Use sanitized names for the formula (they are valid R names), then restore
  # the original dimension names via names(dimnames()).
  safe_vars <- paste0("`", df_tvars, "`")
  fml       <- as.formula(paste("Freq ~", paste(safe_vars, collapse = "+")))
  
  res <- xtabs(fml, data = res_df, drop.unused.levels = FALSE)
  
  # Guard against NA cells for zero-count combinations absent from res_df
  res[is.na(res)] <- 0
  
  # Remove xtabs-specific attributes so result is a plain table
  class(res)      <- setdiff(class(res), "xtabs")
  attr(res, "call") <- NULL
  
  # Restore original dimension names (undoes make.names() sanitization)
  names(dimnames(res)) <- tvars
  
  res
}
