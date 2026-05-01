
#' Parse SAS DATALINES/CARDS blocks into a data.frame
#'
#' A parser for simple SAS dataline command texts. A \code{data.frame} is being
#' built with the columnnames listed in the input section. 
#' 
#' The SAS function \code{DATA} is designed for quickly creating a dataset from
#' scratch. The whole step normally consists out of the \code{DATA} part
#' defining the name of the dataset, an \code{INPUT} line declaring the
#' variables and a \code{DATALINES} command followed by the values.\cr The
#' default delimiter used to separate the different variables is a space (thus
#' each variable should be one word). The $ after the variable name indicates
#' that the variable preceding contain character values and not numeric values.
#' Without specific instructions, SAS assumes that variables are numeric. The
#' function will fail, if it encounters a character in the place of an expected
#' numeric value.\cr Each new row in datalines will create a corresponding
#' unique row in the dataset. Notice that a ; is not needed after every row,
#' rather it is included at the end of the entire data step.
#' 
#' More complex command structures, i.e. other delimiters (dlm), in the
#' \code{INPUT}-section are not (yet) supported. 
#' 
#' @param x A single character string containing a SAS DATA step with
#'   a DATALINES, CARDS, or CARDS4 block.
#' @param validate_names Logical. If \code{TRUE} (default \code{FALSE}), emits
#'   a warning when the dataset name violates SAS naming rules.
#'
#' @return A data.frame with column names taken from the INPUT statement.
#'   The attribute \code{sas_dataset_name} carries the DATA step name.
#'   For \code{DATA _NULL_} the data is still parsed and returned; the caller
#'   decides what to do with it (matching SAS semantics).
#'   SAS missing-value markers (\code{.}) are converted to \code{NA}.
#'
#' @details
#' Only free-format (list) input is supported. The following SAS features
#' are intentionally rejected with an informative error:
#' \itemize{
#'   \item Column pointers (\code{@}, \code{@@})
#'   \item Column input (e.g. \code{var 1-10})
#'   \item Formatted input (\code{:})
#' }
#' Character values must not contain spaces or quotes; scan-based parsing
#' splits on whitespace and does not handle quoted strings.
#'
#' @examples
#' sas_code <- "
#'   data mydata;
#'     input name $ age score;
#'   datalines;
#'   Alice 30 95.5
#'   Bob   25 88.0
#'   ;
#' "
#' df <- parseSASDatalines(sas_code)
#' 

#' @family data.manipulation
#' @concept data-manipulation
#' @concept string-manipulation
#'
#'

#' @export
parseSASDatalines <- function(x, validate_names = FALSE) {
  
 # see: http://www.psychstatistics.com/2012/12/07/using-datalines-in-sas/
  # or:  http://www.ats.ucla.edu/stat/sas/library/SASRead_os.htm
 
  if (!is.character(x) || length(x) != 1) {
    stop("x must be a single character string")
  }
  
  # Normalize line endings
  x <- gsub("\r\n?", "\n", x)
  
  # --- 1. DATA statement ---
  data_match <- regexec("(?i)\\bdata\\s+([^;\\s]+)\\s*;", x, perl = TRUE)
  data_res   <- regmatches(x, data_match)[[1]]
  
  if (length(data_res) < 2) {
    stop("No valid DATA statement found")
  }
  
  dsname       <- data_res[2]
  is_null_data <- toupper(dsname) == "_NULL_"
  
  if (validate_names && !is_null_data) {
    if (nchar(dsname) > 32 || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", dsname)) {
      warning(sprintf(
        "'%s' is not a valid SAS dataset name (max 32 chars, pattern [A-Za-z_][A-Za-z0-9_]*)",
        dsname
      ))
    }
  }
  
  # --- 2. INPUT statement ---
  input_match <- regexec("(?is)\\binput\\b\\s+([^;]+);", x, perl = TRUE)
  input_res   <- regmatches(x, input_match)[[1]]
  
  if (length(input_res) < 2) {
    stop("No valid INPUT statement found")
  }
  
  input_line <- trimws(gsub("\\s+", " ", input_res[2]))
  
  # Reject unsupported features
  if (grepl("@", input_line)) {
    stop("Column pointers (@, @@) are not supported")
  }
  if (grepl("\\d+\\s*-\\s*\\d+", input_line)) {
    stop("Column input (e.g. var 1-10) is not supported")
  }
  if (grepl(":", input_line)) {
    stop("Formatted input (:) is not supported")
  }
  
  # Parse variable names and types
  tokens <- strsplit(input_line, "\\s+")[[1]]
  var_names <- character()
  is_char   <- logical()
  
  i <- 1
  while (i <= length(tokens)) {
    
    tok <- tokens[i]
    
    if (tok == "$") {
      stop("Invalid INPUT syntax: '$' cannot appear without preceding variable name")
    }
    
    # check if next token is "$"
    if (i < length(tokens) && tokens[i + 1] == "$") {
      var_names <- c(var_names, tok)
      is_char   <- c(is_char, TRUE)
      i <- i + 2
    } else {
      var_names <- c(var_names, tok)
      is_char   <- c(is_char, FALSE)
      i <- i + 1
    }
  }
  
    
  if (any(var_names == "")) {
    stop("Invalid variable names in INPUT statement")
  }
  
  # --- 3. DATALINES / CARDS / CARDS4 block ---
  # Locate the keyword + its opening semicolon, extract body_start from
  # the *same* match to keep keyword and terminator logic consistent.
  kw_rx    <- "(?is)\\b(datalines|cards4|cards)\\s*;"
  kw_match <- regexpr(kw_rx, x, perl = TRUE)
  
  if (kw_match == -1) {
    stop("No valid DATALINES block found")
  }
  
  # Extract keyword name from the same match span (strip trailing whitespace + ";")
  kw_name <- tolower(trimws(
    sub("\\s*;\\s*$", "", regmatches(x, kw_match))
  ))
  
  body_start <- kw_match[1] + attr(kw_match, "match.length")
  body_text  <- substring(x, body_start)
  lines_all  <- strsplit(body_text, "\n")[[1]]
  
  if (kw_name == "cards4") {
    # CARDS4 is terminated by a line that is exactly ";;;;"
    end_idx <- which(trimws(lines_all) == ";;;;")[1]
    if (is.na(end_idx)) {
      stop("CARDS4 block has no ';;;;' terminator")
    }
  } else {
    # DATALINES / CARDS: terminated by a line that is exactly ";"
    # (semicolons *within* a data line are valid in real SAS; matching only a
    #  bare ";" on its own line is the correct terminator heuristic)
    end_idx <- which(trimws(lines_all) == ";")[1]
    if (is.na(end_idx)) {
      stop("DATALINES block has no ';' terminator")
    }
  }
  
  # Rows before the terminator line
  data_lines <- trimws(lines_all[seq_len(end_idx - 1)])
  data_lines <- data_lines[nzchar(data_lines)]
  
  if (length(data_lines) == 0) {
    stop("No data found in DATALINES block")
  }
  
  # --- 4. Validate token count per row ---
  expected_cols <- length(var_names)
  token_counts  <- lengths(strsplit(data_lines, "\\s+"))
  
  bad_rows <- which(token_counts != expected_cols)
  if (length(bad_rows) > 0) {
    stop(sprintf(
      "Row(s) %s have %s token(s) but %d variable(s) declared in INPUT",
      paste(bad_rows, collapse = ", "),
      paste(token_counts[bad_rows], collapse = ", "),
      expected_cols
    ))
  }
  
  # --- 5. Parse data ---
  con <- textConnection(data_lines)
  on.exit(close(con), add = TRUE)
  
  what_list <- lapply(is_char, function(flag) if (flag) "" else NA_real_)
  
  # scan() returns a plain vector (not a list) when what is length-1;
  # na.strings = "." converts SAS missing-value markers to NA for all types
  raw <- scan(con, what = what_list, quiet = TRUE, na.strings = ".")
  if (!is.list(raw)) raw <- list(raw)
  
  df <- data.frame(raw, stringsAsFactors = FALSE)
  names(df) <- var_names
  
  # --- 6. Return (no side effects) ---
  # DATA _NULL_: data is fully parsed and returned; caller decides what to do,
  # mirroring SAS semantics (execute but do not save).
  attr(df, "sas_dataset_name") <- dsname
  
  df
}

