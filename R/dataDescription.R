
#' Extract Data Description from Excel File
#'
#' Reads a documentation sheet from an Excel file and extracts variable
#' descriptions and coding information.
#'
#' @param fn Character string. Path to the Excel file.
#' @param sheet Character string. Name of the documentation sheet.
#'   Default is \code{"Description"}.
#'
#' @return
#' A list with the following components:
#' \itemize{
#'   \item \code{desctable}: A data frame containing the description table
#'   \item \code{codes}: A named list of code definitions per variable
#' }
#'
#' @details
#' The function reads the specified sheet and trims trailing empty rows.
#'
#' If a column named \code{"Codes"} is present, its contents are split by
#' line breaks (\code{\\r\\n}) and returned as a list of codes per variable.
#'
#' The Excel sheet is expected to contain at least:
#' \itemize{
#'   \item Variable names
#'   \item Descriptions
#'   \item Optional coding definitions
#' }
#'
#' If the sheet does not exist or no additional sheets are present,
#' the function returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' desc <- dataDescription("example.xlsx")
#'
#' desc$desctable
#' desc$codes[["gender"]]
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#'



#' @export
dataDescription <- function(fn, sheet = "Description") {
  
  sheets <- readxl::excel_sheets(fn)
  
  if (!(sheet %in% sheets) || length(sheets) <= 1) {
    return(NULL)
  }
  
  d.desc <- as.data.frame(readxl::read_excel(fn, sheet = sheet))
  
  empty_rows <- which(apply(d.desc, 1, function(x) all(is.na(x))))
  if (length(empty_rows) > 0) {
    d.desc <- d.desc[1:(min(empty_rows) - 1), ]
  }
  
  tab <- d.desc[!is.na(d.desc$Codes), ]
  
  codelist <- list()
  
  for (i in seq_len(nrow(tab))) {
    codelist[[tab$Variable[i]]] <- strsplit(tab$Codes[i], "\\r\\n")[[1]]
  }
  
  return(list(desctable = d.desc, codes = codelist))
}


