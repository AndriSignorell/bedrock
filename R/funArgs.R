
#' List all arguments of a function
#'
#' Returns the formal arguments of a function together with their default
#' values.
#'
#' @param fun Function object or function name.
#' @param package Optional package name used to resolve \code{fun}.
#' @param sort Logical; should arguments be sorted alphabetically?
#'   Ignored when \code{output = "list"}.
#' @param output Character string specifying the output format:
#'   \itemize{
#'     \item \code{"data.frame"} (default): return a data frame.
#'     \item \code{"list"}: return a named list of formal arguments.
#'     \item \code{"string"}: return a comma-separated character string of
#'       argument assignments.
#'   }
#'
#' @return
#' Depending on \code{output}:
#' \itemize{
#'   \item \code{"data.frame"}: a data frame with columns
#'     \code{name} and \code{value}.
#'   \item \code{"list"}: a named list of formal arguments.
#'   \item \code{"string"}: a character vector of length one.
#' }
#'
#' @seealso \code{\link{formals}}, \code{\link{args}}
#'
#' @examples
#' funArgs("combN")
#'
#' funArgs("combN", output = "list")
#'
#' funArgs("combN", output = "string")
#'
#' cat(funArgs("combN", output = "string"))
#'
#' @family pkg.introspection
#' @concept package-utilities
#' @concept data-inspection
#'
#' @export
funArgs <- function(
    fun,
    package = NULL,
    sort = FALSE,
    output = c("data.frame", "list", "string")
) {
  
  output <- match.arg(output)
  
  if (is.character(fun)) {
    
    if (!is.null(package)) {
      
      fun <- getExportedValue(package, fun)
      
    } else {
      
      fun <- get(fun, mode = "function")
      
    }
  }
  
  fmls <- suppressWarnings(formals(fun))
  
  if (is.null(fmls)) {
    
    return(
      switch(
        output,
        "list" = list(),
        "string" = "",
        "data.frame" = structure(
          data.frame(
            name = character(),
            value = character()
          ),
          class = c("FunArgs", "data.frame")
        )
      )
    )
    
  }
  
  if (output == "list")
    return(fmls)
  
  out <- data.frame(
    name = names(fmls),
    value = vapply(
      fmls,
      function(x)
        paste(deparse(x), collapse = " "),
      character(1)
    )
  )
  
  if (sort) {
    
    dots <- out$name == "..."
    
    out <- rbind(
      out[!dots, ][order(out$name[!dots]), ],
      out[dots, ]
    )
    
  }
  
  string <- paste(
    ifelse(
      out$value == "",
      out$name,
      paste(out$name, out$value, sep = " = ")
    ),
    collapse = ", "
  )
  
  if (output == "string")
    return(string)
  
  attr(out, "string") <- string
  
  class(out) <- c("FunArgs", "data.frame")
  
  out
  
}

