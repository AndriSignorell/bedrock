
#' List All Arguments of a Function
#'
#' Returns the formal arguments of a function together with their default
#' values.
#'
#' @param fun function object or function name
#' @param package optional package name used to resolve \code{fun}
#' @param sorted logical; should arguments be sorted alphabetically?
#'   \code{...} is always kept last. Ignored when \code{output = "list"}.
#' @param output character string specifying the output format:
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
#' @family pkg.funinfo
#' @concept introspection
#' @concept programming
#' @export
funArgs <- function(fun,
                    package = NULL,
                    sorted = FALSE,
                    output = c("data.frame", "list", "string")) {

  output <- match.arg(output)

  if (is.character(fun)) {

    if (!is.null(package)) {

      fun <- getExportedValue(package, fun)

    } else {

      # resolve in the caller's environment: get() from within the
      # package namespace would not see functions defined in the
      # global environment
      fun <- get(fun, mode = "function", envir = parent.frame())

    }
  }

  fmls <- formals(fun)

  # primitives have no formals; args() provides a usable stub for most
  if (is.null(fmls) && is.function(fun) && !is.null(args(fun)))
    fmls <- formals(args(fun))

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

  if (sorted) {

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
