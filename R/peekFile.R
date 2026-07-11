
#' Preview a Delimited Text File
#'
#' Read the first \code{n} data rows of a delimited text file and return
#' the result as a base R \code{data.frame}.
#'
#' This function is intended for quickly inspecting large text files,
#' including compressed files supported by \code{readr::read_delim()}.
#'
#' Column types are guessed from the previewed rows only (the default
#' \code{guess_max} equals \code{n}). If early rows are not representative,
#' supply a larger \code{guess_max} via the dots.
#'
#' @param file character string specifying the file name
#' @param n integer specifying the number of data rows to read,
#'   defaults to 10
#' @param \dots additional arguments passed to
#'   \code{readr::read_delim()}, e.g. \code{delim} or \code{skip}.
#'   The arguments \code{n_max} and \code{show_col_types} are managed
#'   internally and will be ignored if supplied; \code{guess_max}
#'   defaults to \code{n} but may be overridden.
#' @param output character, either \code{"data.frame"} (default) or
#'   \code{"tibble"}, determining the class of the returned object.
#'   Conversion to \code{data.frame} is done by \code{\link{toBaseR}}.
#'   The argument can be abbreviated. Note that it must be given as a
#'   named argument, as it follows the dots.
#'
#' @return a \code{data.frame} or a tibble (according to \code{output})
#'   containing the first \code{n} data rows of the file.
#'
#' @seealso \code{\link[readr]{read_delim}}, \code{\link{toBaseR}}
#'
#' @examples
#' \dontrun{
#' peekFile("data.csv")
#' peekFile("data.csv.gz", delim = "|", n = 20)
#'
#' # unrepresentative early rows: guess types over more lines
#' peekFile("data.csv", n = 10, guess_max = 1000)
#' }
#'
#' @family file.io
#' @concept file-io
#' @concept data-inspection
#' @export
peekFile <- function(file, n = 10, ...,
                     output = c("data.frame", "tibble")) {

  output <- match.arg(output)

  if (!requireNamespace("readr", quietly = TRUE))
    stop("package 'readr' is required for peekFile(), please install it",
         call. = FALSE)

  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n < 1 || n %% 1 != 0)
    stop("'n' must be a single positive integer", call. = FALSE)

  dots <- list(...)

  reserved <- c("n_max", "show_col_types")
  ignored <- intersect(names(dots), reserved)

  if (length(ignored) > 0) {
    warning(
      gettextf(
        "Ignoring argument(s): %s.",
        paste(shQuote(ignored), collapse = ", ")
      ),
      call. = FALSE
    )
    dots[ignored] <- NULL
  }

  # guess_max is a default only, the user may override it
  if (is.null(dots$guess_max))
    dots$guess_max <- n

  dots <- c(
    list(
      file = file,
      n_max = n,
      show_col_types = FALSE
    ),
    dots
  )

  res <- do.call(readr::read_delim, dots)

  if (output == "data.frame") toBaseR(res) else res
}
