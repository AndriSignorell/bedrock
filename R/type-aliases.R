
# Type coercion aliases
# Shorter, more intuitive alternatives to base R coercion functions

#' Coerce to numeric
#' @param x a vector
#' @param ... further arguments passed to \code{as.numeric}
#' @rdname type-aliases
#' @export
num <- function(x, ...) as.numeric(x, ...)

#' Coerce to integer
#' @param x a vector
#' @param ... further arguments passed to \code{as.integer}
#' @rdname type-aliases
#' @export
int <- function(x, ...) as.integer(x, ...)

#' Coerce to character
#' @param x a vector
#' @param ... further arguments passed to \code{as.character}
#' @rdname type-aliases
#' @export
chr <- function(x, ...) as.character(x, ...)

#' Coerce factor or character to numeric
#'
#' Shortcut for \code{as.numeric(as.character(x))}. Avoids returning
#' internal factor codes when coercing factors directly with \code{as.numeric}.
#'
#' @param x a factor or character vector
#' @rdname type-aliases
#' @export
nchr <- function(x) as.numeric(as.character(x))

