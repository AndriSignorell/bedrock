
## ------------------------------------------------------------
## Tests for setAttr() and removeAttr()
## ------------------------------------------------------------

## --- Setup ---------------------------------------------------
x0 <- runif(5)

## --- Test: set single attribute ------------------------------
x1 <- setAttr(x0, attrNames = "a", attrValues = 1)
stopifnot(identical(attr(x1, "a"), 1))

## --- Test: set multiple attributes ---------------------------
x2 <- setAttr(
  x0,
  attrNames  = c("a", "b"),
  attrValues = c(1, 2)
)
stopifnot(
  identical(attr(x2, "a"), 1),
  identical(attr(x2, "b"), 2)
)

## --- Test: overwrite attribute -------------------------------
x3 <- setAttr(x2, attrNames = "a", attrValues = 99)
stopifnot(identical(attr(x3, "a"), 99))

## --- Test: remove single attribute ---------------------------
x4 <- removeAttr(x2, "a")
stopifnot(
  is.null(attr(x4, "a")),
  identical(attr(x4, "b"), 2)
)

## --- Test: remove multiple attributes ------------------------
x5 <- removeAttr(x2, c("a", "b"))
stopifnot(
  is.null(attr(x5, "a")),
  is.null(attr(x5, "b"))
)

## --- Test: remove all attributes -----------------------------
x6 <- removeAttr(x2)
stopifnot(is.null(attributes(x6)))

## --- Test: NA handling (attributes should not affect values) -
stopifnot(identical(unname(x0), unname(removeAttr(x2))))

## --- Test: vector recycling NOT allowed (should error) -------
err <- try(setAttr(x0, c("a","b"), 1), silent = TRUE)
stopifnot(inherits(err, "try-error"))

## --- Test: length mismatch -----------------------------------
err <- try(setAttr(x0, c("a","b"), c(1)), silent = TRUE)
stopifnot(inherits(err, "try-error"))

## --- Test: remove non-existing attribute ---------------------
x7 <- removeAttr(x0, "does_not_exist")
stopifnot(is.null(attr(x7, "does_not_exist")))

## --- All tests passed ----------------------------------------
cat("All tests passed.\n")