test_that("checkConfLevel() accepts what the suite documents", {

  expect_silent(checkConfLevel(0.95))
  expect_silent(checkConfLevel(0.5))
  expect_silent(checkConfLevel(1e-8))
  expect_silent(checkConfLevel(1 - 1e-8))

  # NA is the default of nearly every CI function in the suite; a check
  # that leads with !is.numeric() rejects it, which is how relRisk() once
  # came to refuse its own default
  expect_silent(checkConfLevel(NA))
  expect_silent(checkConfLevel(NA_real_))
  expect_silent(checkConfLevel(NA_integer_))
})


test_that("checkConfLevel() returns its argument invisibly", {

  expect_invisible(checkConfLevel(0.9))

  # the callers use it in an assignment
  conf.level <- checkConfLevel(0.9)
  expect_identical(conf.level, 0.9)

  expect_identical(checkConfLevel(NA), NA)
})


test_that("checkConfLevel() refuses the rest, naming the argument", {

  # length first: is.na() on a longer vector used to make the surrounding
  # if() the error, which then talked about the condition
  expect_error(checkConfLevel(c(0.9, 0.95)), "conf.level")
  expect_error(checkConfLevel(numeric(0)), "conf.level")
  expect_error(checkConfLevel(NULL), "conf.level")

  # NaN is na, but it is not "no interval wanted"
  expect_error(checkConfLevel(NaN), "conf.level")

  expect_error(checkConfLevel("0.95"), "conf.level")
  expect_error(checkConfLevel(list(0.95)), "conf.level")
  expect_error(checkConfLevel(factor(0.95)), "conf.level")

  # the range is open at both ends
  expect_error(checkConfLevel(0), "conf.level")
  expect_error(checkConfLevel(1), "conf.level")
  expect_error(checkConfLevel(-0.1), "conf.level")
  expect_error(checkConfLevel(1.2), "conf.level")
  expect_error(checkConfLevel(Inf), "conf.level")
})


test_that("checkConfLevel() reports without a call, so the message reads plainly", {

  # call. = FALSE: the caller is a statistical function, and prefixing the
  # message with 'Error in checkConfLevel(conf.level):' points at the
  # helper rather than at the argument the user got wrong
  err <- tryCatch(checkConfLevel(2), error = function(e) e)
  expect_null(conditionCall(err))
})


test_that("checkFlag() accepts TRUE and FALSE only", {

  expect_silent(checkFlag(TRUE))
  expect_silent(checkFlag(FALSE))

  expect_invisible(checkFlag(TRUE))
  expect_identical(checkFlag(FALSE), FALSE)

  # NA passes is.logical() and has length one, but a switch that is
  # neither on nor off has no meaning - and if(NA) fails somewhere else
  expect_error(checkFlag(NA), "logical")

  expect_error(checkFlag(c(TRUE, FALSE)), "logical")
  expect_error(checkFlag(logical(0)), "logical")
  expect_error(checkFlag(NULL), "logical")
  expect_error(checkFlag(1), "logical")
  expect_error(checkFlag("TRUE"), "logical")
})


test_that("checkFlag() names the argument it was given", {

  correct <- NA
  expect_error(checkFlag(correct), "'correct'")

  unbiased <- "yes"
  expect_error(checkFlag(unbiased), "'unbiased'")

  # and an explicit name wins, for callers that pass something else
  args <- list(scaled = NA)
  expect_error(checkFlag(args$scaled, "scaled"), "'scaled'")

  err <- tryCatch(checkFlag(NA), error = function(e) e)
  expect_null(conditionCall(err))
})


test_that("checkCount() accepts whole numbers, however stored", {

  expect_silent(checkCount(0))
  expect_silent(checkCount(2L))
  expect_silent(checkCount(2))
  expect_silent(checkCount(1e6))

  # the result of arithmetic on integers is a double, and it is still a count
  expect_silent(checkCount(4 / 2))

  expect_invisible(checkCount(2))
  expect_identical(checkCount(2L), 2L)
})


test_that("checkCount() refuses the rest, naming the argument", {

  expect_error(checkCount(c(1, 2)), "must be a single integer")
  expect_error(checkCount(numeric(0)), "must be a single integer")
  expect_error(checkCount(NULL), "must be a single integer")

  expect_error(checkCount(1.5), "must be a single integer")
  expect_error(checkCount(NA), "must be a single integer")
  expect_error(checkCount(NA_integer_), "must be a single integer")
  expect_error(checkCount(NaN), "must be a single integer")
  expect_error(checkCount(Inf), "must be a single integer")
  expect_error(checkCount("2"), "must be a single integer")

  # a flag survives as.integer(), but reaching a count argument it is a
  # mistake rather than a shorthand
  expect_error(checkCount(TRUE), "must be a single integer")

  sep <- -1
  expect_error(checkCount(sep), "'sep'")

  err <- tryCatch(checkCount(-1), error = function(e) e)
  expect_null(conditionCall(err))
})


test_that("checkCount() honours the lower bound", {

  expect_silent(checkCount(0, min = 0L))
  expect_error(checkCount(0, min = 1L), "not smaller than 1")

  width <- 0
  expect_error(checkCount(width, min = 1L), "'width'")

  expect_silent(checkCount(1, min = 1L))
  expect_silent(checkCount(-1, min = -5L))
})


test_that("checkString() accepts a single string", {

  expect_silent(checkString("a label"))
  expect_silent(checkString(""))     # empty is a label too

  expect_invisible(checkString("x"))
  expect_identical(checkString("x"), "x")
})


test_that("checkString() refuses the rest, naming the argument", {

  expect_error(checkString(c("a", "b")), "character string")
  expect_error(checkString(character(0)), "character string")
  expect_error(checkString(NULL), "character string")
  expect_error(checkString(NA), "character string")
  expect_error(checkString(NA_character_), "character string")
  expect_error(checkString(42), "character string")
  expect_error(checkString(factor("a")), "character string")

  dataName <- 42
  expect_error(checkString(dataName), "'dataName'")

  # an explicit name wins, for callers that pass something else
  args <- list(caption = NA)
  expect_error(checkString(args$caption, "caption"), "'caption'")

  err <- tryCatch(checkString(NA), error = function(e) e)
  expect_null(conditionCall(err))
})
