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
