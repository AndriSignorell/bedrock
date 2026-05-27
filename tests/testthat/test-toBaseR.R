
# =========================================================
# test-toBaseR.R
# =========================================================

library(testthat)

test_that("toBaseR.default warns", {
  
  expect_warning(
    toBaseR(1:5),
    "Not implemented"
  )
  
})

test_that("toBaseR converts data.frame unchanged", {
  
  x <- data.frame(a = 1:3)
  
  expect_warning(
    res <- toBaseR(x)
  )
  
  expect_equal(res, NULL)
  
})


