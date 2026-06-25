
test_that("setLength extends with default fill", {
  expect_equal(setLength(LETTERS[1:3], 5), c("A", "B", "C", NA, NA))
})

test_that("setLength truncates", {
  expect_equal(setLength(LETTERS[1:5], 3), c("A", "B", "C"))
})

test_that("setLength exact length is identity", {
  expect_equal(setLength(1:4, 4), 1:4)
})

test_that("setLength extends numeric with custom fill", {
  expect_equal(setLength(1:4, 6, fill = 0), c(1, 2, 3, 4, 0, 0))
})

test_that("setLength to length 0 returns empty", {
  expect_equal(setLength(1:3, 0), integer(0))
})

test_that("setLength preserves type", {
  expect_type(setLength(1L:3L, 5, fill = 0L), "integer")
  expect_type(setLength(c(TRUE, FALSE), 4, fill = FALSE), "logical")
})


