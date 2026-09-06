
test_that("compareDataFrames reports missing and differing rows", {
  x <- data.frame(
    id = c("A", "B", "C"),
    value = c(1L, 2L, 3L),
    text = c("a", "b", "c")
  )
  y <- data.frame(
    id = c("A", "B", "D"),
    value = c(1L, 9L, 4L),
    text = c("a", "b", "d")
  )
  
  out <- compareDataFrames(x, y, key = "id")
  
  expect_false(out$identical)
  expect_identical(out$onlyInX$id, "C")
  expect_identical(out$onlyInY$id, "D")
  expect_identical(out$diffs$id, "B")
  expect_identical(out$diffs$diffCols[[1L]], "value")
})

test_that("compareDataFrames matches rows by key rather than position", {
  x <- data.frame(id = c("A", "B", "C"), value = 1:3)
  y <- x[c(3, 1, 2), ]
  
  out <- compareDataFrames(x, y, key = "id")
  
  expect_true(out$identical)
  expect_equal(nrow(out$onlyInX), 0L)
  expect_equal(nrow(out$onlyInY), 0L)
  expect_equal(nrow(out$diffs), 0L)
})

test_that("compareDataFrames compares only common columns", {
  x <- data.frame(id = 1:2, value = c("a", "b"), onlyX = 3:4)
  y <- data.frame(id = 1:2, value = c("a", "b"), onlyY = 5:6)
  
  out <- compareDataFrames(x, y, key = "id")
  
  expect_true(out$identical)
  expect_named(out$onlyInX, c("id", "value"))
  expect_named(out$onlyInY, c("id", "value"))
})

test_that("compareDataFrames detects type differences", {
  x <- data.frame(id = 1L, value = 1L)
  y <- data.frame(id = 1L, value = 1)
  
  out <- compareDataFrames(x, y, key = "id")
  
  expect_false(out$identical)
  expect_identical(out$diffs$diffCols[[1L]], "value")
})

test_that("compareDataFrames preserves the key type in differences", {
  x <- data.frame(id = as.Date("2025-01-01") + 0:1, value = 1:2)
  y <- data.frame(id = as.Date("2025-01-01") + 0:1, value = c(1L, 9L))
  
  out <- compareDataFrames(x, y, key = "id")
  
  expect_s3_class(out$diffs$id, "Date")
  expect_identical(out$diffs$id, as.Date("2025-01-02"))
})

test_that("compareDataFrames validates the key", {
  x <- data.frame(id = c(1, 1), value = 1:2)
  y <- data.frame(id = 1:2, value = 1:2)
  
  expect_error(compareDataFrames(x, y, key = "missing"),
               "not found in 'x'")
  expect_error(compareDataFrames(y, x, key = "missing"),
               "not found in 'x'")
  expect_error(compareDataFrames(x, y, key = "id"),
               "duplicated values in 'x'")
  expect_error(compareDataFrames(y, x, key = "id"),
               "duplicated values in 'y'")
})
