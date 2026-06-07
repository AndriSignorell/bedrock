
# ── recodeX ───────────────────────────────────────────────────────────────────
test_that("recodeX recodes factor levels", {
  x   <- factor(c("a","b","c","d"))
  res <- recodeX(x, AB = c("a","b"), elseLevel = "other")
  expect_true("AB" %in% levels(res))
  expect_true("other" %in% levels(res))
  expect_false("a" %in% levels(res))
})

test_that("recodeX keeps specified levels", {
  x   <- factor(c("a","b","c"))
  res <- recodeX(x, keep = "a", BC = c("b","c"))
  expect_true("a" %in% levels(res))
  expect_true("BC" %in% levels(res))
})

test_that("recodeX elseLevel=NA sets unmatched to NA", {
  x   <- factor(letters[1:4])
  res <- recodeX(x, AB = c("a","b"), elseLevel = NA)
  expect_true(any(is.na(res)))
})

test_that("recodeX elseLevel=NULL leaves unmatched unchanged", {
  x   <- factor(letters[1:4])
  res <- recodeX(x, AB = c("a","b"), elseLevel = NULL)
  expect_true(all(c("AB","c","d") %in% levels(res)))
})

test_that("recodeX useEmpty=TRUE keeps empty levels", {
  x   <- factor(letters[1:4])
  res <- recodeX(x, AB = c("a","b"), GH = c("g","h"),
                 elseLevel = NA, useEmpty = TRUE)
  expect_true("GH" %in% levels(res))
})

test_that("recodeX useEmpty=FALSE drops empty levels", {
  x   <- factor(letters[1:4])
  res <- recodeX(x, AB = c("a","b"), GH = c("g","h"),
                 elseLevel = NA, useEmpty = FALSE)
  expect_false("GH" %in% levels(res))
})

test_that("recodeX supports numeric output", {
  x   <- factor(c("1","2","1"))
  res <- recodeX(x, "10" = 1, "20" = 2, num = TRUE)
  expect_equal(res, c(10, 20, 10))
})

test_that("recodeX works on character input and returns character", {
  x   <- c("a","b","c")
  res <- recodeX(x, AB = c("a","b"), elseLevel = "other")
  expect_true(is.character(res))
})

test_that("recodeX sets reference level", {
  x   <- factor(letters[1:4])
  res <- recodeX(x, AB = c("a","b"), elseLevel = NULL, ref = "c")
  expect_equal(levels(res)[1], "c")
})

test_that("recodeX errors on duplicated mappings", {
  x <- factor(c("a","b"))
  expect_error(recodeX(x, X = c("a","b"), Y = "b"))
})


