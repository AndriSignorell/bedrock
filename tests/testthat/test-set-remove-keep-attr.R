
# ===============================================================
# setAttr / removeAttr / keepAttr TESTS
# ===============================================================
# merged from test-set-and-remove-attributes.R; runif() replaced by fixed
# values, the unseeded draws were irrelevant but made the tests
# non-reproducible

x0 <- c(1.5, 2.5, 3.5)

# -- setAttr ----------------------------------------------------

test_that("setAttr sets single and multiple scalar attributes", {
  x <- setAttr(x0, "a", 1)
  expect_identical(attr(x, "a"), 1)

  x <- setAttr(1:3, c("a", "b"), c("A", "B"))
  expect_equal(attr(x, "a"), "A")
  expect_equal(attr(x, "b"), "B")
})

test_that("a single attribute takes a vector value", {
  x <- setAttr(1:10, "dim", c(2, 5))
  expect_equal(dim(x), c(2L, 5L))
})

test_that("list values allow non-scalar and mixed types", {
  x <- setAttr(1:10, c("dim", "myattr"), list(c(2, 5), "abc"))
  expect_equal(dim(x), c(2L, 5L))
  expect_equal(attr(x, "myattr"), "abc")
})

test_that("setAttr overwrites an existing attribute", {
  x <- setAttr(setAttr(x0, "a", 1), "a", 99)
  expect_identical(attr(x, "a"), 99)
})

test_that("setAttr validates its arguments", {
  expect_error(setAttr(1:3, c("a", "b"), list(1)), "same length")
  expect_error(setAttr(x0, c("a", "b"), 1))
  expect_error(setAttr(1:3, 1, "x"), "character")
})

# -- removeAttr -------------------------------------------------

test_that("removeAttr removes single and multiple attributes", {
  x <- setAttr(1:3, c("a", "b"), c("A", "B"))

  x1 <- removeAttr(x, "a")
  expect_null(attr(x1, "a"))
  expect_equal(attr(x1, "b"), "B")

  x2 <- removeAttr(x, c("a", "b"))
  expect_null(attr(x2, "a"))
  expect_null(attr(x2, "b"))
})

test_that("removeAttr without attrNames removes all attributes", {
  x <- setAttr(1:3, c("a", "b"), c("A", "B"))
  expect_null(attributes(removeAttr(x)))
})

test_that("removeAttr does not affect values", {
  expect_identical(removeAttr(setAttr(x0, "a", 1)), x0)
})

test_that("removeAttr silently ignores a non-existing attribute", {
  expect_no_error(res <- removeAttr(x0, "does_not_exist"))
  expect_identical(res, x0)
})

# -- keepAttr ---------------------------------------------------

test_that("keepAttr keeps only the listed attributes", {
  x <- setAttr(x0, c("a", "b", "c"), c(1, 2, 3))
  x <- keepAttr(x, "b")
  expect_equal(names(attributes(x)), "b")
  expect_identical(attr(x, "b"), 2)
})

test_that("keepAttr retains the class attribute", {
  r.lm <- lm(Fertility ~ ., swiss)
  tt   <- keepAttr(r.lm$terms, "class")
  expect_equal(class(tt), c("terms", "formula"))
  expect_null(attr(tt, "variables"))
})

test_that("keepAttr with empty attrNames removes all attributes", {
  x <- setAttr(x0, c("a", "b"), c(1, 2))
  expect_null(attributes(keepAttr(x, character(0))))
})
