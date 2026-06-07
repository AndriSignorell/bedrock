
# ── setAttr ───────────────────────────────────────────────────────────────────
test_that("setAttr sets a single attribute", {
  x <- setAttr(runif(5), attrNames = "a", attrValues = 1)
  expect_identical(attr(x, "a"), 1)
})

test_that("setAttr sets multiple attributes", {
  x <- setAttr(runif(5), attrNames = c("a", "b"), attrValues = c(1, 2))
  expect_identical(attr(x, "a"), 1)
  expect_identical(attr(x, "b"), 2)
})

test_that("setAttr overwrites existing attribute", {
  x <- setAttr(runif(5), attrNames = "a", attrValues = 1)
  x <- setAttr(x,        attrNames = "a", attrValues = 99)
  expect_identical(attr(x, "a"), 99)
})

test_that("setAttr stops on length mismatch", {
  expect_error(setAttr(runif(5), c("a", "b"), 1))
})

test_that("setAttr stops on non-character attrNames", {
  expect_error(setAttr(runif(5), 1, "val"))
})


# ── removeAttr ────────────────────────────────────────────────────────────────
test_that("removeAttr removes a single attribute", {
  x <- setAttr(runif(5), c("a", "b"), c(1, 2))
  x <- removeAttr(x, "a")
  expect_null(attr(x, "a"))
  expect_identical(attr(x, "b"), 2)
})

test_that("removeAttr removes multiple attributes", {
  x <- setAttr(runif(5), c("a", "b"), c(1, 2))
  x <- removeAttr(x, c("a", "b"))
  expect_null(attr(x, "a"))
  expect_null(attr(x, "b"))
})

test_that("removeAttr removes all attributes when called without attrNames", {
  x <- setAttr(runif(5), c("a", "b"), c(1, 2))
  x <- removeAttr(x)
  expect_null(attributes(x))
})

test_that("removeAttr does not affect values", {
  x0 <- runif(5)
  x  <- setAttr(x0, "a", 1)
  expect_identical(unname(removeAttr(x)), unname(x0))
})

test_that("removeAttr silently ignores non-existing attribute", {
  x <- runif(5)
  expect_no_error(removeAttr(x, "does_not_exist"))
})


# ── keepAttr ──────────────────────────────────────────────────────────────────
test_that("keepAttr retains only specified attributes", {
  x <- setAttr(runif(5), c("a", "b", "c"), c(1, 2, 3))
  x <- keepAttr(x, "b")
  expect_null(attr(x, "a"))
  expect_identical(attr(x, "b"), 2)
  expect_null(attr(x, "c"))
})

test_that("keepAttr retains class attribute", {
  r.lm <- lm(Fertility ~ ., swiss)
  t     <- keepAttr(r.lm$terms, "class")
  expect_equal(class(t), c("terms", "formula"))
  expect_null(attr(t, "variables"))
})

test_that("keepAttr with empty attrNames removes all attributes", {
  x <- setAttr(runif(5), c("a", "b"), c(1, 2))
  x <- keepAttr(x, character(0))
  expect_null(attributes(x))
})
