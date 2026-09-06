
# ── resolveGroups ─────────────────────────────────────────────────────────────

test_that("resolveGroups works with vector and grouping", {
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c("A","A","A","B","B","B")
  res <- resolveGroups(x, g)
  expect_equal(res$k, 2L)
  expect_equal(res$n, 6L)
  expect_equal(res$groupNames, c("A","B"))
})

test_that("resolveGroups works with list input", {
  res <- resolveGroups(list(A = c(1,2,3), B = c(4,5,6)))
  expect_equal(res$k, 2L)
  expect_equal(res$groupNames, c("A","B"))
})

test_that("resolveGroups removes NAs", {
  x <- c(1, NA, 3, 4, 5, 6)
  g <- c("A","A","A","B","B","B")
  res <- resolveGroups(x, g)
  expect_equal(res$n, 5L)
})

test_that("resolveGroups stops if groups missing", {
  expect_error(resolveGroups(c(1,2,3)), "'groups' is missing")
})

test_that("resolveGroups stops if only one group", {
  expect_error(resolveGroups(c(1,2,3), c("A","A","A")), "same group")
})

test_that("resolveGroups errors when list has non-numeric", {
  expect_error(resolveGroups(list(A = c("x","y"), B = c("a","b"))),
               "must be numeric")
})

test_that("the returned components are named in camel case", {
  res <- resolveGroups(c(1, 2, 3, 4), c("A","A","B","B"))
  expect_named(res, c("x", "groups", "n", "k",
                      "groupSizes", "groupNames", "dataName"))
})

test_that("groupSizes is a named integer vector in level order", {
  x <- c(1, 2, 3, 4, 5)
  g <- c("B","B","A","A","A")

  res <- resolveGroups(x, g)
  expect_type(res$groupSizes, "integer")
  expect_equal(res$groupSizes, c(A = 3L, B = 2L))

  # the same shape for a list input, there in the order of the list
  lst <- list(B = c(1, 2), A = c(3, 4, 5))
  res <- resolveGroups(lst)
  expect_type(res$groupSizes, "integer")
  expect_equal(res$groupSizes, c(B = 2L, A = 3L))
})

test_that("x and groups match up after the missing values are removed", {
  x <- c(1, NA, 3, 4, 5, NA)
  g <- c("A","A","A","B","B","B")
  res <- resolveGroups(x, g)
  expect_equal(res$x, c(1, 3, 4, 5))
  expect_equal(as.character(res$groups), c("A","A","B","B"))
  expect_equal(res$groupSizes, c(A = 2L, B = 2L))
  expect_equal(res$n, 4L)
})

test_that("NaN is treated like NA", {
  res <- resolveGroups(c(1, NaN, 3, 4), c("A","A","B","B"))
  expect_equal(res$n, 3L)

  res <- resolveGroups(list(A = c(1, NaN, 3), B = c(4, 5)))
  expect_equal(res$n, 4L)
})

test_that("a missing group label drops the observation", {
  res <- resolveGroups(c(1, 2, 3, 4), c("A", NA, "B", "B"))
  expect_equal(res$n, 3L)
  expect_equal(res$groupSizes, c(A = 1L, B = 2L))
})

test_that("empty levels are dropped", {
  g <- factor(c("A","A","B","B"), levels = c("A","B","C"))
  res <- resolveGroups(c(1, 2, 3, 4), g)
  expect_equal(res$k, 2L)
  expect_equal(res$groupNames, c("A","B"))
})

test_that("the level order of a factor is preserved", {
  g <- factor(c("b","b","a","a"), levels = c("b","a"))
  expect_equal(resolveGroups(c(1, 2, 3, 4), g)$groupNames, c("b","a"))

  g <- factor(c("lo","lo","hi","hi"), levels = c("lo","hi"), ordered = TRUE)
  res <- resolveGroups(c(1, 2, 3, 4), g)
  expect_equal(res$groupNames, c("lo","hi"))
  expect_true(is.ordered(res$groups))
})

test_that("NAs are removed within the groups of a list", {
  res <- resolveGroups(list(A = c(1, NA, 3), B = c(4, 5)))
  expect_equal(res$n, 4L)
  expect_equal(res$groupSizes, c(A = 2L, B = 2L))
  expect_equal(res$x, c(1, 3, 4, 5))
})

test_that("an unnamed list is labelled by position", {
  res <- resolveGroups(list(c(1, 2, 3), c(4, 5)))
  expect_equal(res$groupNames, c("1", "2"))
  expect_equal(res$groupSizes, c(`1` = 3L, `2` = 2L))
})

test_that("list names must be complete and unique", {
  # duplicated names would silently merge two groups into one level
  expect_error(resolveGroups(list(a = c(1, 2), a = c(3, 4))),
               "complete and unique")
  # a partially named list is an error, not silently renumbered
  expect_error(resolveGroups(list(a = c(1, 2), c(3, 4))),
               "complete and unique")
  expect_error(resolveGroups(setNames(list(c(1, 2), c(3, 4)), c("a", NA))),
               "complete and unique")
})

test_that("a data frame is resolved column by column", {
  d <- data.frame(ctrl = c(1, 2, 3), treat = c(4, 5, NA))
  res <- resolveGroups(d)
  expect_equal(res$k, 2L)
  expect_equal(res$groupNames, c("ctrl", "treat"))
  expect_equal(res$groupSizes, c(ctrl = 3L, treat = 2L))
  expect_equal(res$x, c(1, 2, 3, 4, 5))
  expect_equal(res$dataName, "d")

  # a non-numeric column is caught like any other list element
  expect_error(resolveGroups(data.frame(a = 1:3, b = letters[1:3])),
               "must be numeric")
})

test_that("dataName describes the input", {
  x <- c(1, 2, 3, 4)
  g <- c("A","A","B","B")
  expect_equal(resolveGroups(x, g)$dataName, "x and g")

  lst <- list(A = c(1, 2), B = c(3, 4))
  expect_equal(resolveGroups(lst)$dataName, "lst")
})

test_that("groups is ignored with a warning for a list input", {
  expect_warning(resolveGroups(list(A = c(1, 2), B = c(3, 4)), c("x", "y")),
                 "ignoring argument 'groups'")
})

test_that("a list needs at least two groups", {
  expect_error(resolveGroups(list(A = c(1, 2))), "at least two groups")
})

test_that("a group left empty by the missing values is an error", {
  expect_error(resolveGroups(list(A = c(1, 2), B = c(NA_real_, NA_real_))),
               "must contain observations")
})

test_that("x must be a numeric vector", {
  expect_error(resolveGroups(letters[1:6], rep(1:2, each = 3)),
               "must be a numeric vector")
  expect_error(resolveGroups(matrix(1:6, ncol = 1), rep(1:2, each = 3)),
               "must be a numeric vector")
})

test_that("groups must be a vector", {
  expect_error(resolveGroups(c(1, 2, 3, 4), matrix(c("A","A","B","B"), ncol = 2)),
               "must be a vector")
  expect_error(resolveGroups(c(1, 2, 3, 4),
                             data.frame(g = c("A","A","B","B"))),
               "must be a vector")
})

test_that("x and groups must have the same length", {
  expect_error(resolveGroups(c(1, 2, 3), c("A", "B")), "same length")
})

test_that("both interfaces lead to the same result", {
  x <- c(1, 2, 3, 4, 5, 6)
  g <- factor(rep(c("A", "B"), each = 3))

  fromVector <- resolveGroups(x, g)
  fromList <- resolveGroups(split(x, g))

  expect_equal(fromVector[c("x", "n", "k", "groupSizes", "groupNames")],
               fromList[c("x", "n", "k", "groupSizes", "groupNames")])
})
