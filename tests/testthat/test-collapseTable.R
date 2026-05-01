
# Tests for collapseTable()

library(testthat)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# 2x3 base table (age x edu), values 1..6
make_tab_2x3 <- function() {
  as.table(matrix(
    1:6, nrow = 2,
    dimnames = list(age = c("young", "old"),
                    edu = c("low", "mid", "high"))
  ))
}

# 3x3 base table (age x edu), values 1..9
make_tab_3x3 <- function() {
  as.table(matrix(
    1:9, nrow = 3,
    dimnames = list(age  = c("0-17", "18-64", "65+"),
                    edu  = c("low", "mid", "high"))
  ))
}

# 2x2x2 base table
make_tab_2x2x2 <- function() {
  as.table(array(
    1:8, dim = c(2, 2, 2),
    dimnames = list(sex    = c("m", "f"),
                    edu    = c("low", "high"),
                    region = c("north", "south"))
  ))
}

# ---------------------------------------------------------------------------
# 1. Input validation
# ---------------------------------------------------------------------------

test_that("non-table input is rejected", {
  expect_error(collapseTable(data.frame(x = 1)),
               "must be a table or ftable")
  expect_error(collapseTable(matrix(1:4, 2)),
               "must be a table or ftable")
  expect_error(collapseTable(1L),
               "must be a table or ftable")
})

test_that("ftable input is accepted and converted", {
  ft <- ftable(Titanic, row.vars = "Class")
  expect_silent(collapseTable(ft, Age = c("child+adult", "child+adult")))
})

test_that("table without named dimensions is rejected", {
  tab <- as.table(matrix(1:4, 2))   # no dimnames names
  expect_error(collapseTable(tab, c("a", "a")), "named dimensions")
})

test_that("too many arguments are rejected", {
  tab <- make_tab_2x3()
  expect_error(
    collapseTable(tab, c("a", "b"), c("x", "x", "x"), c("p", "p")),
    "only 2 dimension"
  )
})

test_that("no-op (zero extra args) returns input unchanged", {
  tab <- make_tab_2x3()
  expect_identical(collapseTable(tab), tab)
})

# ---------------------------------------------------------------------------
# 2. Mapping validation
# ---------------------------------------------------------------------------

test_that("wrong-length mapping is rejected", {
  tab <- make_tab_3x3()
  expect_error(
    collapseTable(tab, age = c("a", "b")),   # needs length 3
    "length 2 but dimension has 3"
  )
})

test_that("NA in mapping is rejected", {
  tab <- make_tab_3x3()
  expect_error(
    collapseTable(tab, age = c("minor", NA, "adult")),
    "NA values"
  )
})

test_that("unknown dimension name errors in strict mode", {
  tab <- make_tab_3x3()
  expect_error(collapseTable(tab, gender = c("a", "b", "c")),
               "not found in table")
})

test_that("unknown dimension name warns in non-strict mode", {
  tab <- make_tab_3x3()
  expect_warning(
    collapseTable(tab, gender = c("a", "b", "c"), strict = FALSE),
    "not found in table"
  )
})

# ---------------------------------------------------------------------------
# 3. No-collapse pass-through
# ---------------------------------------------------------------------------

test_that("identity mapping returns table with same sums", {
  tab <- make_tab_3x3()
  res <- collapseTable(tab, age = c("0-17", "18-64", "65+"))
  expect_equal(sum(res), sum(tab))
  expect_equal(dim(res), dim(tab))
})

# ---------------------------------------------------------------------------
# 4. Basic collapsing — frequencies
# ---------------------------------------------------------------------------

test_that("collapsing two levels sums their frequencies", {
  tab <- make_tab_3x3()
  # age: collapse "18-64" and "65+" into "adult"
  res <- collapseTable(tab, age = c("minor", "adult", "adult"))
  
  expect_equal(dimnames(res)$age, c("minor", "adult"))
  # "minor" row = original row 1 (values 1,4,7)
  expect_equal(as.vector(res["minor", ]), c(1, 4, 7))
  # "adult" row = rows 2+3 summed (2+3, 5+6, 8+9)
  expect_equal(as.vector(res["adult", ]), c(5, 11, 17))
})

test_that("collapsing all levels to one returns row/column sums", {
  tab <- make_tab_2x3()
  res <- collapseTable(tab, edu = c("any", "any", "any"))
  expect_equal(dimnames(res)$edu, "any")
  # each age row should equal its marginal sum
  expect_equal(as.vector(res["young", "any"]), sum(tab["young", ]))
  expect_equal(as.vector(res["old",   "any"]), sum(tab["old",   ]))
})

test_that("total frequency is preserved after collapse", {
  tab <- make_tab_3x3()
  res <- collapseTable(tab,
                       age = c("minor", "adult", "adult"),
                       edu = c("low", "high", "high"))
  expect_equal(sum(res), sum(tab))
})

# ---------------------------------------------------------------------------
# 5. Level ordering
# ---------------------------------------------------------------------------

test_that("output levels follow first-occurrence in mapping vector", {
  tab <- make_tab_3x3()
  # mapping puts "senior" first even though "65+" is the last original level
  res <- collapseTable(tab, age = c("adult", "adult", "senior"))
  expect_equal(dimnames(res)$age, c("adult", "senior"))
})

test_that("untouched dimension retains original level order", {
  tab <- make_tab_3x3()
  res <- collapseTable(tab, age = c("minor", "adult", "adult"))
  expect_equal(dimnames(res)$edu, c("low", "mid", "high"))
})

# ---------------------------------------------------------------------------
# 6. Argument matching
# ---------------------------------------------------------------------------

test_that("named arguments are matched by name regardless of order", {
  tab <- make_tab_3x3()
  res_fwd <- collapseTable(tab,
                           age = c("minor", "adult", "adult"),
                           edu = c("low", "high", "high"))
  res_rev <- collapseTable(tab,
                           edu = c("low", "high", "high"),
                           age = c("minor", "adult", "adult"))
  expect_equal(res_fwd, res_rev)
})

test_that("unnamed arguments warn in strict mode and are assigned positionally", {
  tab <- make_tab_3x3()
  expect_warning(
    res <- collapseTable(tab, c("minor", "adult", "adult")),
    "unnamed"
  )
  expect_equal(dimnames(res)$age, c("minor", "adult"))
})

test_that("unnamed arguments are silent in non-strict mode", {
  tab <- make_tab_3x3()
  expect_no_warning(
    collapseTable(tab, c("minor", "adult", "adult"), strict = FALSE)
  )
})

test_that("mixed named/unnamed arguments assign correctly", {
  tab <- make_tab_3x3()
  # edu named, age unnamed (goes to the remaining dim = age)
  expect_warning(
    res <- collapseTable(tab,
                         edu = c("low", "high", "high"),
                         c("minor", "adult", "adult")),
    "by position"
  )
  expect_equal(dimnames(res)$age, c("minor", "adult"))
  expect_equal(dimnames(res)$edu, c("low", "high"))
})

# ---------------------------------------------------------------------------
# 7. Return type and structure
# ---------------------------------------------------------------------------

test_that("result is a plain table, not xtabs", {
  tab <- make_tab_3x3()
  res <- collapseTable(tab, age = c("minor", "adult", "adult"))
  expect_s3_class(res, "table")
  expect_false("xtabs" %in% class(res))
  expect_null(attr(res, "call"))
})

test_that("result has correct number of dimensions", {
  tab <- make_tab_3x3()
  res <- collapseTable(tab, age = c("minor", "adult", "adult"))
  expect_equal(length(dim(res)), 2L)
  expect_equal(unname(dim(res)), c(2L, 3L))
})

test_that("3-dimensional table collapses correctly", {
  tab <- make_tab_2x2x2()
  res <- collapseTable(tab, region = c("any", "any"))
  expect_equal(dimnames(res)$region, "any")
  expect_equal(sum(res), sum(tab))
  expect_equal(length(dim(res)), 3L)
})

# ---------------------------------------------------------------------------
# 8. Edge cases
# ---------------------------------------------------------------------------

test_that("1x1 table after full collapse returns scalar table", {
  tab <- make_tab_2x3()
  res <- collapseTable(tab,
                       age = c("all", "all"),
                       edu = c("all", "all", "all"))
  expect_equal(as.vector(res), sum(tab))
})

test_that("variable names containing special characters are handled", {
  tab <- as.table(matrix(
    1:4, 2,
    dimnames = list("age group" = c("young", "old"),
                    "edu.level" = c("low", "high"))
  ))
  res <- collapseTable(tab, `age group` = c("any", "any"))
  expect_equal(sum(res), sum(tab))
})

test_that("zero-count cells after collapse are 0, not NA", {
  # young/A=5, young/B=0, old/A=3, old/B=0
  # After collapsing age -> "all": all/A = 5+3 = 8, all/B = 0+0 = 0
  tab <- as.table(matrix(
    c(5, 3, 0, 0), 2,
    dimnames = list(age = c("young", "old"),
                    group = c("A", "B"))
  ))
  res <- collapseTable(tab, age = c("all", "all"))
  expect_true(all(!is.na(res)))
  expect_equal(as.vector(res["all", "B"]), 0)
})

test_that("result dimensions are named", {
  tab <- make_tab_3x3()
  res <- collapseTable(tab, age = c("minor", "adult", "adult"))
  expect_named(dimnames(res))
  expect_equal(names(dimnames(res)), c("age", "edu"))
})