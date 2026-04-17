

test_that("basic formats work", {
  expect_equal(as.CDateFmt("yyyy-mm-dd"), "%Y-%m-%d")
  expect_equal(as.CDateFmt("dd.mm.yy"), "%d.%m.%y")
  expect_equal(as.CDateFmt("d/m/yyyy"), "%e/%m/%Y")
})

test_that("compact formats work", {
  expect_equal(as.CDateFmt("yyyymmdd"), "%Y%m%d")
  expect_equal(as.CDateFmt("ddmmyyyy"), "%d%m%Y")
})

test_that("text and mixed formats work", {
  expect_equal(as.CDateFmt("mmm d, yyyy"), "%b %e, %Y")
  expect_equal(as.CDateFmt("Today is dddd"), "Today is %A")
})

test_that("all token lengths are handled", {
  expect_equal(as.CDateFmt("d"), "%e")
  expect_equal(as.CDateFmt("dd"), "%d")
  expect_equal(as.CDateFmt("ddd"), "%a")
  expect_equal(as.CDateFmt("dddd"), "%A")
})

test_that("month tokens work", {
  expect_equal(as.CDateFmt("m"), "%m")
  expect_equal(as.CDateFmt("mm"), "%m")
  expect_equal(as.CDateFmt("mmm"), "%b")
  expect_equal(as.CDateFmt("mmmm"), "%B")
})

test_that("year tokens work", {
  expect_equal(as.CDateFmt("y"), "%y")
  expect_equal(as.CDateFmt("yy"), "%y")
  expect_equal(as.CDateFmt("yyyy"), "%Y")
})

test_that("unknown tokens are preserved", {
  expect_equal(as.CDateFmt("yyyy-qq-dd"), "%Y-qq-%d")
  expect_equal(as.CDateFmt("abc"), "abc")
})

test_that("non-format characters are preserved", {
  expect_equal(as.CDateFmt("yyyy/mm/dd"), "%Y/%m/%d")
  expect_equal(as.CDateFmt("yyyy mm dd"), "%Y %m %d")
})