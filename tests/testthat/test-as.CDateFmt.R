

test_that("basic formats work", {
  expect_equal(asCDateFmt("yyyy-mm-dd"), "%Y-%m-%d")
  expect_equal(asCDateFmt("dd.mm.yy"), "%d.%m.%y")
  expect_equal(asCDateFmt("d/m/yyyy"), "%e/%m/%Y")
})

test_that("compact formats work", {
  expect_equal(asCDateFmt("yyyymmdd"), "%Y%m%d")
  expect_equal(asCDateFmt("ddmmyyyy"), "%d%m%Y")
})

test_that("text and mixed formats work", {
  expect_equal(asCDateFmt("mmm d, yyyy"), "%b %e, %Y")
  expect_equal(asCDateFmt("Today is dddd"), "Today is %A")
})

test_that("all token lengths are handled", {
  expect_equal(asCDateFmt("d"), "%e")
  expect_equal(asCDateFmt("dd"), "%d")
  expect_equal(asCDateFmt("ddd"), "%a")
  expect_equal(asCDateFmt("dddd"), "%A")
})

test_that("month tokens work", {
  expect_equal(asCDateFmt("m"), "%m")
  expect_equal(asCDateFmt("mm"), "%m")
  expect_equal(asCDateFmt("mmm"), "%b")
  expect_equal(asCDateFmt("mmmm"), "%B")
})

test_that("year tokens work", {
  expect_equal(asCDateFmt("y"), "%y")
  expect_equal(asCDateFmt("yy"), "%y")
  expect_equal(asCDateFmt("yyyy"), "%Y")
})

test_that("unknown tokens are preserved", {
  expect_equal(asCDateFmt("yyyy-qq-dd"), "%Y-qq-%d")
  expect_equal(asCDateFmt("abc"), "abc")
})

test_that("non-format characters are preserved", {
  expect_equal(asCDateFmt("yyyy/mm/dd"), "%Y/%m/%d")
  expect_equal(asCDateFmt("yyyy mm dd"), "%Y %m %d")
})
