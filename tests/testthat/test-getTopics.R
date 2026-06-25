# ------------------------------------------------------------------------------
# getTopics
# ------------------------------------------------------------------------------

test_that("getTopics returns sorted unique topics", {
  fake_rd <- list(
    "foo.Rd" = structure(
      list(
        structure(list("topic.regression"), "Rd_tag" = "\\concept"),
        structure(list("topic.anova"),       "Rd_tag" = "\\concept"),
        structure(list("other-concept"),     "Rd_tag" = "\\concept")
      ),
      class = "Rd"
    )
  )
  mockery::stub(getTopics, "tools::Rd_db", function(...) fake_rd)
  res <- getTopics("somepkg")
  expect_equal(res, c("anova", "regression"))
})

test_that("getTopics returns character(0) when no concepts", {
  mockery::stub(getTopics, "tools::Rd_db", function(...) list())
  expect_equal(getTopics("somepkg"), character(0))
})

test_that("getTopics sort = FALSE preserves order", {
  fake_rd <- list(
    "foo.Rd" = structure(
      list(
        structure(list("topic.z"), "Rd_tag" = "\\concept"),
        structure(list("topic.a"), "Rd_tag" = "\\concept")
      ),
      class = "Rd"
    )
  )
  mockery::stub(getTopics, "tools::Rd_db", function(...) fake_rd)
  res <- getTopics("somepkg", sort = FALSE)
  expect_equal(res, c("z", "a"))
})


