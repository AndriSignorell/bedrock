
# ------------------------------------------------------------------------------
# funKeywords
# ------------------------------------------------------------------------------

test_that("funKeywords without topic calls file.show", {
  called_with <- NULL
  mockery::stub(funKeywords, "file.show",
                function(f) { called_with <<- f })
  funKeywords()
  expect_true(grepl("KEYWORDS", called_with))
})

test_that("funKeywords returns keywords for known topic", {
  mockery::stub(funKeywords, "scan", function(...) {
    c("math&Arithmetic")   # kein Leerzeichen, exakt ein Token nach gsub
  })
  mockery::stub(funKeywords, "help.search", function(keyword, ...) {
    matches <- if (keyword == "math")
      data.frame(topic = "mean", Package = "base", stringsAsFactors = FALSE)
    else
      data.frame(topic = character(0), Package = character(0),
                 stringsAsFactors = FALSE)
    structure(list(matches = matches), class = "hsearch")
  })
  res <- funKeywords("mean")
  expect_true("math" %in% res)
})

test_that("funKeywords returns NULL/empty for unknown topic", {
  mockery::stub(funKeywords, "scan", function(...) c("math&Arithmetic"))
  mockery::stub(funKeywords, "help.search", function(...) {
    structure(
      list(matches = data.frame(topic = character(0),
                                stringsAsFactors = FALSE)),
      class = "hsearch"
    )
  })
  res <- funKeywords("nonexistent_fn_xyz")
  expect_true(length(res) == 0)   # NULL oder character(0) — beide akzeptiert
})

test_that("funKeywords accepts string topic", {
  mockery::stub(funKeywords, "scan", function(...) {
    c("math & arith & Arithmetic")
  })
  mockery::stub(funKeywords, "help.search", function(keyword, ...) {
    matches <- data.frame(topic = "mean", Package = "base",
                          stringsAsFactors = FALSE)
    structure(list(matches = matches), class = "hsearch")
  })
  res_sym <- funKeywords(mean)
  res_str <- funKeywords("mean")
  expect_equal(res_sym, res_str)
})

