# Additional coverage tests for bedrock 0.1.2
#
# Copy this file to tests/testthat/.
# It contains only new tests and does not replace existing test files.

# coalesceX ------------------------------------------------------------

test_that("coalesceX method is.null operates on whole objects", {
  obj <- data.frame(x = 1:2)

  expect_identical(
    coalesceX(NULL, NULL, obj, method = "is.null"),
    obj
  )
  expect_identical(
    coalesceX(list(NULL, 7), method = "is.null"),
    7
  )
  expect_null(coalesceX(NULL, NULL, method = "is.null"))
})

test_that("coalesceX supports flatten = FALSE", {
  expect_identical(
    coalesceX(c(NA, 2), 1, flatten = FALSE),
    c(1, 2)
  )
})

test_that("coalesceX does not recycle non-scalar inputs", {
  expect_error(
    coalesceX(c(NA, 2), c(1, NA, 3, 4))
  )
})

test_that("coalesceX validates method", {
  expect_error(coalesceX(1, method = "unknown"), "arg")
})

# compareDataFrames ----------------------------------------------------

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

# download -------------------------------------------------------------

test_that("readDownload reads CSV and TSV files", {
  skip_if_not_installed("readr")

  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(c(csv, tsv)), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b"), csv)
  writeLines(c("id\tvalue", "1\ta", "2\tb"), tsv)

  current <- csv
  local_mocked_bindings(findDownload = function(...) current)

  out_csv <- readDownload("data.csv")
  current <- tsv
  out_tsv <- readDownload("data.tsv")

  expect_s3_class(out_csv, "data.frame")
  expect_false(inherits(out_csv, "tbl_df"))
  expect_identical(out_csv$id, c(1, 2))
  expect_identical(out_tsv, out_csv)
})

test_that("readDownload reads delimited text and can retain a tibble", {
  skip_if_not_installed("readr")

  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id|value", "1|a", "2|b"), tf)

  local_mocked_bindings(findDownload = function(...) tf)

  out <- readDownload("data.txt", delim = "|", output = "tibble")

  expect_s3_class(out, "tbl_df")
  expect_identical(out$id, c(1, 2))
})

test_that("readDownload validates the extension and output", {
  local_mocked_bindings(findDownload = function(...) "/tmp/file")
  expect_error(readDownload("file"), "has no extension")

  expect_error(readDownload("file.csv", output = "matrix"), "arg")
})

# funArgs --------------------------------------------------------------

test_that("funArgs supports all output formats", {
  f <- function(x, y = 2, ..., z = quote(a + b)) NULL

  as_list <- funArgs(f, output = "list")
  as_string <- funArgs(f, output = "string")
  as_data_frame <- funArgs(f)

  expect_identical(as_list, formals(f))
  expect_identical(
    as_string,
    "x, y = 2, ..., z = quote(a + b)"
  )
  expect_s3_class(as_data_frame, "FunArgs")
  expect_identical(attr(as_data_frame, "string"), as_string)
})

test_that("funArgs resolves an exported function from a package", {
  out <- funArgs("mean", package = "base", output = "list")

  expect_identical(out, formals(base::mean))
})

test_that("funArgs sorts names while keeping dots last", {
  f <- function(z = 1, ..., a, m = 2) NULL
  out <- funArgs(f, sorted = TRUE)

  expect_identical(out$name, c("a", "m", "z", "..."))
})

test_that("funArgs handles functions without arguments", {
  out <- funArgs(function() NULL)

  expect_s3_class(out, "FunArgs")
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), c("name", "value"))
  expect_identical(funArgs(function() NULL, output = "list"), list())
  expect_identical(funArgs(function() NULL, output = "string"), "")
})

# getConcepts ----------------------------------------------------------

test_that("getConcepts extracts unique sorted concepts", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{mock}",
      "\\alias{mock}",
      "\\title{Mock documentation}",
      "\\concept{zeta}",
      "\\concept{number-theory}",
      "\\concept{alpha}",
      "\\concept{zeta}"
    ),
    tf
  )

  fakeRd <- list("mock.Rd" = tools::parse_Rd(tf))
  local_mocked_bindings(
    Rd_db = function(...) fakeRd,
    .package = "tools"
  )

  concepts <- getConcepts("mockPackage")

  expect_identical(concepts, c("alpha", "number-theory", "zeta"))
})

test_that("getConcepts filters and removes a prefix", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{mock}",
      "\\alias{mock}",
      "\\title{Mock documentation}",
      "\\concept{number-theory}",
      "\\concept{number-prime}",
      "\\concept{comparison}"
    ),
    tf
  )

  fakeRd <- list("mock.Rd" = tools::parse_Rd(tf))
  local_mocked_bindings(
    Rd_db = function(...) fakeRd,
    .package = "tools"
  )

  concepts <- getConcepts("mockPackage", prefix = "number-")

  expect_identical(concepts, c("prime", "theory"))
})

# getRdLabels ----------------------------------------------------------

test_that("rdLabels extracts names and descriptions", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{ExampleData}",
      "\\alias{ExampleData}",
      "\\title{Example data}",
      "\\format{A data frame with two variables:",
      "\\describe{",
      "  \\item{x}{First variable.}",
      "  \\item{long_name}{Second variable with several words.}",
      "}",
      "}"
    ),
    tf
  )

  fake_rd <- list("ExampleData.Rd" = tools::parse_Rd(tf))
  local_mocked_bindings(Rd_db = function(...) fake_rd)

  out <- rdLabels("ExampleData", "bedrock")

  expect_identical(
    out,
    c(
      x = "First variable.",
      long_name = "Second variable with several words."
    )
  )
})

# isPrime --------------------------------------------------------------

test_that("isPrime identifies known primes and composites", {
  x <- c(-3, 0, 1, 2, 3, 4, 17, 18, 19)
  expected <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
                TRUE, FALSE, TRUE)

  expect_identical(isPrime(x), expected)
})

test_that("isPrime returns FALSE for irregular numeric values", {
  x <- c(NA_real_, NaN, Inf, -Inf, 2.5)

  expect_identical(isPrime(x), rep(FALSE, length(x)))
  expect_error(isPrime("7"), "numeric vector")
})

test_that("isPrime agrees with primes over a reference range", {
  x <- 1:1000
  expected <- x %in% primes(max(x))

  expect_identical(isPrime(x), expected)
})

test_that("isPrime preserves length including empty input", {
  expect_identical(isPrime(numeric()), logical())
  expect_length(isPrime(c(2, 3, 5)), 3L)
})

# label ----------------------------------------------------------------

test_that("label returns NA for unlabelled variables", {
  df <- data.frame(a = 1:3, b = 4:6)
  label(df$a) <- "A"

  expect_identical(label(df, vars = TRUE), c(a = "A", b = NA_character_))
  expect_null(label(df))
})

test_that("label accepts names and numeric column indices", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  label(df, vars = c("a", "c")) <- c("A", "C")
  expect_identical(label(df, vars = c("c", "a")), c(c = "C", a = "A"))

  label(df, vars = c(1, 2)) <- c("First", "Second")
  expect_identical(
    label(df, vars = c(1, 2)),
    c(a = "First", b = "Second")
  )
})

test_that("label removes object and variable labels with NULL", {
  df <- data.frame(a = 1:3, b = 4:6)
  label(df) <- "Dataset"
  label(df, vars = TRUE) <- c("A", "B")

  label(df) <- NULL
  expect_null(label(df))

  label(df, vars = TRUE) <- NULL
  expect_identical(
    label(df, vars = TRUE),
    c(a = NA_character_, b = NA_character_)
  )
})

test_that("label validates vars and replacement values", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(label(df, vars = "missing"), "Unknown variable")
  expect_error(label(df, vars = 0), "out of range")
  expect_error(label(df, vars = 3), "out of range")
  expect_error(label(df, vars = FALSE), "must be TRUE")

  expect_error(label(df, vars = TRUE) <- c("A", "B", "C"),
               "same length as vars")
  expect_error(label(df) <- c("A", "B"), "length 1")
  expect_error(label(df) <- list("A"), "cannot assign a list")
})

# locf -----------------------------------------------------------------

test_that("locf handles data frames column by column", {
  x <- data.frame(
    a = c(NA, 1, NA, 3),
    b = c(10, NA, 20, NA)
  )
  expected <- data.frame(
    a = c(NA, 1, 1, 3),
    b = c(10, 10, 20, 20)
  )

  out <- locf(x)

  expect_s3_class(out, "data.frame")
  expect_identical(out, expected)
})

test_that("locf handles matrix columns independently", {
  x <- cbind(
    a = c(NA, 1, NA, 3),
    b = c(10, NA, 20, NA)
  )
  expected <- cbind(
    a = c(NA, 1, 1, 3),
    b = c(10, 10, 20, 20)
  )

  out <- locf(x)

  expect_true(is.matrix(out))
  expect_identical(out, expected)
})

test_that("locf preserves factor levels and ordering", {
  x <- ordered(
    c(NA, "low", NA, "high", NA),
    levels = c("low", "medium", "high")
  )

  out <- locf(x)

  expect_true(is.ordered(out))
  expect_identical(levels(out), levels(x))
  expect_identical(
    as.character(out),
    c(NA, "low", "low", "high", "high")
  )
})

test_that("locf preserves empty vector types", {
  expect_identical(locf(numeric()), numeric())
  expect_identical(locf(character()), character())
  expect_identical(locf(as.Date(character())), as.Date(character()))
})

# openDataObject -------------------------------------------------------

test_that("openDataObject returns unlabelled data when doc is NA", {
  dat <- data.frame(id = 1:3, value = c("a", "b", "c"))

  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Success"),
    status_code = function(...) 200L,
    .package = "httr"
  )
  local_mocked_bindings(
    read_excel = function(path, sheet = NULL, ...) dat,
    excel_sheets = function(path) "Data",
    .package = "readxl"
  )

  out <- openDataObject("example.xlsx", url = "https://example.test", doc = NA)

  expect_s3_class(out, "data.frame")
  expect_identical(out, dat)
})

test_that("openDataObject applies codes, factor order and labels", {
  dat <- data.frame(
    sex = c(1, 2, 3),
    grade = c(1, 2, 3),
    value = c(10, 20, 30)
  )
  code <- data.frame(
    Variable = c("sex", "grade", "value"),
    Beschreibung = c("Sex", "Grade", "Measured value"),
    Codes = c(
      "1=Male\r\n2=Female",
      "1=Low\r\n2=Medium\r\n3=High",
      NA_character_
    ),
    Skala = c("nominal", "ordinal", "metric")
  )

  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Success"),
    status_code = function(...) 200L,
    .package = "httr"
  )
  local_mocked_bindings(
    read_excel = function(path, sheet = NULL, ...) {
      if (is.null(sheet)) dat else code
    },
    excel_sheets = function(path) c("Data", "Description"),
    .package = "readxl"
  )

  out <- openDataObject("example.xlsx", url = "https://example.test")

  expect_true(is.factor(out$sex))
  expect_false(is.ordered(out$sex))
  expect_identical(levels(out$sex), c("Male", "Female"))
  expect_identical(as.character(out$sex), c("Male", "Female", NA_character_))

  expect_true(is.ordered(out$grade))
  expect_identical(levels(out$grade), c("Low", "Medium", "High"))
  expect_identical(as.character(out$grade), c("Low", "Medium", "High"))

  expect_identical(label(out$sex), "Sex")
  expect_identical(label(out$grade), "Grade")
  expect_identical(label(out$value), "Measured value")
})

test_that("openDataObject only auto-detects a Description sheet", {
  dat <- data.frame(id = 1:2, value = c("a", "b"))

  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Success"),
    status_code = function(...) 200L,
    .package = "httr"
  )
  local_mocked_bindings(
    read_excel = function(path, sheet = NULL, ...) {
      if (!is.null(sheet))
        stop("metadata sheet must not be read")
      dat
    },
    excel_sheets = function(path) c("Data", "Notes"),
    .package = "readxl"
  )

  out <- openDataObject("example.xlsx", url = "https://example.test")

  expect_identical(out, dat)
})

test_that("openDataObject reports HTTP failures", {
  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Client error"),
    status_code = function(...) 404L,
    .package = "httr"
  )

  expect_error(
    openDataObject("missing.xlsx", url = "https://example.test", doc = NA),
    "Download failed \\[404\\]"
  )
})

# peekFile -------------------------------------------------------------

test_that("peekFile reads only the requested number of rows", {
  skip_if_not_installed("readr")

  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b", "3,c"), tf)

  out <- peekFile(tf, n = 2, delim = ",")

  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "tbl_df"))
  expect_equal(nrow(out), 2L)
  expect_identical(out$id, c(1, 2))
})

test_that("peekFile can retain tibble output", {
  skip_if_not_installed("readr")

  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b"), tf)

  out <- peekFile(tf, n = 1, delim = ",", output = "tibble")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
})

test_that("peekFile ignores internally managed reader arguments", {
  skip_if_not_installed("readr")

  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b"), tf)

  expect_warning(
    out <- peekFile(
      tf,
      n = 1,
      delim = ",",
      n_max = 99,
      show_col_types = TRUE
    ),
    "Ignoring argument"
  )
  expect_equal(nrow(out), 1L)
})

test_that("peekFile validates n and output", {
  skip_if_not_installed("readr")

  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id", "1"), tf)

  for (n in list(0, -1, 1.5, NA_real_, Inf, c(1, 2), "1")) {
    expect_error(
      peekFile(tf, n = n, delim = ","),
      "single positive integer",
      info = paste("n =", paste(n, collapse = ", "))
    )
  }

  expect_error(peekFile(tf, output = "matrix"), "arg")
})

# rankX ----------------------------------------------------------------

test_that("rankX agrees with base rank for common tie methods", {
  x <- c(30, 10, 20, 20, NA)

  for (method in c("average", "first", "max", "min")) {
    expect_equal(
      rankX(x, ties.method = method, na.last = "keep"),
      rank(x, ties.method = method, na.last = "keep"),
      info = paste("ties.method =", method)
    )
  }
})

test_that("rankX supports last and random tie handling", {
  x <- c(2, 1, 2, 1)

  expect_identical(rankX(x, ties.method = "last"), c(4L, 2L, 3L, 1L))

  set.seed(42)
  out <- rankX(x, ties.method = "random")
  expect_equal(sort(out), 1:4)
})

test_that("rankX ranks several vectors lexicographically", {
  a <- c(1, 1, 2, 2)
  b <- c(2, 1, 2, 1)

  expect_equal(rankX(a, b), c(2, 1, 4, 3))
  expect_equal(
    rankX(a, b, decreasing = c(FALSE, TRUE)),
    1:4
  )
})

test_that("rankX handles NA placement", {
  x <- c(3, NA, 1, 2)

  expect_equal(rankX(x, na.last = TRUE), rank(x, na.last = TRUE))
  expect_equal(rankX(x, na.last = FALSE), rank(x, na.last = FALSE))
  expect_equal(rankX(x, na.last = "keep"), rank(x, na.last = "keep"))
})

test_that("rankX validates its inputs", {
  expect_error(rankX(), "no input vectors")
  expect_error(rankX(1:3, decreasing = NA), "decreasing")
  expect_error(rankX(1:3, decreasing = 1), "decreasing")
  expect_error(rankX(1:3, ties.method = "unknown"), "arg")
})

# rdTitle --------------------------------------------------------------

test_that("rdTitle resolves names and aliases", {
  td <- tempfile("rd-title-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "\\name{exampleTopic}",
      "\\alias{exampleTopic}",
      "\\alias{exampleAlias}",
      "\\title{An Example Title}"
    ),
    file.path(td, "exampleTopic.Rd")
  )

  expect_identical(rdTitle("exampleTopic", man = td), "An Example Title")
  expect_identical(rdTitle("exampleAlias", man = td), "An Example Title")
})

test_that("rdTitle returns NA for an unknown topic or missing title", {
  td <- tempfile("rd-title-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  writeLines(
    c("\\name{untitled}", "\\alias{untitled}"),
    file.path(td, "untitled.Rd")
  )

  expect_identical(rdTitle("untitled", man = td), NA_character_)
  expect_identical(rdTitle("unknown", man = td), NA_character_)
})

test_that("rdTitle validates its inputs", {
  td <- tempfile("rd-title-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  expect_error(rdTitle(c("a", "b"), man = td), "single character string")
  expect_error(rdTitle(1, man = td), "single character string")
  expect_error(rdTitle("a", man = file.path(td, "missing")),
               "Directory not found")
  expect_error(rdTitle("a", man = td), "No \\.Rd files found")
})

# revCode --------------------------------------------------------------

test_that("numeric min and max must be supplied together", {
  expect_error(revCode(1:3, min = 1), "both `min` and `max`")
  expect_error(revCode(1:3, max = 5), "both `min` and `max`")
  expect_error(revCode(1:3, min = 5, max = 1), "must be <=")
})

test_that("explicit ranges warn about values outside the scale", {
  expect_warning(
    out <- revCode(c(0, 1, 5, 6), min = 1, max = 5),
    "outside \\[min, max\\]"
  )
  expect_equal(out, c(6, 5, 1, 0))
})

test_that("implicit ranges warn when NA is not removed", {
  expect_warning(
    out <- revCode(c(1, NA, 3)),
    "na.rm = FALSE"
  )
  expect_true(all(is.na(out)))
})

test_that("factor reverse coding preserves missing values and ordering class", {
  x <- factor(
    c("low", NA, "high"),
    levels = c("low", "medium", "high"),
    ordered = FALSE
  )

  out <- revCode(x)

  expect_false(is.ordered(out))
  expect_identical(levels(out), c("high", "medium", "low"))
  expect_identical(as.character(out), c("high", NA, "low"))
})

# roundTo --------------------------------------------------------------

test_that("roundTo supports all documented rounding directions", {
  x <- c(-1.26, 1.26)

  expect_equal(roundTo(x, 0.1, FUN = floor), c(-1.3, 1.2))
  expect_equal(roundTo(x, 0.1, FUN = ceiling), c(-1.2, 1.3))
  expect_equal(roundTo(x, 0.1, FUN = trunc), c(-1.2, 1.2))
})

test_that("roundTo follows round-to-even at exact ties", {
  expect_equal(roundTo(c(2.5, 3.5), 1), c(2, 4))
})

test_that("roundTo handles negative multiples only with non-positive x", {
  expect_equal(roundTo(-10, -3), -9)
  expect_error(roundTo(5, -2), "negative `multiple`")
  expect_error(roundTo(c(-5, 5), -2), "negative `multiple`")
})

test_that("roundTo rejects zero and missing multiples", {
  expect_error(roundTo(1, 0), "must not be 0")
  expect_error(roundTo(1, NA_real_))
})
