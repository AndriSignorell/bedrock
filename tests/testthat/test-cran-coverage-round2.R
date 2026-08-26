# Additional coverage tests for bedrock
#
# Copy this file to tests/testthat/.
# The tests are additive, deterministic and do not perform network requests.

# findDownload ---------------------------------------------------------

test_that(".getDownloadsPath returns a non-empty scalar path", {
  out <- .getDownloadsPath()

  expect_type(out, "character")
  expect_length(out, 1L)
  expect_true(nzchar(out))
})

test_that(".getDownloadsPath reads the Windows Downloads registry entry", {
  if (.Platform$OS.type == "windows") {
    guid <- "{374DE290-123F-4565-9164-39C4925E467B}"
    registry <- setNames(list("C:\\Users\\tester\\Downloads"), guid)

    local_mocked_bindings(
      readRegistry = function(...) registry,
      .package = "utils"
    )

    expect_identical(
      .getDownloadsPath(),
      normalizePath(registry[[guid]], winslash = "/", mustWork = FALSE)
    )
  } else {
    expect_true(TRUE)
  }
})

test_that(".getDownloadsPath falls back when the Windows registry fails", {
  if (.Platform$OS.type == "windows") {
    local_mocked_bindings(
      readRegistry = function(...) stop("registry unavailable"),
      .package = "utils"
    )

    expect_identical(
      .getDownloadsPath(),
      file.path(path.expand("~"), "Downloads")
    )
  } else {
    expect_true(TRUE)
  }
})

test_that(".getDownloadsPath expands the Linux XDG home variable", {
  if (Sys.info()[["sysname"]] == "Linux") {
    home <- tempfile("bedrock-home-")
    configDir <- file.path(home, ".config")
    dir.create(configDir, recursive = TRUE)
    on.exit(unlink(home, recursive = TRUE), add = TRUE)

    writeLines(
      'XDG_DOWNLOAD_DIR="$HOME/My Downloads"',
      file.path(configDir, "user-dirs.dirs")
    )

    local_mocked_bindings(
      path.expand = function(path) {
        if (identical(path, "~")) home else path
      },
      .package = "base"
    )

    expect_identical(
      .getDownloadsPath(),
      normalizePath(
        file.path(home, "My Downloads"),
        winslash = "/",
        mustWork = FALSE
      )
    )
  } else {
    expect_true(TRUE)
  }
})

# binaryTree -----------------------------------------------------------

test_that("binaryTree returns the documented ordering", {
  expect_identical(binaryTree(1), 1L)
  expect_identical(
    binaryTree(13),
    c(8L, 4L, 9L, 2L, 10L, 5L, 11L, 1L, 12L, 6L, 13L, 3L, 7L)
  )
})

test_that("binaryTree rejects non-whole and irregular inputs", {
  expect_error(binaryTree(2.5), "whole number")
  expect_error(binaryTree(NA_real_), "positive integer")
  expect_error(binaryTree("3"), "positive integer")
  expect_error(binaryTree(numeric()), "positive integer")
})

# char-ascii-conversion ------------------------------------------------

test_that("charToAscii simplifies several one-character strings", {
  expect_identical(
    charToAscii(c("A", "B", "C")),
    c(65L, 66L, 67L)
  )
})

test_that("charToAscii retains mixed-width strings as a list", {
  expect_identical(
    charToAscii(c("A", "BC")),
    list(65L, c(66L, 67L))
  )
  expect_identical(charToAscii(character(), output = "list"), list())
  expect_error(charToAscii("A", output = "matrix"), "arg")
})

test_that("asciiToChar converts vectors including printable boundaries", {
  expect_identical(
    asciiToChar(c(32L, 65L, 126L)),
    c(" ", "A", "~")
  )
  expect_identical(asciiToChar(integer()), character())
})

# isEuclid -------------------------------------------------------------

test_that("isEuclid detects non-Euclidean distances at different scales", {
  D <- matrix(
    c(
      0, 1, 1,
      1, 0, 3,
      1, 3, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  expect_false(isEuclid(as.dist(D)))
  expect_false(isEuclid(as.dist(D * 1e-8)))
  expect_false(isEuclid(as.dist(D * 1e8)))
})

test_that("isEuclid handles zero distances and reports diagnostics", {
  out <- isEuclid(as.dist(matrix(0, nrow = 3, ncol = 3)), tol = 1e-8)

  expect_true(out)
  expect_length(attr(out, "eigenvalues"), 3L)
  expect_identical(
    attr(out, "minEigenvalue"),
    min(attr(out, "eigenvalues"))
  )
  expect_identical(attr(out, "tol"), 1e-8)
})

test_that("isEuclid validates its tolerance", {
  d <- dist(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))

  for (tol in list(-1, NA_real_, Inf, c(0, 1), "0.1")) {
    expect_error(
      isEuclid(d, tol = tol),
      "single non-negative number",
      info = paste("tol =", paste(tol, collapse = ", "))
    )
  }
})

test_that("isEuclid rejects undersized and missing distance matrices", {
  expect_error(
    isEuclid(as.dist(matrix(0, nrow = 1, ncol = 1))),
    "at least two"
  )

  D <- matrix(c(0, NA, NA, 0), nrow = 2)
  expect_error(isEuclid(as.dist(D)), "missing values")
})

# resolveContingency ---------------------------------------------------

test_that("resolveContingency validates matrix entries", {
  expect_error(
    resolveContingency(matrix(c(1, -1, 2, 3), nrow = 2)),
    "nonnegative and finite"
  )
  expect_error(
    resolveContingency(matrix(c(1, Inf, 2, 3), nrow = 2)),
    "nonnegative and finite"
  )
  expect_error(
    resolveContingency(matrix(c(1, NA, 2, 3), nrow = 2)),
    "nonnegative and finite"
  )
})

test_that("resolveContingency can allow non-integer counts", {
  m <- matrix(c(1.5, 2.5, 3.5, 4.5), nrow = 2)

  expect_no_warning(
    out <- resolveContingency(m, integerCounts = FALSE)
  )
  expect_equal(out$n, 12)
})

test_that("resolveContingency validates paired vectors", {
  expect_error(resolveContingency(c("A", "B")), "must be given")
  expect_error(
    resolveContingency(c("A", "B"), c("X")),
    "same length"
  )
  expect_error(
    resolveContingency(c("A", "A"), c("X", "Y")),
    "at least 2 levels"
  )
})

test_that("resolveContingency removes incomplete pairs", {
  x <- c("A", "A", "B", "B", NA)
  y <- c("X", "Y", "X", "Y", "X")

  out <- resolveContingency(x, y, data.name = "complete groups")

  expect_equal(out$n, 4)
  expect_identical(unname(rowSums(out$table)), c(2, 2))
  expect_identical(out$data.name, "complete groups")
})

test_that("resolveContingency handles square tables and data names", {
  m <- matrix(1:4, nrow = 2)
  out <- resolveContingency(m, square = TRUE, data.name = "custom table")

  expect_identical(out$k, 2L)
  expect_identical(out$data.name, "custom table")
  expect_error(
    resolveContingency(matrix(1, nrow = 1), square = TRUE),
    "at least two rows"
  )
})

test_that("resolveContingency derives default data names", {
  x <- c("A", "A", "B", "B")
  y <- c("X", "Y", "X", "Y")
  m <- matrix(1:4, nrow = 2)

  expect_identical(resolveContingency(x, y)$data.name, "x and y")
  expect_identical(resolveContingency(m)$data.name, "m")
})

# isURL ----------------------------------------------------------------

test_that("isURL recognises all documented schemes case-insensitively", {
  urls <- c(
    "HTTPS://example.com/data.csv",
    "ftps://files.example.org/data.csv",
    "file:///tmp/data.csv",
    "gs://bucket/data.csv",
    "az://container/data.csv"
  )

  expect_true(all(vapply(urls, isURL, logical(1L))))
  expect_false(any(vapply(urls, isFilePath, logical(1L))))
})

test_that("isFilePath recognises ambiguous and backslash paths", {
  paths <- c(
    "folder/data.csv",
    "folder\\data.csv",
    "\\\\server\\share\\data.csv"
  )

  expect_true(all(vapply(paths, isFilePath, logical(1L))))
  expect_false(any(vapply(paths, isURL, logical(1L))))
})

test_that("URL detection distinguishes unknown strings", {
  expect_identical(.detectInputType("report.csv"), "unknown")
  expect_identical(.detectInputType(""), "unknown")
  expect_false(isURL("mailto:user@example.com"))
  expect_false(isFilePath("mailto:user@example.com"))
})

test_that("URL and path detection validate scalar character input", {
  expect_error(isURL(1), "single character string")
  expect_error(isURL(character()), "single character string")
  expect_error(isFilePath(c("a", "b")), "single character string")
})
