
test_that("renameX works correctly", {
  
  # ------------------------------------------------------------------
  # Exact renaming
  # ------------------------------------------------------------------
  
  x <- c(a = 1, b = 2, c = 3)
  
  expect_identical(
    names(renameX(x, a = "alpha", c = "gamma")),
    c("alpha", "b", "gamma")
  )
  
  
  # ------------------------------------------------------------------
  # Positional renaming
  # ------------------------------------------------------------------
  
  expect_identical(
    names(renameX(x, "alpha", "beta")),
    c("alpha", "beta", "c")
  )
  
  
  # ------------------------------------------------------------------
  # Pattern mode with fixed strings
  # ------------------------------------------------------------------
  
  y <- c(v_mean = 1, v_sd = 2, v_n = 3)
  
  expect_identical(
    names(renameX(y, v_ = "", useGsub = TRUE)),
    c("mean", "sd", "n")
  )
  
  
  # ------------------------------------------------------------------
  # Pattern mode with regex
  # ------------------------------------------------------------------
  
  expect_identical(
    names(renameX(
      y,
      `^v_` = "",
      useGsub = TRUE,
      fixed = FALSE
    )),
    c("mean", "sd", "n")
  )
  
  
  # ------------------------------------------------------------------
  # Data frame columns
  # ------------------------------------------------------------------
  
  d <- data.frame(foo = 1:3, bar = 4:6)
  
  expect_identical(
    names(renameX(d, foo = "x", bar = "y")),
    c("x", "y")
  )
  
  
  # ------------------------------------------------------------------
  # Non-existing names trigger warning
  # ------------------------------------------------------------------
  
  expect_warning(
    renameX(x, z = "zzz"),
    "not found"
  )
  
  
  # ------------------------------------------------------------------
  # Non-existing names can be ignored silently
  # ------------------------------------------------------------------
  
  expect_silent(
    renameX(x, z = "zzz", warn = FALSE)
  )
  
  
  # ------------------------------------------------------------------
  # Unknown names leave object unchanged
  # ------------------------------------------------------------------
  
  expect_identical(
    renameX(x, z = "zzz", warn = FALSE),
    x
  )
  
  
  # ------------------------------------------------------------------
  # Mixed named and unnamed replacements are rejected
  # ------------------------------------------------------------------
  
  expect_error(
    renameX(x, a = "alpha", "beta"),
    "Either all or none"
  )
  
  
  # ------------------------------------------------------------------
  # More positional replacements than names
  # ------------------------------------------------------------------
  
  expect_error(
    renameX(x, "a", "b", "c", "d"),
    "More replacement names"
  )
  
  
  # ------------------------------------------------------------------
  # Objects without names are rejected
  # ------------------------------------------------------------------
  
  expect_error(
    renameX(1:3, a = "x"),
    "has no names"
  )
  
  
  # ------------------------------------------------------------------
  # Empty call leaves object unchanged
  # ------------------------------------------------------------------
  
  expect_identical(
    renameX(x),
    x
  )
  
  
  # ------------------------------------------------------------------
  # Multiple substitutions are applied sequentially in pattern mode
  # ------------------------------------------------------------------
  
  z <- c(v_mean = 1, v_sd = 2)
  
  expect_identical(
    names(renameX(
      z,
      v_ = "",
      mean = "avg",
      useGsub = TRUE
    )),
    c("avg", "sd")
  )
  
  
  # ------------------------------------------------------------------
  # Attributes are preserved
  # ------------------------------------------------------------------
  
  d <- data.frame(a = 1:3, b = 4:6)
  
  res <- renameX(d, a = "alpha")
  
  expect_identical(
    class(res),
    class(d)
  )
  
  expect_identical(
    row.names(res),
    row.names(d)
  )
  
})