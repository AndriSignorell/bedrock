library(testthat)

test_that("%:% selects between first occurrences", {

  expect_equal(
    letters %:% c("c", "g"),
    c("c","d","e","f","g")
  )
})

test_that("%:% uses first match for both from and to (lazy)", {

  x <- c("a", "b", "c", "d", "c", "e", "f", "c")

  expect_equal(
    x %:% c("c", "e"),
    c("c","d","c","e")
  )
})

test_that("%:% works on the practical colnames use case", {

  cols <- c("id", "name", "price", "size", "nps", "weight")

  expect_equal(
    cols %:% c("price", "nps"),
    c("price","size","nps")
  )
})

test_that("%:% returns a single element when from equals to", {

  expect_equal(
    letters %:% c("d", "d"),
    "d"
  )
})

test_that("%:% returns the full vector when from is first and to is last element", {

  expect_equal(
    letters %:% c("a", "z"),
    letters
  )
})

test_that("%:% errors when from occurs after to", {

  expect_error(
    letters %:% c("m", "c")
  )
})

test_that("%:% errors when from is not found", {

  expect_error(
    letters %:% c("not_there", "c")
  )
})

test_that("%:% errors when to is not found", {

  expect_error(
    letters %:% c("c", "not_there")
  )
})

test_that("%:% errors when rng does not have length 2", {

  expect_error(
    letters %:% c("c")
  )

  expect_error(
    letters %:% c("c", "d", "e")
  )
})

test_that("%:% supports NA in rng to match missing values in x", {

  x <- c("a", NA, "b", "c")

  expect_equal(
    x %:% c(NA, "b"),
    c(NA, "b")
  )
})

test_that("%:% errors when NA is requested but not present in x", {

  x <- c("a", "b", "c")

  expect_error(
    x %:% c(NA, "b")
  )
})


test_that("%::% selects from first occurrence to last occurrence", {

  x <- c("B", "A", "X", "K", "S", "K", "G", "L", "K", "V", "K", "Z")

  expect_equal(
    x %::% c("A", "K"),
    x[2:11]
  )
})

test_that("%::% and %:% agree when the to-value occurs only once", {

  x <- c("a", "b", "c", "d", "c", "e", "f", "c")

  expect_equal(
    x %::% c("c", "e"),
    x %:% c("c", "e")
  )
})

test_that("%::% differs from %:% when to-value has multiple occurrences", {

  x <- c("a", "b", "c", "d", "c", "e", "f", "c")

  expect_equal(
    x %::% c("a", "c"),
    x  # runs to the last "c", i.e. the whole vector
  )

  expect_equal(
    x %:% c("a", "c"),
    c("a","b","c")  # stops at the first "c"
  )
})

test_that("%::% errors when from occurs after to", {

  expect_error(
    letters %::% c("m", "c")
  )
})

test_that("%::% errors when from is not found", {

  expect_error(
    letters %::% c("not_there", "c")
  )
})

test_that("%::% errors when to is not found", {

  expect_error(
    letters %::% c("c", "not_there")
  )
})

test_that("%::% errors when rng does not have length 2", {

  expect_error(
    letters %::% c("c")
  )

  expect_error(
    letters %::% c("c", "d", "e")
  )
})

test_that("%::% supports NA in rng to match missing values in x", {

  x <- c("a", NA, "b", NA, "c")

  expect_equal(
    x %::% c(NA, "c"),
    c(NA, "b", NA, "c")
  )
})

test_that("%::% errors when NA is requested but not present in x", {

  x <- c("a", "b", "c")

  expect_error(
    x %::% c(NA, "b")
  )
})
