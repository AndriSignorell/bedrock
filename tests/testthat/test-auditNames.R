# Naming audit, design rules section 3. The rules are checked by
# bedrock::auditNames(); what is listed here is accepted, each entry with
# its reason. An entry starting with "open:" is a name still to be
# decided - it documents a debt and is removed when the name changes.

.auditNamesExceptions <- c(
  "isNA"                                       = "NA is R's own constant, not an acronym",
  "checkConfLevel(allowNA)"                    = "NA is R's own constant, not an acronym",
  "coalesceX(method = \"is.na\")"              = "the values are the names of the predicates that are applied",
  "coalesceX(method = \"is.null\")"            = "the values are the names of the predicates that are applied",
  "coalesceX(method = \"is.finite\")"          = "the values are the names of the predicates that are applied",
  "moveAvg(endrule = \"NA\")"                  = "the value names what is filled in at the boundary",
  "withSeed(seed)"                             = "the one function whose job is to set the seed",
  "funArgs(fun)"                               = "the function that is inspected, not one that is applied (section 3.3 H)",
  "funCalls(fun)"                              = "the function that is inspected, not one that is applied (section 3.3 H)"
)


test_that("exported names and arguments follow the naming rules", {

  res <- bedrock::auditNames("bedrock",
                             exceptions = names(.auditNamesExceptions))

  expect_identical(
    nrow(res), 0L,
    info = paste(sprintf("%s [%s] %s", res$key, res$rule, res$detail),
                 collapse = "\n"))
})


test_that("every accepted exception still matches a finding", {

  res <- bedrock::auditNames("bedrock",
                             exceptions = names(.auditNamesExceptions))

  expect_identical(attr(res, "unused"), character(0))
})


test_that("camelCase words respect the closed list of abbreviations", {

  expect_identical(bedrock:::.auditWords("meanCI"), c("mean", "CI"))
  expect_identical(bedrock:::.auditWords("binomCIn"), c("binom", "CI", "n"))
  expect_identical(bedrock:::.auditWords("plotECDF"), c("plot", "ECDF"))
  expect_identical(bedrock:::.auditWords("plotXY"), c("plot", "XY"))
  expect_identical(bedrock:::.auditWords("toHtmlTable"),
                   c("to", "Html", "Table"))

  # an acronym that is not on the list comes back as a run of capitals
  expect_identical(bedrock:::.auditWords("colToRGB"), c("col", "To", "RGB"))
  expect_identical(bedrock:::.auditWords("hotellingT2Test"),
                   c("hotelling", "T", "2", "Test"))
})


test_that("a name is taken exactly or in the case of the whole word only", {

  set <- c("IQR", "combn", "strsplit", "mean")

  expect_true(bedrock:::.auditTaken("mean", set))
  expect_true(bedrock:::.auditTaken("iqr", set))

  # an internal capital makes a name of its own
  expect_false(bedrock:::.auditTaken("combN", set))
  expect_false(bedrock:::.auditTaken("strSplit", set))
  expect_false(bedrock:::.auditTaken("gmean", set))
})


test_that("forwarding is recognised by name and by value", {

  byName  <- quote({ grepl(pattern, x, ignore.case = ignore.case) })
  byValue <- quote({ p.adjust(p, method = p.adjust.method) })
  none    <- quote({ if (ignore.case) x <- tolower(x); x })

  expect_true(bedrock:::.auditForwards(byName, "ignore.case"))
  expect_true(bedrock:::.auditForwards(byValue, "p.adjust.method"))
  expect_false(bedrock:::.auditForwards(none, "ignore.case"))
})


test_that("auditNames() validates its arguments and reports unused exceptions", {

  expect_error(auditNames(c("bedrock", "stats")), "package")
  expect_error(auditNames("bedrock", exceptions = 1), "exceptions")

  res <- auditNames("bedrock", exceptions = "noSuchFunction(arg)")
  expect_identical(attr(res, "unused"), "noSuchFunction(arg)")
  expect_named(res, c("package", "fun", "arg", "rule", "detail", "key"))
})
