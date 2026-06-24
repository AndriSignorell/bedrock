

Rcpp::compileAttributes()
devtools::clean_dll()
devtools::check()
devtools::install()
devtools::build_manual(pkg = "C:/temp/DescToolsX")
devtools::build_manual(pkg = "C:/temp/lumen")
devtools::build_manual(pkg = "C:/temp/aurora")
devtools::build_manual(pkg = "C:/temp/bedrock")

devtools::build_manual(pkg = "C:/temp/swissButler")

devtools::document()
devtools::load_all()
devtools::test()


covr::package_coverage()


x <- sample(letters[1:2],10,r=T)

!bin(x)
callIf

sortX(funList("bedrock", ex=T))

grep(sortX(funList("bedrock", ex=T)), v=T)



renameX

bedrock::courseData()


tab <- HairEyeColor[,,1]

sortX(tab, ord = 2, decreasing = TRUE, method = "d")
sortX(tab, ord = 2, decreasing = TRUE, method = "m")
sortX(tab, ord = "Blue", decreasing = TRUE)

str(tab)

DescTools::Sort(tab, ord=2)


(x <- runif(5))
caTools::runmean(x, 3)

bedrock::moveAvg(x, 3, endrule = "trim")

microbenchmark::microbenchmark(
  catools=caTools::runmean(x, 3),
  desctools=bedrock::moveAvg(x, 3, endrule = "trim")
)





# helper
mkdf <- function(data, rnames) {
  df <- as.data.frame(data)
  rownames(df) <- rnames
  df
}

a <- mkdf(list(w = 1:3), c("A", "B", "C"))
b <- mkdf(list(x = 4:6), c("B", "C", "D"))

# debug
res <- merge(
  data.frame(a, rn = row.names(a)),
  data.frame(b, rn = row.names(b)),
  all.x = FALSE, all.y = FALSE
)
print(res)
print(class(res$rn))
print(is.na(res$rn))



c <- mkdf(list(y = 7:9), c("C", "D", "E"))

# Runde 1: a + b  (sollte ok sein)
step1 <- merge(
  data.frame(a, rn = row.names(a)),
  data.frame(b, rn = row.names(b)),
  all.x = FALSE, all.y = FALSE
)
print(step1)

# Runde 2: step1 + c  (hier entsteht das Problem)
step2 <- merge(
  step1,
  data.frame(c, rn = row.names(c)),
  all.x = FALSE, all.y = FALSE
)
print(step2)
print(is.na(step2$rn))


body(multMerge)

data(package = "bedrock")$results[, "Item"]


f <- function(nx = NA, ny = NA) list(nx = nx, ny = ny)

callIf(f, list(nx = NA, ny = NULL), 
       defaults = list(nx = NA, ny = NA))


devtools::document()
devtools::load_all()
devtools::test()

x <- sample(1:10, size=10)

isLowCardinality(x, 2)


fortunes::fortune(220)

bedrock::randGroupSplit(LETTERS[1:17], groupSizes = c(7,6,4))
