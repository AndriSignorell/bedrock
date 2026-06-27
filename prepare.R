

Rcpp::compileAttributes()
devtools::clean_dll()
devtools::check()
devtools::install()
devtools::build_manual(pkg = "C:/temp/DescToolsX")
devtools::build_manual(pkg = "C:/temp/lumen")
devtools::build_manual(pkg = "C:/temp/aurora")
devtools::build_manual(pkg = "C:/temp/bedrock")

devtools::build_manual(pkg = "C:/temp/swissValet")

devtools::document()
devtools::load_all()
devtools::test()


covr::package_coverage()
goodpractice::gp()


pkgdown::build_site()
pkgdown::build_reference_index()

# for themes
# https://bootswatch.com/
  

# rename system data

rename_rda <- function(old_file, old_name, new_name, data_dir = "C:/temp/bedrock/data/") {
  e <- new.env()
  load(file.path(data_dir, old_file), envir = e)
  assign(new_name, get(old_name, envir = e), envir = e)
  rm(list = old_name, envir = e)
  
  tmp <- tempfile(fileext = ".rda")
  save(list = new_name, envir = e, file = tmp, compress = "xz")
  
  file.remove(file.path(data_dir, old_file))
  file.copy(tmp, file.path(data_dir, paste0(new_name, ".rda")))
  file.remove(tmp)
  
  cat(sprintf("  %s -> %s\n", old_name, new_name))
}


rename_rda("d.pizza.rda",  "d.pizza",  "Pizza")
rename_rda("cards.rda",    "cards",    "Cards")
rename_rda("roulette.rda", "roulette", "Roulette")
rename_rda("tarot.rda",    "tarot",    "Tarot")





x <- sample(letters[1:2],10,r=T)

!bin(x)
callIf

sortX(funList("bedrock", ex=T))

grep(sortX(funList("bedrock", ex=T)), v=T)

cut()

renameX

bedrock::courseData()






