
# ***********************************************************************
# Developping Code for DescToolsX
# place in root folder and mention it in .RBuildignore
#
# ***********************************************************************


# available names
available::available("figura", browse = FALSE)


# restart with Ctrl+Shift+F10
.rs.restartR()
Rcpp::compileAttributes()
devtools::clean_dll()

devtools::check()
devtools::install()

setwd("C:/Users/andri/OneDrive/Dokumente/R-Dev/bedrock")
getwd()

devtools::build_manual(pkg = "C:/temp/DescToolsX")
devtools::build_manual(pkg = "C:/temp/lumen")
devtools::build_manual(pkg = "C:/temp/pharos")
devtools::build_manual(pkg = "C:/Users/andri/OneDrive/Dokumente/R-Dev/bedrock")
devtools::build_manual(pkg = "C:/temp/alloy")
devtools::build_manual(pkg = "C:/temp/hermes")
devtools::build_manual(pkg = "C:/temp/pons")
devtools::build_manual(pkg = "C:/temp/swissValet")

devtools::document()
devtools::load_all()

devtools::test()
devtools::run_examples()

devtools::test(filter = "precision")

devtools::check(args = "--as-cran")
devtools::check_mac_release()

tools::compactPDF("vignettes/Combinatorics.pdf", gs_quality = "ebook")

covr::package_coverage()
goodpractice::gp()


pkgdown::build_home()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()

pkgdown::build_reference_index()
pkgdown::build_favicons(overwrite = TRUE)



# 1. Öffnet GitHub im Browser und bereitet einen passenden Token vor
usethis::create_github_token(
  description = "Andri Windows RStudio"
)

# 2. Den auf GitHub erzeugten Token kopieren und hier bei der
#    verdeckten Eingabe einfügen
gitcreds::gitcreds_set()

gh::gh_whoami()

# Wenn dein GitHub-Benutzer korrekt angezeigt wird:
usethis::use_pkgdown_github_pages()

usethis::edit_r_environ()


gitcreds::gitcreds_delete("https://github.com")

usethis::edit_file(".github/workflows/pkgdown.yaml")





# hard CRAN check
rhub::rhub_check()


# for themes
# https://bootswatch.com/



# ==========================================================================
# handling rename system data

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


# put the datasets in sysdata.rda
Prefix <- d.prefix
Units  <- d.units
usethis::use_data(Prefix, Units, internal = TRUE, overwrite = TRUE)




# ==========================================================================
# complete function list

sortX(funList("bedrock", ex=T))
grep(sortX(funList("bedrock", ex=T)), v=T)


pkgs <- c("DescToolsX","lumen","pharos","bedrock","pons","alloy","swissValet")

funs <- setNamesX(lapply(pkgs, bedrock::funList), names=pkgs)




# ==========================================================================
# organise @family and @concept tags systematically

files <- list.files("c:/temp/bedrock/R", full.names = TRUE, pattern = "\\.R$")
files <- list.files("c:/temp/pharos/R", full.names = TRUE, pattern = "\\.R$")
files <- list.files("c:/temp/lumen/R", full.names = TRUE, pattern = "\\.R$")
files <- list.files("c:/temp/alloy/R", full.names = TRUE, pattern = "\\.R$")

tax <- bedrock::toBaseR(readxl::read_excel("c:/temp/DescToolsX_doku/bedrockFamiliesConcepts.xlsx"))

for (f in files) {
  cat("Updating:", f, "\n")
  update_roxy_safe(f, tax)
}


update_roxy_safe <- function(file, tax) {
  
  lines <- readLines(file)
  fun_idx <- grep("^[a-zA-Z0-9_.]+\\s*<-\\s*function", lines)
  
  # rueckwaerts iterieren: Insertions verschieben nur Zeilen unterhalb,
  # die noch abzuarbeitenden Indizes weiter oben bleiben gueltig
  for (i in rev(fun_idx)) {
    
    fun_name <- sub("\\s*<-.*$", "", lines[i])
    
    row <- tax[tax$fun == fun_name, , drop = FALSE]
    if (nrow(row) == 0L) next
    row <- row[1L, ]
    
    # ------------------------------------------------------------------
    # NEUE TAGS AUS family + concept1..4 BAUEN
    # ------------------------------------------------------------------
    
    if (is.na(row$family) || !nzchar(trimws(row$family))) next
    
    conceptCols <- intersect(paste0("concept", 1:4), names(tax))
    concepts <- trimws(as.character(unlist(row[conceptCols], use.names = FALSE)))
    concepts <- concepts[!is.na(concepts) & nzchar(concepts)]
    
    new_tags <- c(
      paste0("#' @family ", trimws(row$family)),
      if (length(concepts)) paste0("#' @concept ", concepts)
    )
    
    # ------------------------------------------------------------------
    # GANZEN ROXYGEN-BLOCK FINDEN (inkl. Leerzeilen)
    # ------------------------------------------------------------------
    
    start <- i - 1
    while (start > 0 && grepl("^#'|^\\s*$", lines[start]))
      start <- start - 1
    start <- start + 1
    end <- i - 1
    
    if (end < start) next   # keine roxygen-Doku vorhanden -> nichts zu tun
    
    block <- lines[start:end]
    
    # ------------------------------------------------------------------
    # ALTE @family/@concept-ZEILEN ENTFERNEN
    # ------------------------------------------------------------------
    
    keep <- rep(TRUE, length(block))
    
    j <- 1L
    while (j <= length(block)) {
      if (grepl("^#'\\s*@(family|concept)\\b", block[j])) {
        keep[j] <- FALSE
        k <- j + 1L
        while (k <= length(block) && grepl("^#'\\s*$", block[k])) {
          keep[k] <- FALSE
          k <- k + 1L
        }
        j <- k
      } else {
        j <- j + 1L
      }
    }
    
    block <- block[keep]
    
    # ------------------------------------------------------------------
    # EINBAU VOR @export (sonst ans Blockende)
    # ------------------------------------------------------------------
    
    export_idx <- grep("^#'\\s*@export\\b", block)
    
    if (length(export_idx))
      block <- append(block, new_tags, after = export_idx[1L] - 1L)
    else
      block <- c(block, new_tags)
    
    # ------------------------------------------------------------------
    # BLOCK ERSETZEN (splice)
    # ------------------------------------------------------------------
    
    lines <- c(
      if (start > 1L) lines[seq_len(start - 1L)] else character(0),
      block,
      if (end < length(lines)) lines[(end + 1L):length(lines)] else character(0)
    )
  }
  
  writeLines(lines, file)
  invisible(file)
}


# ==========================================================================
# handling helpfiles

rd2pdf("between-operators.Rd")


rd2pdf <- function(file,
                   output = file.path(
                     "C:/temp",
                     sub("\\.[Rr]d$", ".pdf", basename(file))
                   )) {
  
  project <- paste0(rstudioapi::getActiveProject(), "/man")
  
  if (is.null(project))
    stop("No active RStudio project.")
  
  file <- file.path(project, file)
  
  system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "Rd2pdf", "--force",
      paste0("--output=", shQuote(output)),
      shQuote(file)
    )
  )
}



## Link-Ziele aus R/ oder man/ einsammeln --------------------------------
linkTargets <- function(path) {
  files <- list.files(path, pattern = "[.]([Rr]|Rd)$", full.names = TRUE)
  txt   <- unlist(lapply(files, readLines, warn = FALSE))
  hits  <- unlist(regmatches(txt, gregexpr("\\\\link(\\[[^]]*\\])?\\{[^}]*\\}", txt)))
  
  opt  <- sub("^\\\\link(\\[([^]]*)\\])?\\{.*$", "\\2", hits)         # in [ ]
  body <- sub("^\\\\link(\\[[^]]*\\])?\\{([^}]*)\\}$", "\\2", hits)   # in { }
  
  ifelse(opt == "",            body,                        # \link{ziel}
         ifelse(startsWith(opt, "="), sub("^=", "", opt),          # \link[=ziel]{text}
                ifelse(grepl(":", opt),      opt,                         # \link[pkg:ziel]{text}
                       paste0(opt, ":", body))))    # \link[pkg]{ziel}
}


sort(table(linkTargets("R")))


alt <- linkTargets("man")

roxygen2md::roxygen2md("full")

neu <- linkTargets("man")
setdiff(alt, neu)   # verlorene Ziele
setdiff(neu, alt)   # neue Ziele

r <- list.files("R", pattern = "[.][Rr]$", full.names = TRUE)
txt <- lapply(r, readLines, warn = FALSE)
bad <- c("Pizza", "Cards", "Roulette", "Tarot", "courseData",
         "type-aliases", "long-wide-reshape", "between-operators",
         "range-operators", "setAttr-removeAttr-keepAttr", "GCD-LCM",
         "char-ascii-conversion", "numeric-conversions")
pat <- paste0("\\[(", paste(bad, collapse = "|"), ")\\(\\)\\]")
hits <- lapply(txt, grep, pattern = pat, value = TRUE)
data.frame(file = basename(r)[lengths(hits) > 0],
           line = unlist(hits[lengths(hits) > 0]))


