# Concept Utilities for Package Documentation

Helper functions to inspect and analyse the use of `\\concept` tags
within a package.

## Usage

``` r
getConcepts(pkg, prefix = NULL)

conceptMap(pkg)

conceptAudit(pkg)
```

## Arguments

- pkg:

  character string. Name of the installed package.

- prefix:

  optional character string. If given, `getConcepts()` returns only the
  concepts starting with it, with the prefix removed.

## Value

- `getConcepts`:

  character vector of unique concept names, sorted

- `conceptMap`:

  named list mapping concepts to topics

- `conceptAudit`:

  data frame with the columns `concept` and `nTopics`, ordered by
  decreasing frequency

## Details

These utilities extract concept metadata from Rd files and allow
structured auditing of conceptual organisation inside a package.

**Functions**

- `getConcepts()` - Returns all unique concepts used in a package.

- `conceptMap()` - Returns a mapping of concepts to functions.

- `conceptAudit()` - Returns a summary table of concept usage.

The functions use
[`tools::Rd_db()`](https://rdrr.io/r/tools/Rdutils.html) to parse Rd
files and extract `\\concept` tags programmatically.

These tools are intended for package development, documentation
consistency checks, and conceptual audits.

## Examples

``` r
getConcepts("stats")
#>  [1] "#1DFBETAsDFBETAs"                 "#1DFFITsDFFITs"                  
#>  [3] "#1MDSMDS"                         "#1PRESSPRESS"                    
#>  [5] "#1erfcerfc"                       "#1erfcinverfcinv"                
#>  [7] "#1erferf"                         "#1erfinverfinv"                  
#>  [9] "#1twowaytwoway"                   "ARMA"                            
#> [11] "Cook's distances"                 "Covariance ratios"               
#> [13] "Kendall correlation coefficient"  "Kendall's tau"                   
#> [15] "Mann-Whitney Test"                "PCA"                             
#> [17] "Pearson correlation coefficient"  "Spearman correlation coefficient"
#> [19] "Spearman's rho"                   "autoregression"                  
#> [21] "bandwidth"                        "dissimilarity"                   
#> [23] "error function"                   "goodness-of-fit"                 
#> [25] "incomplete beta function"         "incomplete gamma function"       
#> [27] "log-linear"                       "logistic"                        
#> [29] "logit"                            "loglinear"                       
#> [31] "margins"                          "matrix visualization"            
#> [33] "maximization"                     "minimization"                    
#> [35] "monotonic regression"             "normal probability plot"         
#> [37] "optimization"                     "ordination"                      
#> [39] "portmanteau"                      "regression"                      
#> [41] "sigmoid"                          "standardized residuals"          
#> [43] "studentized residuals"            "totals"                          
getConcepts("stats", prefix = "erf")
#> character(0)
head(conceptMap("stats"))
#> $`#1DFBETAsDFBETAs`
#> [1] "influence.measures"
#> 
#> $`#1DFFITsDFFITs`
#> [1] "influence.measures"
#> 
#> $`#1MDSMDS`
#> [1] "cmdscale"
#> 
#> $`#1PRESSPRESS`
#> [1] "influence.measures"
#> 
#> $`#1erfcerfc`
#> [1] "Normal"
#> 
#> $`#1erfcinverfcinv`
#> [1] "Normal"
#> 
head(conceptAudit("stats"))
#>             concept nTopics
#> 40       regression       5
#> 10             ARMA       2
#> 16              PCA       2
#> 20   autoregression       2
#> 1  #1DFBETAsDFBETAs       1
#> 2    #1DFFITsDFFITs       1
```
