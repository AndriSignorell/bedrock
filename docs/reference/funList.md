# List Functions in a Package

List all the functions in a package.

## Usage

``` r
funList(package, exported = TRUE)
```

## Arguments

- package:

  the name of the package.

- exported:

  logical; whether only exported functions are listed. Defaults to
  `TRUE`.

## Value

a sorted character vector with the function names.

## Details

This is just a wrapper for the namespace inspection functions (as I
always forgot how to do the trick). By default only the exported
functions are returned; with `exported = FALSE` all functions defined in
the package namespace are listed, including internal ones.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`ls()`](https://rdrr.io/r/base/ls.html),
[`ls.str()`](https://rdrr.io/r/utils/ls_str.html),
[`lsf.str()`](https://rdrr.io/r/utils/ls_str.html),
[`getNamespaceExports()`](https://rdrr.io/r/base/ns-reflect.html)

Other pkg.funinfo: [`auditNames()`](auditNames.md),
[`funArgs()`](funArgs.md), [`funCalls()`](funCalls.md),
[`funKeywords()`](funKeywords.md), [`rdLabels()`](rdLabels.md),
[`rdTitle()`](rdTitle.md)

## Examples

``` r

funList("bedrock")
#>   [1] "%()%"                   "%(]%"                   "%)(%"                  
#>   [4] "%)[%"                   "%:%"                    "%::%"                  
#>   [7] "%[)%"                   "%[]%"                   "%](%"                  
#>  [10] "%][%"                   "%^%"                    "%overlaps%"            
#>  [13] "GCD"                    "LCM"                    "abind"                 
#>  [16] "allDuplicated"          "allIdentical"           "appendEnum"            
#>  [19] "appendRowNames"         "appendX"                "applySides"            
#>  [22] "asBinary"               "asCDateFmt"             "asciiToChar"           
#>  [25] "auditNames"             "baseToBase"             "bin"                   
#>  [28] "binToDec"               "binaryTree"             "buildPath"             
#>  [31] "callIf"                 "charToAscii"            "checkConfLevel"        
#>  [34] "checkCount"             "checkFlag"              "checkString"           
#>  [37] "chr"                    "closest"                "coalesceX"             
#>  [40] "collapseTable"          "columnWrap"             "combLevels"            
#>  [43] "combN"                  "combPairs"              "combSet"               
#>  [46] "compareDataFrames"      "completeColumns"        "conceptAudit"          
#>  [49] "conceptMap"             "countCompCases"         "crossProd"             
#>  [52] "crossProdN"             "dataDescription"        "decToBin"              
#>  [55] "decToHex"               "decToOct"               "digitSum"              
#>  [58] "distance"               "divisors"               "dotProd"               
#>  [61] "dummy"                  "extractArgs"            "factorize"             
#>  [64] "fibonacci"              "findDownload"           "flags"                 
#>  [67] "frac"                   "funArgs"                "funCalls"              
#>  [70] "funKeywords"            "funList"                "getConcepts"           
#>  [73] "getDotsArg"             "hexToDec"               "int"                   
#>  [76] "isDichotomous"          "isEuclid"               "isFilePath"            
#>  [79] "isLowCardinality"       "isNA"                   "isNumeric"             
#>  [82] "isOdd"                  "isPrime"                "isUrl"                 
#>  [85] "isWholeLike"            "isZero"                 "keepAttr"              
#>  [88] "label"                  "label<-"                "linScale"              
#>  [91] "locf"                   "logit"                  "logitInv"              
#>  [94] "mGsub"                  "mReplace"               "maxDec"                
#>  [97] "mergeArgs"              "midx"                   "moveAvg"               
#> [100] "multMerge"              "nDec"                   "nUnique"               
#> [103] "naIf"                   "naReplace"              "nchr"                  
#> [106] "nf"                     "num"                    "nz"                    
#> [109] "octToDec"               "openDataObject"         "overlapSize"           
#> [112] "overlaps"               "pairApply"              "parseSasDatalines"     
#> [115] "pdfManual"              "peekFile"               "percentRank"           
#> [118] "permn"                  "prec"                   "primes"                
#> [121] "printCharMatrix"        "ptInPoly"               "quot"                  
#> [124] "rBetaShape"             "rSum21"                 "randGroupSplit"        
#> [127] "rankX"                  "rdLabels"               "rdTitle"               
#> [130] "readCourseData"         "readDownload"           "recodeX"               
#> [133] "recycle"                "removeAttr"             "renameX"               
#> [136] "resolveContingency"     "resolveFormula"         "resolveFormulaFromCall"
#> [139] "resolveGroups"          "revCode"                "revX"                  
#> [142] "romanToInt"             "roundTo"                "sampleX"               
#> [145] "setAttr"                "setLength"              "setNamesX"             
#> [148] "sortX"                  "splitAt"                "splitPath"             
#> [151] "splitX"                 "strSplitToCol"          "strSplitToDummy"       
#> [154] "strX"                   "stringsAsFactors"       "toBaseR"               
#> [157] "toLong"                 "toWide"                 "trim"                  
#> [160] "unirootAll"             "untable"                "unwhich"               
#> [163] "urlExists"              "vRot"                   "vShift"                
#> [166] "winsorize"              "withSeed"              
```
