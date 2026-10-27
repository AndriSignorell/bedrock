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
#>  [31] "callIf"                 "caseWhen"               "charToAscii"           
#>  [34] "checkConfLevel"         "checkCount"             "checkFlag"             
#>  [37] "checkString"            "chr"                    "closest"               
#>  [40] "coalesceX"              "collapseTable"          "columnWrap"            
#>  [43] "combLevels"             "combN"                  "combPairs"             
#>  [46] "combSet"                "compareDataFrames"      "completeColumns"       
#>  [49] "conceptAudit"           "conceptMap"             "countCompCases"        
#>  [52] "crossProd"              "crossProdN"             "dataDescription"       
#>  [55] "decToBin"               "decToHex"               "decToOct"              
#>  [58] "digitSum"               "distance"               "divisors"              
#>  [61] "dotProd"                "dummy"                  "extractArgs"           
#>  [64] "factorize"              "fibonacci"              "findDownload"          
#>  [67] "flags"                  "frac"                   "funArgs"               
#>  [70] "funCalls"               "funKeywords"            "funList"               
#>  [73] "getConcepts"            "getDotsArg"             "hexToDec"              
#>  [76] "int"                    "isDichotomous"          "isEuclid"              
#>  [79] "isFilePath"             "isLowCardinality"       "isNA"                  
#>  [82] "isNumeric"              "isOdd"                  "isPrime"               
#>  [85] "isUrl"                  "isWholeLike"            "isZero"                
#>  [88] "keepAttr"               "label"                  "label<-"               
#>  [91] "linScale"               "locf"                   "logit"                 
#>  [94] "logitInv"               "mGsub"                  "mReplace"              
#>  [97] "maxDec"                 "mergeArgs"              "midx"                  
#> [100] "moveAvg"                "multMerge"              "nDec"                  
#> [103] "nUnique"                "naIf"                   "naReplace"             
#> [106] "nchr"                   "nf"                     "num"                   
#> [109] "nz"                     "octToDec"               "openDataObject"        
#> [112] "overlapSize"            "overlaps"               "pairApply"             
#> [115] "parseSasDatalines"      "pdfManual"              "peekFile"              
#> [118] "percentRank"            "permn"                  "prec"                  
#> [121] "primes"                 "printCharMatrix"        "ptInPoly"              
#> [124] "quot"                   "rBetaShape"             "rSum21"                
#> [127] "randGroupSplit"         "rankX"                  "rdLabels"              
#> [130] "rdTitle"                "readCourseData"         "readDownload"          
#> [133] "recodeX"                "recycle"                "removeAttr"            
#> [136] "renameX"                "resolveContingency"     "resolveFormula"        
#> [139] "resolveFormulaFromCall" "resolveGroups"          "revCode"               
#> [142] "revX"                   "romanToInt"             "roundTo"               
#> [145] "sampleX"                "setAttr"                "setLength"             
#> [148] "setNamesX"              "sortX"                  "splitAt"               
#> [151] "splitPath"              "splitX"                 "strSplitToCol"         
#> [154] "strSplitToDummy"        "strX"                   "stringsAsFactors"      
#> [157] "toBaseR"                "toLong"                 "toWide"                
#> [160] "trim"                   "unirootAll"             "untable"               
#> [163] "unwhich"                "urlExists"              "vRot"                  
#> [166] "vShift"                 "winsorize"              "withSeed"              
```
