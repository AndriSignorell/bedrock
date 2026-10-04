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

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md)

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
#>  [25] "baseToBase"             "bin"                    "binToDec"              
#>  [28] "binaryTree"             "buildPath"              "callIf"                
#>  [31] "charToAscii"            "checkConfLevel"         "checkCount"            
#>  [34] "checkFlag"              "checkString"            "chr"                   
#>  [37] "closest"                "coalesceX"              "collapseTable"         
#>  [40] "columnWrap"             "combLevels"             "combN"                 
#>  [43] "combPairs"              "combSet"                "compareDataFrames"     
#>  [46] "completeColumns"        "conceptAudit"           "conceptMap"            
#>  [49] "countCompCases"         "crossProd"              "crossProdN"            
#>  [52] "dataDescription"        "decToBin"               "decToHex"              
#>  [55] "decToOct"               "digitSum"               "distance"              
#>  [58] "divisors"               "dotProd"                "dummy"                 
#>  [61] "extractArgs"            "factorize"              "fibonacci"             
#>  [64] "fileExistURL"           "findDownload"           "flags"                 
#>  [67] "frac"                   "funArgs"                "funCalls"              
#>  [70] "funKeywords"            "funList"                "getConcepts"           
#>  [73] "getDotsArg"             "hexToDec"               "int"                   
#>  [76] "isDichotomous"          "isEuclid"               "isFilePath"            
#>  [79] "isLowCardinality"       "isNA"                   "isNumeric"             
#>  [82] "isOdd"                  "isPrime"                "isURL"                 
#>  [85] "isWholeLike"            "isZero"                 "keepAttr"              
#>  [88] "label"                  "label<-"                "linScale"              
#>  [91] "locf"                   "logit"                  "logitInv"              
#>  [94] "mGsub"                  "mReplace"               "maxDec"                
#>  [97] "mergeArgs"              "midx"                   "moveAvg"               
#> [100] "multMerge"              "nDec"                   "nUnique"               
#> [103] "naIf"                   "naReplace"              "nchr"                  
#> [106] "nf"                     "num"                    "nz"                    
#> [109] "octToDec"               "openDataObject"         "overlap"               
#> [112] "overlaps"               "pairApply"              "parseSASDatalines"     
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
#> [163] "vRot"                   "vShift"                 "winsorize"             
#> [166] "withSeed"              
```
