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
#>  [49] "countCompCases"         "courseData"             "crossProd"             
#>  [52] "crossProdN"             "dataDescription"        "decToBin"              
#>  [55] "decToHex"               "decToOct"               "digitSum"              
#>  [58] "distance"               "divisors"               "dotProd"               
#>  [61] "dummy"                  "extractArgs"            "factorize"             
#>  [64] "fibonacci"              "fileExistURL"           "findDownload"          
#>  [67] "flags"                  "frac"                   "funArgs"               
#>  [70] "funCalls"               "funKeywords"            "funList"               
#>  [73] "getConcepts"            "getDotsArg"             "hexToDec"              
#>  [76] "int"                    "isDichotomous"          "isEuclid"              
#>  [79] "isFilePath"             "isLowCardinality"       "isNA"                  
#>  [82] "isNumeric"              "isOdd"                  "isPrime"               
#>  [85] "isURL"                  "isWholeLike"            "isZero"                
#>  [88] "keepAttr"               "label"                  "label<-"               
#>  [91] "linScale"               "locf"                   "logit"                 
#>  [94] "logitInv"               "mGsub"                  "mReplace"              
#>  [97] "maxDec"                 "mergeArgs"              "midx"                  
#> [100] "moveAvg"                "multMerge"              "nDec"                  
#> [103] "nUnique"                "naIf"                   "naReplace"             
#> [106] "nchr"                   "nf"                     "num"                   
#> [109] "nz"                     "octToDec"               "openDataObject"        
#> [112] "overlap"                "overlaps"               "pairApply"             
#> [115] "parseSASDatalines"      "pdfManual"              "peekFile"              
#> [118] "percentRank"            "permn"                  "prec"                  
#> [121] "primes"                 "printCharMatrix"        "ptInPoly"              
#> [124] "quot"                   "rBetaShape"             "rSum21"                
#> [127] "randGroupSplit"         "rankX"                  "rdLabels"              
#> [130] "rdTitle"                "readDownload"           "recodeX"               
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
