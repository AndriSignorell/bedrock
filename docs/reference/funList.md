# List Functions in a Package

List all the functions in a package.

## Usage

``` r
funList(package, exported = TRUE)
```

## Arguments

- package:

  the name of the package

- exported:

  logical (default `TRUE`) should only exported functions be listed?

## Value

A sorted character vector with the function names.

## Details

This is just a wrapper for the namespace inspection functions (as I
always forgot how to do the trick). By default only the exported
functions are returned; with `exported = FALSE` all functions defined in
the package namespace are listed, including internal ones.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`ls`](https://rdrr.io/r/base/ls.html),
[`ls.str`](https://rdrr.io/r/utils/ls_str.html),
[`lsf.str`](https://rdrr.io/r/utils/ls_str.html),
[`getNamespaceExports`](https://rdrr.io/r/base/ns-reflect.html)

Other pkg.funinfo: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md)

## Examples

``` r

funList("bedrock")
#>   [1] "%()%"               "%(]%"               "%)(%"              
#>   [4] "%)[%"               "%:%"                "%::%"              
#>   [7] "%[)%"               "%[]%"               "%](%"              
#>  [10] "%][%"               "%^%"                "%overlaps%"        
#>  [13] "GCD"                "LCM"                "abind"             
#>  [16] "allDuplicated"      "allIdentical"       "appendEnum"        
#>  [19] "appendRowNames"     "appendX"            "asBinary"          
#>  [22] "asCDateFmt"         "asciiToChar"        "baseToBase"        
#>  [25] "bin"                "binToDec"           "binaryTree"        
#>  [28] "buildPath"          "callIf"             "charToAscii"       
#>  [31] "chr"                "closest"            "coalesceX"         
#>  [34] "collapseTable"      "columnWrap"         "combLevels"        
#>  [37] "combN"              "combPairs"          "combSet"           
#>  [40] "compareDataFrames"  "completeColumns"    "countCompCases"    
#>  [43] "courseData"         "crossProd"          "crossProdN"        
#>  [46] "dataDescription"    "decToBin"           "decToHex"          
#>  [49] "decToOct"           "digitSum"           "distance"          
#>  [52] "divisors"           "dotProd"            "dummy"             
#>  [55] "extractArgs"        "factorize"          "fibonacci"         
#>  [58] "fileExistURL"       "findDownload"       "flags"             
#>  [61] "frac"               "funArgs"            "funCalls"          
#>  [64] "funKeywords"        "funList"            "getDotsArg"        
#>  [67] "hexToDec"           "int"                "isDichotomous"     
#>  [70] "isEuclid"           "isFilePath"         "isLowCardinality"  
#>  [73] "isNA"               "isNumeric"          "isOdd"             
#>  [76] "isPrime"            "isURL"              "isWholeLike"       
#>  [79] "isZero"             "keepAttr"           "label"             
#>  [82] "label<-"            "linScale"           "locf"              
#>  [85] "logit"              "logitInv"           "mGsub"             
#>  [88] "mReplace"           "maxDigits"          "mergeArgs"         
#>  [91] "midx"               "moveAvg"            "multMerge"         
#>  [94] "nDec"               "nUnique"            "naIf"              
#>  [97] "naReplace"          "nchr"               "nf"                
#> [100] "num"                "nz"                 "octToDec"          
#> [103] "openDataObject"     "overlap"            "overlaps"          
#> [106] "pairApply"          "parseSASDatalines"  "pdfManual"         
#> [109] "peekFile"           "percentRank"        "permn"             
#> [112] "prec"               "primes"             "printCharMatrix"   
#> [115] "ptInPoly"           "quot"               "rSum21"            
#> [118] "randGroupSplit"     "rankX"              "rdLabels"          
#> [121] "rdTitle"            "readDownload"       "recodeX"           
#> [124] "recycle"            "removeAttr"         "renameX"           
#> [127] "resolveContingency" "resolveFormula"     "resolveGroups"     
#> [130] "revCode"            "revX"               "romanToInt"        
#> [133] "roundTo"            "sampleX"            "setAttr"           
#> [136] "setLength"          "setNamesX"          "sortX"             
#> [139] "splitAt"            "splitPath"          "splitX"            
#> [142] "strSplitToCol"      "strSplitToDummy"    "strX"              
#> [145] "stringsAsFactors"   "toBaseR"            "toLong"            
#> [148] "toWide"             "trim"               "unirootAll"        
#> [151] "untable"            "unwhich"            "vRot"              
#> [154] "vShift"             "winsorize"         
```
