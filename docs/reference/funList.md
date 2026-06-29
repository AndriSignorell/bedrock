# List Objects, Functions Or Data in a Package

List all the objects, functions or data in a package.

## Usage

``` r
funList(package, exported = TRUE)
```

## Arguments

- package:

  the name of the package

- exported:

  logical (default `TRUE`) should only exported functions be listed?

## Details

This is just a wrapper for [`ls`](https://rdrr.io/r/base/ls.html),
[`ls.str`](https://rdrr.io/r/utils/ls_str.html) and
[`lsf.str`](https://rdrr.io/r/utils/ls_str.html) with the appropriate
arguments (as I always forgot how to do the trick). `objList()` lists
all objects, `funList()` just the functions in a package.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`ls`](https://rdrr.io/r/base/ls.html),
[`ls.str`](https://rdrr.io/r/utils/ls_str.html),
[`lsf.str`](https://rdrr.io/r/utils/ls_str.html)

Other pkg.introspection: [`funArgs()`](funArgs.md),
[`funCalls()`](funCalls.md), [`funKeywords()`](funKeywords.md),
[`rdLabels()`](rdLabels.md), [`mergeArgs()`](mergeArgs.md)

## Examples

``` r

funList("bedrock")
#>   [1] "stringsAsFactors"   "%[)%"               "dataDescription"   
#>   [4] "splitX"             "asciiToChar"        "combLevels"        
#>   [7] "nz"                 "openDataObject"     "isNA"              
#>  [10] "pairApply"          "crossProdN"         "setLength"         
#>  [13] "funList"            "naReplace"          "funCalls"          
#>  [16] "charToAscii"        "nDec"               "rankX"             
#>  [19] "strX"               "prec"               "completeColumns"   
#>  [22] "roundTo"            "GCD"                "label"             
#>  [25] "readDownload"       "mReplace"           "combPairs"         
#>  [28] "naIf"               "removeAttr"         "overlaps"          
#>  [31] "appendRowNames"     "%^%"                "logit"             
#>  [34] "sampleX"            "coalesceX"          "%[]%"              
#>  [37] "parseSASDatalines"  "label<-"            "moveAvg"           
#>  [40] "percentRank"        "isLowCardinality"   "compareDataFrames" 
#>  [43] "midx"               "allIdentical"       "recodeX"           
#>  [46] "permn"              "courseData"         "decToHex"          
#>  [49] "binToDec"           "%overlaps%"         "abind"             
#>  [52] "fibonacci"          "isDichotomous"      "isFilePath"        
#>  [55] "distance"           "flags"              "renameX"           
#>  [58] "revX"               "%)(%"               "asCDateFmt"        
#>  [61] "isZero"             "chr"                "ptInPoly"          
#>  [64] "toWide"             "linScale"           "binaryTree"        
#>  [67] "allDuplicated"      "keepAttr"           "collapseTable"     
#>  [70] "isWholeLike"        "isNumeric"          "divisors"          
#>  [73] "%)[%"               "fileExistURL"       "funKeywords"       
#>  [76] "unirootAll"         "callIf"             "unwhich"           
#>  [79] "digitSum"           "dotProd"            "decToOct"          
#>  [82] "overlap"            "closest"            "%()%"              
#>  [85] "bin"                "baseToBase"         "int"               
#>  [88] "multMerge"          "toBaseR"            "funArgs"           
#>  [91] "getDotsArg"         "maxDigits"          "isOdd"             
#>  [94] "decToBin"           "setNamesX"          "winsorize"         
#>  [97] "asBinary"           "strSplitToCol"      "isPrime"           
#> [100] "vRot"               "%](%"               "vShift"            
#> [103] "revCode"            "printCharMatrix"    "quot"              
#> [106] "splitAt"            "logitInv"           "columnWrap"        
#> [109] "buildPath"          "untable"            "LCM"               
#> [112] "nUnique"            "%(]%"               "nchr"              
#> [115] "extractArgs"        "resolveFormula"     "rdLabels"       
#> [118] "%][%"               "isURL"              "mGsub"             
#> [121] "mergeArgs"          "appendEnum"         "combSet"           
#> [124] "resolveGroups"      "findDownload"       "sortX"             
#> [127] "hexToDec"           "factorize"          "octToDec"          
#> [130] "resolveContingency" "toLong"             "randGroupSplit"    
#> [133] "combN"              "setAttr"            "%:%"               
#> [136] "nf"                 "locf"               "crossProd"         
#> [139] "isEuclid"           "trim"               "num"               
#> [142] "strSplitToDummy"    "pdfManual"          "appendX"           
#> [145] "primes"             "frac"               "countCompCases"    
#> [148] "recycle"            "dummy"              "splitPath"         
#> [151] "romanToInt"         "%::%"              
```
