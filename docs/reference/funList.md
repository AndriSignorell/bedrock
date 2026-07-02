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

Other pkg.introspection: [`extractArgs()`](extractArgs.md),
[`funArgs()`](funArgs.md), [`funCalls()`](funCalls.md),
[`funKeywords()`](funKeywords.md), [`getDotsArg()`](getDotsArg.md),
[`mergeArgs()`](mergeArgs.md), [`quot()`](quot.md),
[`rdLabels()`](rdLabels.md), [`rdTitle()`](rdTitle.md),
[`strX()`](strX.md)

## Examples

``` r

funList("bedrock")
#>   [1] "stringsAsFactors"   "%[)%"               "dataDescription"   
#>   [4] "splitX"             "asciiToChar"        "combLevels"        
#>   [7] "nz"                 "openDataObject"     "isNA"              
#>  [10] "rdTitle"            "pairApply"          "crossProdN"        
#>  [13] "setLength"          "funList"            "naReplace"         
#>  [16] "funCalls"           "charToAscii"        "nDec"              
#>  [19] "strX"               "rankX"              "prec"              
#>  [22] "completeColumns"    "roundTo"            "GCD"               
#>  [25] "label"              "readDownload"       "mReplace"          
#>  [28] "combPairs"          "naIf"               "removeAttr"        
#>  [31] "overlaps"           "appendRowNames"     "%^%"               
#>  [34] "logit"              "sampleX"            "coalesceX"         
#>  [37] "%[]%"               "parseSASDatalines"  "label<-"           
#>  [40] "moveAvg"            "percentRank"        "isLowCardinality"  
#>  [43] "compareDataFrames"  "midx"               "allIdentical"      
#>  [46] "recodeX"            "permn"              "courseData"        
#>  [49] "decToHex"           "binToDec"           "%overlaps%"        
#>  [52] "abind"              "fibonacci"          "isDichotomous"     
#>  [55] "isFilePath"         "distance"           "flags"             
#>  [58] "renameX"            "revX"               "%)(%"              
#>  [61] "asCDateFmt"         "isZero"             "chr"               
#>  [64] "ptInPoly"           "toWide"             "linScale"          
#>  [67] "binaryTree"         "allDuplicated"      "keepAttr"          
#>  [70] "collapseTable"      "isWholeLike"        "isNumeric"         
#>  [73] "divisors"           "%)[%"               "fileExistURL"      
#>  [76] "funKeywords"        "unirootAll"         "callIf"            
#>  [79] "unwhich"            "digitSum"           "dotProd"           
#>  [82] "decToOct"           "rdLabels"           "overlap"           
#>  [85] "closest"            "%()%"               "bin"               
#>  [88] "baseToBase"         "int"                "multMerge"         
#>  [91] "toBaseR"            "funArgs"            "getDotsArg"        
#>  [94] "maxDigits"          "isOdd"              "decToBin"          
#>  [97] "setNamesX"          "winsorize"          "asBinary"          
#> [100] "strSplitToCol"      "isPrime"            "vRot"              
#> [103] "%](%"               "vShift"             "revCode"           
#> [106] "printCharMatrix"    "quot"               "splitAt"           
#> [109] "logitInv"           "columnWrap"         "buildPath"         
#> [112] "untable"            "LCM"                "nUnique"           
#> [115] "%(]%"               "nchr"               "extractArgs"       
#> [118] "resolveFormula"     "%][%"               "isURL"             
#> [121] "mGsub"              "mergeArgs"          "appendEnum"        
#> [124] "combSet"            "resolveGroups"      "findDownload"      
#> [127] "sortX"              "hexToDec"           "factorize"         
#> [130] "octToDec"           "resolveContingency" "toLong"            
#> [133] "randGroupSplit"     "combN"              "setAttr"           
#> [136] "%:%"                "nf"                 "locf"              
#> [139] "crossProd"          "isEuclid"           "trim"              
#> [142] "strSplitToDummy"    "num"                "pdfManual"         
#> [145] "appendX"            "primes"             "frac"              
#> [148] "countCompCases"     "recycle"            "dummy"             
#> [151] "splitPath"          "romanToInt"         "%::%"              
```
