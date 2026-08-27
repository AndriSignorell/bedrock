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
#>  [19] "appendRowNames"     "appendX"            "applySides"        
#>  [22] "asBinary"           "asCDateFmt"         "asciiToChar"       
#>  [25] "baseToBase"         "bin"                "binToDec"          
#>  [28] "binaryTree"         "buildPath"          "callIf"            
#>  [31] "charToAscii"        "checkConfLevel"     "checkFlag"         
#>  [34] "chr"                "closest"            "coalesceX"         
#>  [37] "collapseTable"      "columnWrap"         "combLevels"        
#>  [40] "combN"              "combPairs"          "combSet"           
#>  [43] "compareDataFrames"  "completeColumns"    "countCompCases"    
#>  [46] "courseData"         "crossProd"          "crossProdN"        
#>  [49] "dataDescription"    "decToBin"           "decToHex"          
#>  [52] "decToOct"           "digitSum"           "distance"          
#>  [55] "divisors"           "dotProd"            "dummy"             
#>  [58] "extractArgs"        "factorize"          "fibonacci"         
#>  [61] "fileExistURL"       "findDownload"       "flags"             
#>  [64] "frac"               "funArgs"            "funCalls"          
#>  [67] "funKeywords"        "funList"            "getDotsArg"        
#>  [70] "hexToDec"           "int"                "isDichotomous"     
#>  [73] "isEuclid"           "isFilePath"         "isLowCardinality"  
#>  [76] "isNA"               "isNumeric"          "isOdd"             
#>  [79] "isPrime"            "isURL"              "isWholeLike"       
#>  [82] "isZero"             "keepAttr"           "label"             
#>  [85] "label<-"            "linScale"           "locf"              
#>  [88] "logit"              "logitInv"           "mGsub"             
#>  [91] "mReplace"           "maxDigits"          "mergeArgs"         
#>  [94] "midx"               "moveAvg"            "multMerge"         
#>  [97] "nDec"               "nUnique"            "naIf"              
#> [100] "naReplace"          "nchr"               "nf"                
#> [103] "num"                "nz"                 "octToDec"          
#> [106] "openDataObject"     "overlap"            "overlaps"          
#> [109] "pairApply"          "parseSASDatalines"  "pdfManual"         
#> [112] "peekFile"           "percentRank"        "permn"             
#> [115] "prec"               "primes"             "printCharMatrix"   
#> [118] "ptInPoly"           "quot"               "rBetaShape"        
#> [121] "rSum21"             "randGroupSplit"     "rankX"             
#> [124] "rdLabels"           "rdTitle"            "readDownload"      
#> [127] "recodeX"            "recycle"            "removeAttr"        
#> [130] "renameX"            "resolveContingency" "resolveFormula"    
#> [133] "resolveGroups"      "revCode"            "revX"              
#> [136] "romanToInt"         "roundTo"            "sampleX"           
#> [139] "setAttr"            "setLength"          "setNamesX"         
#> [142] "sortX"              "splitAt"            "splitPath"         
#> [145] "splitX"             "strSplitToCol"      "strSplitToDummy"   
#> [148] "strX"               "stringsAsFactors"   "toBaseR"           
#> [151] "toLong"             "toWide"             "trim"              
#> [154] "unirootAll"         "untable"            "unwhich"           
#> [157] "vRot"               "vShift"             "winsorize"         
#> [160] "withSeed"          
```
