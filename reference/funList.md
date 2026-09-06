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
#>  [31] "charToAscii"        "checkConfLevel"     "checkCount"        
#>  [34] "checkFlag"          "checkString"        "chr"               
#>  [37] "closest"            "coalesceX"          "collapseTable"     
#>  [40] "columnWrap"         "combLevels"         "combN"             
#>  [43] "combPairs"          "combSet"            "compareDataFrames" 
#>  [46] "completeColumns"    "countCompCases"     "courseData"        
#>  [49] "crossProd"          "crossProdN"         "dataDescription"   
#>  [52] "decToBin"           "decToHex"           "decToOct"          
#>  [55] "digitSum"           "distance"           "divisors"          
#>  [58] "dotProd"            "dummy"              "extractArgs"       
#>  [61] "factorize"          "fibonacci"          "fileExistURL"      
#>  [64] "findDownload"       "flags"              "frac"              
#>  [67] "funArgs"            "funCalls"           "funKeywords"       
#>  [70] "funList"            "getDotsArg"         "hexToDec"          
#>  [73] "int"                "isDichotomous"      "isEuclid"          
#>  [76] "isFilePath"         "isLowCardinality"   "isNA"              
#>  [79] "isNumeric"          "isOdd"              "isPrime"           
#>  [82] "isURL"              "isWholeLike"        "isZero"            
#>  [85] "keepAttr"           "label"              "label<-"           
#>  [88] "linScale"           "locf"               "logit"             
#>  [91] "logitInv"           "mGsub"              "mReplace"          
#>  [94] "maxDec"             "mergeArgs"          "midx"              
#>  [97] "moveAvg"            "multMerge"          "nDec"              
#> [100] "nUnique"            "naIf"               "naReplace"         
#> [103] "nchr"               "nf"                 "num"               
#> [106] "nz"                 "octToDec"           "openDataObject"    
#> [109] "overlap"            "overlaps"           "pairApply"         
#> [112] "parseSASDatalines"  "pdfManual"          "peekFile"          
#> [115] "percentRank"        "permn"              "prec"              
#> [118] "primes"             "printCharMatrix"    "ptInPoly"          
#> [121] "quot"               "rBetaShape"         "rSum21"            
#> [124] "randGroupSplit"     "rankX"              "rdLabels"          
#> [127] "rdTitle"            "readDownload"       "recodeX"           
#> [130] "recycle"            "removeAttr"         "renameX"           
#> [133] "resolveContingency" "resolveFormula"     "resolveGroups"     
#> [136] "revCode"            "revX"               "romanToInt"        
#> [139] "roundTo"            "sampleX"            "setAttr"           
#> [142] "setLength"          "setNamesX"          "sortX"             
#> [145] "splitAt"            "splitPath"          "splitX"            
#> [148] "strSplitToCol"      "strSplitToDummy"    "strX"              
#> [151] "stringsAsFactors"   "toBaseR"            "toLong"            
#> [154] "toWide"             "trim"               "unirootAll"        
#> [157] "untable"            "unwhich"            "vRot"              
#> [160] "vShift"             "winsorize"          "withSeed"          
```
