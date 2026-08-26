# 📦 bedrock

**Title:** Base Functions for the DescToolsX Ecosystem  
**License:** GPL (≥ 2)

## 🧩 Overview

`bedrock` is the foundation layer of the **DescToolsX ecosystem**. It
provides low-level, generic utilities and basic routines — data
manipulation, inspection, vector operations, string handling, math and
combinatorics — that serve as building blocks for the higher-level
statistical, graphics, and modelling packages of the suite.

The package is self-contained. It has no dependency on any other package
of the suite and is equally useful on its own.

It follows the DescToolsX design rules: a consistent lowerCamelCase API,
generic functions with S3 methods, predictable argument names and
ordering, and performance-critical routines implemented in Rcpp.

📖 **Documentation:** <https://andrisignorell.github.io/bedrock/>

## ⚙️ Installation

Install the released version from CRAN:

``` r

install.packages("bedrock")
```

Or the development version from GitHub:

``` r

remotes::install_github("AndriSignorell/bedrock")
```

## 📚 Core Features

### 🔹 Data Manipulation

Appending, recoding, renaming, sorting, and reshaping of vectors,
factors, matrices, and data frames.

- [`appendX()`](https://andrisignorell.github.io/bedrock/reference/appendX.md),
  [`appendRowNames()`](https://andrisignorell.github.io/bedrock/reference/appendRowNames.md),
  [`appendEnum()`](https://andrisignorell.github.io/bedrock/reference/appendEnum.md)
- [`recodeX()`](https://andrisignorell.github.io/bedrock/reference/recodeX.md),
  [`revCode()`](https://andrisignorell.github.io/bedrock/reference/revCode.md),
  [`combLevels()`](https://andrisignorell.github.io/bedrock/reference/combLevels.md),
  [`dummy()`](https://andrisignorell.github.io/bedrock/reference/dummy.md)
- [`sortX()`](https://andrisignorell.github.io/bedrock/reference/sortX.md),
  [`revX()`](https://andrisignorell.github.io/bedrock/reference/revX.md),
  [`renameX()`](https://andrisignorell.github.io/bedrock/reference/renameX.md),
  [`setNamesX()`](https://andrisignorell.github.io/bedrock/reference/setNamesX.md)
- [`splitX()`](https://andrisignorell.github.io/bedrock/reference/splitX.md),
  [`splitAt()`](https://andrisignorell.github.io/bedrock/reference/splitAt.md),
  [`toLong()`](https://andrisignorell.github.io/bedrock/reference/long-wide-reshape.md)
  /
  [`toWide()`](https://andrisignorell.github.io/bedrock/reference/long-wide-reshape.md)
- [`recycle()`](https://andrisignorell.github.io/bedrock/reference/recycle.md),
  [`columnWrap()`](https://andrisignorell.github.io/bedrock/reference/columnWrap.md),
  [`compareDataFrames()`](https://andrisignorell.github.io/bedrock/reference/compareDataFrames.md)
- [`toBaseR()`](https://andrisignorell.github.io/bedrock/reference/toBaseR.md),
  [`stringsAsFactors()`](https://andrisignorell.github.io/bedrock/reference/stringsAsFactors.md)
- Type coercion shortcuts:
  [`num()`](https://andrisignorell.github.io/bedrock/reference/type-aliases.md),
  [`int()`](https://andrisignorell.github.io/bedrock/reference/type-aliases.md),
  [`chr()`](https://andrisignorell.github.io/bedrock/reference/type-aliases.md),
  [`nchr()`](https://andrisignorell.github.io/bedrock/reference/type-aliases.md),
  [`bin()`](https://andrisignorell.github.io/bedrock/reference/type-aliases.md)

### 🔹 Data Inspection & Validation

Predicates and checks for data quality and structure.

- [`isNumeric()`](https://andrisignorell.github.io/bedrock/reference/isNumeric.md),
  [`isDichotomous()`](https://andrisignorell.github.io/bedrock/reference/isDichotomous.md),
  [`isLowCardinality()`](https://andrisignorell.github.io/bedrock/reference/isLowCardinality.md),
  [`isWholeLike()`](https://andrisignorell.github.io/bedrock/reference/isWholeLike.md),
  [`isZero()`](https://andrisignorell.github.io/bedrock/reference/isZero.md),
  [`isNA()`](https://andrisignorell.github.io/bedrock/reference/isNA.md)
- [`allDuplicated()`](https://andrisignorell.github.io/bedrock/reference/allDuplicated.md),
  [`allIdentical()`](https://andrisignorell.github.io/bedrock/reference/allIdentical.md),
  [`completeColumns()`](https://andrisignorell.github.io/bedrock/reference/completeColumns.md),
  [`countCompCases()`](https://andrisignorell.github.io/bedrock/reference/countCompCases.md),
  [`flags()`](https://andrisignorell.github.io/bedrock/reference/flags.md)
- Between operators: `%[]%`, `%()%`, `%[)%`, `%(]%`
- [`isFilePath()`](https://andrisignorell.github.io/bedrock/reference/isFilePath.md),
  [`isURL()`](https://andrisignorell.github.io/bedrock/reference/isURL.md),
  [`isEuclid()`](https://andrisignorell.github.io/bedrock/reference/isEuclid.md)

### 🔹 Vector Operations

- [`coalesceX()`](https://andrisignorell.github.io/bedrock/reference/coalesceX.md),
  [`closest()`](https://andrisignorell.github.io/bedrock/reference/closest.md),
  [`locf()`](https://andrisignorell.github.io/bedrock/reference/locf.md)
- [`naIf()`](https://andrisignorell.github.io/bedrock/reference/naIf.md),
  [`naReplace()`](https://andrisignorell.github.io/bedrock/reference/naReplace.md),
  [`nz()`](https://andrisignorell.github.io/bedrock/reference/nz.md)
- [`trim()`](https://andrisignorell.github.io/bedrock/reference/trim.md),
  [`winsorize()`](https://andrisignorell.github.io/bedrock/reference/winsorize.md),
  [`setLength()`](https://andrisignorell.github.io/bedrock/reference/setLength.md)
- [`moveAvg()`](https://andrisignorell.github.io/bedrock/reference/moveAvg.md),
  [`midx()`](https://andrisignorell.github.io/bedrock/reference/midx.md),
  [`vRot()`](https://andrisignorell.github.io/bedrock/reference/vRot.md),
  [`vShift()`](https://andrisignorell.github.io/bedrock/reference/vShift.md),
  [`pairApply()`](https://andrisignorell.github.io/bedrock/reference/pairApply.md)

### 🔹 String Utilities

- [`mGsub()`](https://andrisignorell.github.io/bedrock/reference/mGsub.md),
  [`mReplace()`](https://andrisignorell.github.io/bedrock/reference/mReplace.md)
- [`strSplitToCol()`](https://andrisignorell.github.io/bedrock/reference/strSplitToCol.md),
  [`strSplitToDummy()`](https://andrisignorell.github.io/bedrock/reference/strSplitToDummy.md)
- [`charToAscii()`](https://andrisignorell.github.io/bedrock/reference/char-ascii-conversion.md)
  /
  [`asciiToChar()`](https://andrisignorell.github.io/bedrock/reference/char-ascii-conversion.md),
  [`asCDateFmt()`](https://andrisignorell.github.io/bedrock/reference/asCDateFmt.md)

### 🔹 Mathematical Functions

- [`roundTo()`](https://andrisignorell.github.io/bedrock/reference/roundTo.md),
  [`linScale()`](https://andrisignorell.github.io/bedrock/reference/linScale.md),
  [`logit()`](https://andrisignorell.github.io/bedrock/reference/logit.md)
- [`nDec()`](https://andrisignorell.github.io/bedrock/reference/precision.md),
  [`prec()`](https://andrisignorell.github.io/bedrock/reference/precision.md),
  [`frac()`](https://andrisignorell.github.io/bedrock/reference/precision.md),
  [`maxDigits()`](https://andrisignorell.github.io/bedrock/reference/precision.md)
- [`rankX()`](https://andrisignorell.github.io/bedrock/reference/rankX.md),
  [`percentRank()`](https://andrisignorell.github.io/bedrock/reference/percentRank.md),
  [`nUnique()`](https://andrisignorell.github.io/bedrock/reference/nunique.md)
- [`dotProd()`](https://andrisignorell.github.io/bedrock/reference/dotProd.md),
  [`crossProd()`](https://andrisignorell.github.io/bedrock/reference/crossProd.md),
  [`crossProdN()`](https://andrisignorell.github.io/bedrock/reference/crossProdN.md)
- [`unirootAll()`](https://andrisignorell.github.io/bedrock/reference/unirootAll.md),
  [`untable()`](https://andrisignorell.github.io/bedrock/reference/untable.md)

### 🔹 Number Theory & Combinatorics

- [`primes()`](https://andrisignorell.github.io/bedrock/reference/primes.md),
  [`isPrime()`](https://andrisignorell.github.io/bedrock/reference/isPrime.md),
  [`factorize()`](https://andrisignorell.github.io/bedrock/reference/factorize.md),
  [`divisors()`](https://andrisignorell.github.io/bedrock/reference/divisors.md)
- [`GCD()`](https://andrisignorell.github.io/bedrock/reference/GCD-LCM.md)
  /
  [`LCM()`](https://andrisignorell.github.io/bedrock/reference/GCD-LCM.md),
  [`fibonacci()`](https://andrisignorell.github.io/bedrock/reference/fibonacci.md),
  [`digitSum()`](https://andrisignorell.github.io/bedrock/reference/digitSum.md),
  [`isOdd()`](https://andrisignorell.github.io/bedrock/reference/isOdd.md)
- Base conversions:
  [`decToBin()`](https://andrisignorell.github.io/bedrock/reference/numeric-conversions.md),
  [`decToHex()`](https://andrisignorell.github.io/bedrock/reference/numeric-conversions.md),
  [`decToOct()`](https://andrisignorell.github.io/bedrock/reference/numeric-conversions.md),
  [`baseToBase()`](https://andrisignorell.github.io/bedrock/reference/numeric-conversions.md),
  [`romanToInt()`](https://andrisignorell.github.io/bedrock/reference/numeric-conversions.md)
- [`combN()`](https://andrisignorell.github.io/bedrock/reference/combN.md),
  [`combSet()`](https://andrisignorell.github.io/bedrock/reference/combSet.md),
  [`combPairs()`](https://andrisignorell.github.io/bedrock/reference/combPairs.md),
  [`permn()`](https://andrisignorell.github.io/bedrock/reference/permn.md)
- [`sampleX()`](https://andrisignorell.github.io/bedrock/reference/sampleX.md),
  [`randGroupSplit()`](https://andrisignorell.github.io/bedrock/reference/randGroupSplit.md),
  [`unwhich()`](https://andrisignorell.github.io/bedrock/reference/unwhich.md)

### 🔹 Tables & Merging

- [`collapseTable()`](https://andrisignorell.github.io/bedrock/reference/collapseTable.md),
  [`multMerge()`](https://andrisignorell.github.io/bedrock/reference/multMerge.md),
  [`printCharMatrix()`](https://andrisignorell.github.io/bedrock/reference/printCharMatrix.md)

### 🔹 Labels & Metadata

- [`label()`](https://andrisignorell.github.io/bedrock/reference/Label.md)
  — get or set variable labels
- [`setAttr()`](https://andrisignorell.github.io/bedrock/reference/setAttr-removeAttr-keepAttr.md),
  [`removeAttr()`](https://andrisignorell.github.io/bedrock/reference/setAttr-removeAttr-keepAttr.md),
  [`keepAttr()`](https://andrisignorell.github.io/bedrock/reference/setAttr-removeAttr-keepAttr.md)
- [`openDataObject()`](https://andrisignorell.github.io/bedrock/reference/openDataObject.md),
  [`dataDescription()`](https://andrisignorell.github.io/bedrock/reference/dataDescription.md)
  — Excel data with codes and labels

### 🔹 File Utilities

- [`buildPath()`](https://andrisignorell.github.io/bedrock/reference/buildPath.md),
  [`splitPath()`](https://andrisignorell.github.io/bedrock/reference/splitPath.md)
- [`findDownload()`](https://andrisignorell.github.io/bedrock/reference/findDownload.md),
  [`readDownload()`](https://andrisignorell.github.io/bedrock/reference/readDownload.md),
  [`peekFile()`](https://andrisignorell.github.io/bedrock/reference/peekFile.md)
- [`fileExistURL()`](https://andrisignorell.github.io/bedrock/reference/fileExistURL.md),
  [`pdfManual()`](https://andrisignorell.github.io/bedrock/reference/pdfManual.md)
- [`parseSASDatalines()`](https://andrisignorell.github.io/bedrock/reference/parseSASDatalines.md)
  — parse SAS DATALINES blocks into a data.frame

### 🔹 Programming & Introspection

- [`callIf()`](https://andrisignorell.github.io/bedrock/reference/callIf.md),
  [`mergeArgs()`](https://andrisignorell.github.io/bedrock/reference/mergeArgs.md),
  [`extractArgs()`](https://andrisignorell.github.io/bedrock/reference/extractArgs.md),
  [`getDotsArg()`](https://andrisignorell.github.io/bedrock/reference/getDotsArg.md),
  [`quot()`](https://andrisignorell.github.io/bedrock/reference/quot.md)
- [`resolveFormula()`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.md),
  [`resolveGroups()`](https://andrisignorell.github.io/bedrock/reference/resolveGroups.md),
  [`resolveContingency()`](https://andrisignorell.github.io/bedrock/reference/resolveContingency.md)
- [`funArgs()`](https://andrisignorell.github.io/bedrock/reference/funArgs.md),
  [`funCalls()`](https://andrisignorell.github.io/bedrock/reference/funCalls.md),
  [`funList()`](https://andrisignorell.github.io/bedrock/reference/funList.md),
  [`funKeywords()`](https://andrisignorell.github.io/bedrock/reference/funKeywords.md)
- [`rdTitle()`](https://andrisignorell.github.io/bedrock/reference/rdTitle.md),
  [`rdLabels()`](https://andrisignorell.github.io/bedrock/reference/rdLabels.md),
  [`strX()`](https://andrisignorell.github.io/bedrock/reference/strX.md)

### 🔹 Datasets

Teaching and example datasets: `Cards`, `Pizza`, `Roulette`, `Tarot`,
plus
[`courseData()`](https://andrisignorell.github.io/bedrock/reference/courseData.md)
for loading course material.

## 🚀 Design Principles

- **Consistent** — lowerCamelCase API and uniform argument conventions
  across the whole DescToolsX suite
- **Fast** — performance-critical routines implemented in Rcpp
- **Generic** — S3 generics with methods for vectors, factors, matrices,
  tables, and data frames
- **Robust** — validated inputs, informative errors, extensive testthat
  coverage

## 🧪 Example

``` r

library(bedrock)

# range operators
x <- 1:10
x %[]% c(3, 6)

# first non-missing value
coalesceX(c(NA, NA, 5, 3))

# round to arbitrary multiples
roundTo(c(1.23, 4.56), 0.25)

# all 2-element subsets
combSet(letters[1:4], 2)

# parse a SAS data step
sas <- "
  data mydata;
    input name $ age score;
  datalines;
  Alice 30 95.5
  Bob   25 88.0
  ;
"
parseSASDatalines(sas)
```

## 🙏 Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.

## 📜 License

GPL (≥ 2)
