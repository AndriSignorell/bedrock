# 📦 bedrock

**Version:** 0.0.0.938  
**Title:** Base functions for the DescToolsX ecosystem  
**License:** GPL (≥ 2)

## 🧩 Overview

`bedrock` is the foundation layer of the **DescToolsX ecosystem**. It
provides low-level, generic utilities and basic routines — data
manipulation, inspection, vector operations, string handling, math and
combinatorics — that serve as building blocks for the higher-level
statistical, graphics, and modelling packages of the suite.

The package follows the DescToolsX design rules: a consistent
lowerCamelCase API, generic functions with S3 methods, predictable
argument names and ordering, and performance-critical routines
implemented in Rcpp.

📖 **Documentation:** <https://andrisignorell.github.io/bedrock/>

## ⚙️ Installation

``` r

remotes::install_github("AndriSignorell/bedrock")
```

## 📚 Core Features

### 🔹 Data Manipulation

Appending, recoding, renaming, sorting, and reshaping of vectors,
factors, matrices, and data frames.

- [`appendX()`](reference/appendX.md),
  [`appendRowNames()`](reference/appendRowNames.md),
  [`appendEnum()`](reference/appendEnum.md)
- [`recodeX()`](reference/recodeX.md),
  [`revCode()`](reference/revCode.md),
  [`combLevels()`](reference/combLevels.md),
  [`dummy()`](reference/dummy.md)
- [`sortX()`](reference/sortX.md), [`revX()`](reference/revX.md),
  [`renameX()`](reference/renameX.md),
  [`setNamesX()`](reference/setNamesX.md)
- [`splitX()`](reference/splitX.md),
  [`splitAt()`](reference/splitAt.md),
  [`toLong()`](reference/long-wide-reshape.md) /
  [`toWide()`](reference/long-wide-reshape.md)
- [`recycle()`](reference/recycle.md),
  [`columnWrap()`](reference/columnWrap.md),
  [`compareDataFrames()`](reference/compareDataFrames.md)
- [`toBaseR()`](reference/toBaseR.md),
  [`stringsAsFactors()`](reference/stringsAsFactors.md)
- Type coercion shortcuts: [`num()`](reference/type-aliases.md),
  [`int()`](reference/type-aliases.md),
  [`chr()`](reference/type-aliases.md),
  [`nchr()`](reference/type-aliases.md),
  [`bin()`](reference/type-aliases.md)

### 🔹 Data Inspection & Validation

Predicates and checks for data quality and structure.

- [`isNumeric()`](reference/isNumeric.md),
  [`isDichotomous()`](reference/isDichotomous.md),
  [`isLowCardinality()`](reference/isLowCardinality.md),
  [`isWholeLike()`](reference/isWholeLike.md),
  [`isZero()`](reference/isZero.md), [`isNA()`](reference/isNA.md)
- [`allDuplicated()`](reference/allDuplicated.md),
  [`allIdentical()`](reference/allIdentical.md),
  [`completeColumns()`](reference/completeColumns.md),
  [`countCompCases()`](reference/countCompCases.md),
  [`flags()`](reference/flags.md)
- Between operators: `%[]%`, `%()%`, `%[)%`, `%(]%`
- [`isFilePath()`](reference/isFilePath.md),
  [`isURL()`](reference/isURL.md), [`isEuclid()`](reference/isEuclid.md)

### 🔹 Vector Operations

- [`coalesceX()`](reference/coalesceX.md),
  [`closest()`](reference/closest.md), [`locf()`](reference/locf.md)
- [`naIf()`](reference/naIf.md),
  [`naReplace()`](reference/naReplace.md), [`nz()`](reference/nz.md)
- [`trim()`](reference/trim.md),
  [`winsorize()`](reference/winsorize.md),
  [`setLength()`](reference/setLength.md)
- [`moveAvg()`](reference/moveAvg.md), [`midx()`](reference/midx.md),
  [`vRot()`](reference/vRot.md), [`vShift()`](reference/vShift.md),
  [`pairApply()`](reference/pairApply.md)

### 🔹 String Utilities

- [`mGsub()`](reference/mGsub.md), [`mReplace()`](reference/mReplace.md)
- [`strSplitToCol()`](reference/strSplitToCol.md),
  [`strSplitToDummy()`](reference/strSplitToDummy.md)
- [`charToAscii()`](reference/char-ascii-conversion.md) /
  [`asciiToChar()`](reference/char-ascii-conversion.md),
  [`asCDateFmt()`](reference/asCDateFmt.md)

### 🔹 Mathematical Functions

- [`roundTo()`](reference/roundTo.md),
  [`linScale()`](reference/linScale.md),
  [`logit()`](reference/logit.md),
  [`precision()`](reference/precision.md)
- [`rankX()`](reference/rankX.md),
  [`percentRank()`](reference/percentRank.md),
  [`nUnique()`](reference/nUnique.md)
- [`dotProd()`](reference/dotProd.md),
  [`crossProd()`](reference/crossProd.md),
  [`crossProdN()`](reference/crossProdN.md)
- [`unirootAll()`](reference/unirootAll.md),
  [`untable()`](reference/untable.md)

### 🔹 Number Theory & Combinatorics

- [`primes()`](reference/primes.md),
  [`isPrime()`](reference/isPrime.md),
  [`factorize()`](reference/factorize.md),
  [`divisors()`](reference/divisors.md)
- `gcd()` / [`lcm()`](https://rdrr.io/r/graphics/layout.html),
  [`fibonacci()`](reference/fibonacci.md),
  [`digitSum()`](reference/digitSum.md), [`isOdd()`](reference/isOdd.md)
- Base conversions: `numericConversions`
- [`combN()`](reference/combN.md), [`combSet()`](reference/combSet.md),
  [`combPairs()`](reference/combPairs.md),
  [`permn()`](reference/permn.md)
- [`sampleX()`](reference/sampleX.md),
  [`randGroupSplit()`](reference/randGroupSplit.md),
  [`unwhich()`](reference/unwhich.md)

### 🔹 Tables & Merging

- [`collapseTable()`](reference/collapseTable.md),
  [`multMerge()`](reference/multMerge.md),
  [`printCharMatrix()`](reference/printCharMatrix.md)

### 🔹 Labels & Metadata

- [`label()`](reference/label.md) — get or set variable labels
- [`setAttr()`](reference/setAttr-removeAttr-keepAttr.md),
  [`removeAttr()`](reference/setAttr-removeAttr-keepAttr.md),
  [`keepAttr()`](reference/setAttr-removeAttr-keepAttr.md)
- [`openDataObject()`](reference/openDataObject.md),
  [`dataDescription()`](reference/dataDescription.md) — Excel data with
  codes and labels

### 🔹 File Utilities

- [`buildPath()`](reference/buildPath.md),
  [`splitPath()`](reference/splitPath.md)
- [`findDownload()`](reference/findDownload.md),
  [`readDownload()`](reference/readDownload.md),
  [`peekFile()`](reference/peekFile.md)
- [`fileExistURL()`](reference/fileExistURL.md),
  [`pdfManual()`](reference/pdfManual.md)
- [`parseSASDatalines()`](reference/parseSASDatalines.md) — parse SAS
  DATALINES blocks into a data.frame

### 🔹 Programming & Introspection

- [`callIf()`](reference/callIf.md),
  [`mergeArgs()`](reference/mergeArgs.md),
  [`extractArgs()`](reference/extractArgs.md),
  [`getDotsArg()`](reference/getDotsArg.md),
  [`quot()`](reference/quot.md)
- [`resolveFormula()`](reference/resolveFormula.md),
  [`resolveGroups()`](reference/resolveGroups.md),
  [`resolveContingency()`](reference/resolveContingency.md)
- [`funArgs()`](reference/funArgs.md),
  [`funCalls()`](reference/funCalls.md),
  [`funList()`](reference/funList.md),
  [`funKeywords()`](reference/funKeywords.md)
- [`rdTitle()`](reference/rdTitle.md),
  [`rdLabels()`](reference/rdLabels.md), [`strX()`](reference/strX.md)

### 🔹 Datasets

Teaching and example datasets: `Cards`, `Pizza`, `Roulette`, `Tarot`,
plus [`courseData()`](reference/courseData.md) for loading course
material.

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

## 📜 License

GPL (≥ 2)
