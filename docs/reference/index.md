# Package index

## Data Manipulation

Reshaping, recoding, ordering, and inspecting data structures.

- [`num()`](type-aliases.md) [`int()`](type-aliases.md)
  [`chr()`](type-aliases.md) [`nchr()`](type-aliases.md)
  [`bin()`](type-aliases.md) : Type Coercion Shortcuts

- [`toBaseR()`](toBaseR.md) : Back to Basics with Tibbles

- [`as.array(`*`<xtabs>`*`)`](as.array.xtabs.md)
  [`as.matrix(`*`<xtabs>`*`)`](as.array.xtabs.md) :

  Coerce `xtabs` Object to Array or Matrix

- [`asBinary()`](asBinary.md) : Coerce a Vector to Binary (0/1)

- [`combLevels()`](combLevels.md) : Combine Levels from Multiple Inputs

- [`dummy()`](dummy.md) : Generate Dummy Codes for a Factor

- [`mReplace()`](mReplace.md) : Replace Multiple Values in a Vector

- [`nf()`](nf.md) : Convert to Numeric via Factor

- [`recodeX()`](recodeX.md) : Recode a Variable

- [`revCode()`](revCode.md) : Reverse Coding of Variables

- [`stringsAsFactors()`](stringsAsFactors.md) : Convert Character
  Columns to Factors

- [`collapseTable()`](collapseTable.md) : Collapse Table Dimensions by
  Remapping Factor Levels

- [`toLong()`](long-wide-reshape.md) [`toWide()`](long-wide-reshape.md)
  : Reshape Between Long and Wide Format

- [`splitAt()`](splitAt.md) : Split a Vector at Given Positions

- [`splitX()`](splitX.md) : Split Data into Groups (Extended Interface)

- [`untable()`](untable.md) : Recover Original Data From Contingency
  Table

- [`binaryTree()`](binaryTree.md) : Binary Tree

- [`revX()`](revX.md) : Reverse Elements of a Vector, a Matrix, a Table,
  an Array or a Data.frame

- [`sortX()`](sortX.md) : Sort Vectors, Matrices, Tables, and Data
  Frames

- [`appendEnum()`](appendEnum.md) : Add an Enumeration Column

- [`appendRowNames()`](appendRowNames.md) : Append Rownames to a Data
  Frame or Matrix

- [`appendX()`](appendX.md) : Append Elements to Objects

- [`multMerge()`](multMerge.md) : Merge Multiple Data Frames

- [`` `%[]%` ``](between-operators.md)
  [`` `%[)%` ``](between-operators.md)
  [`` `%(]%` ``](between-operators.md)
  [`` `%()%` ``](between-operators.md)
  [`` `%][%` ``](between-operators.md)
  [`` `%](%` ``](between-operators.md)
  [`` `%)[%` ``](between-operators.md)
  [`` `%)(%` ``](between-operators.md) : Operators To Check, If a Value
  Lies Within Or Outside a Given Range

- [`` `%:%` ``](range-operators.md) [`` `%::%` ``](range-operators.md) :
  Select a Range Between Two Elements

- [`overlap()`](intervals.md) [`overlaps()`](intervals.md)
  [`distance()`](intervals.md) [`` `%overlaps%` ``](intervals.md) :
  Interval Arithmetic

- [`allDuplicated()`](allDuplicated.md) : Logical Indicator for All
  Values Involved in Ties

- [`allIdentical()`](allIdentical.md) : Test Whether Multiple Objects
  Are Identical

- [`compareDataFrames()`](compareDataFrames.md) : Compare Two Data
  Frames by Key Column

- [`flags()`](flags.md) : Extract Dichotomous (Binary) Variables

- [`isDichotomous()`](isDichotomous.md) : Check Whether a Vector Is
  Dichotomous

- [`isEuclid()`](isEuclid.md) : Test if a Distance Matrix Is Euclidean

- [`isLowCardinality()`](isLowCardinality.md) : Check for Low
  Cardinality

- [`isNumeric()`](isNumeric.md) : Check Whether an Object Is a Valid
  Numeric Vector

- [`isWholeLike()`](isWholeLike.md) : Test Whether Values Are (Nearly)
  Whole Numbers

- [`isZero()`](isZero.md) : Check a Vector For Being Zero

- [`nUnique()`](nUnique.md) : Count Unique Values

- [`completeColumns()`](completeColumns.md) : Identify Columns Without
  Missing Values

- [`countCompCases()`](countCompCases.md) : Count Complete Cases

- [`strX()`](strX.md) : Extended str() with numbered variables

- [`columnWrap()`](columnWrap.md) : Column Wrap

- [`printCharMatrix()`](printCharMatrix.md) : Pretty-print a character
  matrix with alignment, spacing and wrapping

- [`resolveContingency()`](resolveContingency.md) : Resolve Contingency
  Table

- [`resolveFormula()`](resolveFormula.md) : Parse and Classify a Model
  Formula

- [`resolveGroups()`](resolveGroups.md) : Resolve Grouped Data

## Vectors

Vector-level utilities: NA handling, moving windows, resizing.

- [`coalesceX()`](coalesceX.md) : Return the First Element Not Being NA
- [`isNA()`](isNA.md) : Test for a Scalar Missing Value
- [`locf()`](locf.md) : Last Observation Carried Forward
- [`naIf()`](naIf.md) : Replace Values with NA
- [`naReplace()`](naReplace.md) : Replace NA Values
- [`midx()`](midx.md) : Midpoints of a Numeric Vector
- [`moveAvg()`](moveAvg.md) : Moving Average
- [`quot()`](quot.md) : Lagged Quotients
- [`setLength()`](setLength.md) : Set the length of a vector, padding or
  truncating as needed
- [`trim()`](trim.md) : Trim a Vector
- [`vRot()`](vRot.md) : Rotate a vector
- [`vShift()`](vShift.md) : Shift a vector with NA padding
- [`closest()`](closest.md) : Find the Closest Value
- [`nz()`](nz.md) : Extract Non-Zero Values
- [`unwhich()`](unwhich.md) : Inverse Which

## Numbers & Mathematics

Number theory, base conversion, and numerical utilities.

- [`digitSum()`](digitSum.md) : Digit Sum for Integer Values
- [`divisors()`](divisors.md) : Calculate Divisors
- [`factorize()`](factorize.md) : Prime Factorization of Integers
- [`fibonacci()`](fibonacci.md) : Fibonacci Numbers
- [`GCD()`](GCD-LCM.md) [`LCM()`](GCD-LCM.md) : Greatest Common Divisor
  and Least Common Multiple
- [`isOdd()`](isOdd.md) : Test if Numbers Are Odd
- [`isPrime()`](isPrime.md) : Test Whether Numbers Are Prime
- [`primes()`](primes.md) : Generate Prime Numbers up to Given Limits
- [`hexToDec()`](numeric-conversions.md)
  [`decToHex()`](numeric-conversions.md)
  [`octToDec()`](numeric-conversions.md)
  [`decToOct()`](numeric-conversions.md)
  [`binToDec()`](numeric-conversions.md)
  [`decToBin()`](numeric-conversions.md)
  [`romanToInt()`](numeric-conversions.md)
  [`baseToBase()`](numeric-conversions.md) : Convert Numbers Between
  Bases
- [`crossProd()`](crossProd.md) : Cross Product of 3D Vectors or
  Matrices
- [`crossProdN()`](crossProdN.md) : Generalized Cross Product via
  Determinants
- [`dotProd()`](dotProd.md) : Dot Product of Vectors or Matrices
- [`roundTo()`](roundTo.md) : Round to Multiple
- [`unirootAll()`](unirootAll.md) : Find multiple roots of a function
  within an interval
- [`nDec()`](precision.md) [`prec()`](precision.md)
  [`frac()`](precision.md) [`maxDigits()`](precision.md) : Precision,
  Decimal Places and Fractional Part of a Numeric Value
- [`linScale()`](linScale.md) : Linearly Rescale Numeric Data
- [`logit()`](logit.md) [`logitInv()`](logit.md) : Logit Transformation
  and Its Inverse
- [`percentRank()`](percentRank.md) : Percent Rank of a Numeric Vector
- [`rankX()`](rankX.md) : Fast Ranking with Extended Tie Handling
- [`winsorize()`](winsorize.md) : Winsorize a Numeric Vector
- [`ptInPoly()`](ptInPoly.md) : Point-in-Polygon Test (Angle Summation,
  Rcpp)

## Strings

Character conversion and string transformation.

- [`charToAscii()`](char-ascii-conversion.md)
  [`asciiToChar()`](char-ascii-conversion.md) : Character \<-\> ASCII
  Conversion
- [`mGsub()`](mGsub.md) : Multiple String Substitution
- [`strSplitToCol()`](strSplitToCol.md) : Split Strings into Multiple
  Columns
- [`strSplitToDummy()`](strSplitToDummy.md) : Split a Character Vector
  into a Dummy Matrix

## Combinatorics

Combinations, permutations, and sampling.

- [`combN()`](combN.md) : Number of Combinations of a Set
- [`combPairs()`](combPairs.md) : Get All Pairs Out of One or Two Sets
  of Elements
- [`combSet()`](combSet.md) : Samples for Combinations of a Set
- [`pairApply()`](pairApply.md) : Pairwise Calculations
- [`permn()`](permn.md) : Set of Permutations
- [`randGroupSplit()`](randGroupSplit.md) : Randomly Split a Vector into
  Groups of Given Sizes
- [`sampleX()`](sampleX.md) : Random Samples and Permutations
- [`rSum21()`](rSum21.md) : Random Numbers Summing to 1

## Files & Paths

File I/O and path/URL handling.

- [`buildPath()`](buildPath.md) : Construct a Normalized File Path
- [`fileExistURL()`](fileExistURL.md) : Check if a File Exists at a URL
- [`findDownload()`](findDownload.md) : Locate a File in the Downloads
  Directory
- [`isFilePath()`](isFilePath.md) : Check Whether a String Is a File
  Path
- [`isURL()`](isURL.md) : Check Whether a String Is a URL
- [`readDownload()`](readDownload.md) : Read a File from the Downloads
  Directory
- [`splitPath()`](splitPath.md) : Split a File Path into Its Components
- [`parseSASDatalines()`](parseSASDatalines.md) : Parse SAS
  DATALINES/CARDS blocks into a data.frame
- [`pdfManual()`](pdfManual.md) : Open CRAN PDF Manual of a Package
- [`peekFile()`](peekFile.md) : Preview a Delimited Text File

## Labels & Attributes

Variable labels, object attributes, and metadata import.

- [`setAttr()`](setAttr-removeAttr-keepAttr.md)
  [`removeAttr()`](setAttr-removeAttr-keepAttr.md)
  [`keepAttr()`](setAttr-removeAttr-keepAttr.md) : Set and Remove Object
  Attributes
- [`label()`](label.md) [`` `label<-`() ``](label.md) : Get or Set
  Object and Variable Labels
- [`renameX()`](renameX.md) : Rename Elements of a Named Object
- [`setNamesX()`](setNamesX.md) : Set the Names in an Object
- [`dataDescription()`](dataDescription.md) : Extract Data Description
  from Excel File
- [`openDataObject()`](openDataObject.md) : Load Excel Data with
  Metadata (Codes and Labels)

## Dates

Date/time formatting.

- [`asCDateFmt()`](asCDateFmt.md) : Convert Custom Date Format to
  strftime Format

## Package Introspection

Inspecting functions, arguments, and Rd documentation.

- [`funArgs()`](funArgs.md) : List All Arguments of a Function
- [`funCalls()`](funCalls.md) : List Calls Used in Function
- [`funKeywords()`](funKeywords.md) : List Keywords For R Manual Pages
- [`funList()`](funList.md) : List Functions in a Package
- [`rdLabels()`](rdLabels.md) : Extract variable labels from Rd
  documentation
- [`rdTitle()`](rdTitle.md) : Extract the Title from an Rd Help File
- [`callIf()`](callIf.md) : Conditionally Call a Function
- [`extractArgs()`](extractArgs.md) : Extract Named Arguments from Dots
  with Defaults
- [`getDotsArg()`](getDotsArg.md) : Get a Single Argument from Dots with
  Default
- [`mergeArgs()`](mergeArgs.md) : Merge Default Arguments with User
  Overrides
- [`recycle()`](recycle.md) : Recycle a List of Elements

## Datasets

Example datasets shipped with bedrock.

- [`Cards`](Cards.md) : Playing Cards dataset
- [`Pizza`](Pizza.md) : Pizza delivery data (extended)
- [`Roulette`](Roulette.md) : Roulette wheel dataset
- [`Tarot`](Tarot.md) : Tarot Cards dataset
- [`courseData()`](courseData.md) : Load Course Dataset from Server
