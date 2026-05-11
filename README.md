
# 📦 bedrock

**Version:** 0.0.0.914\
**Title:** Base functions for the DescToolsX ecosystem\
**License:** GPL (≥ 2)

## 🧩 Overview

`bedrock` is a low-level utility package within the **DescToolsX
ecosystem**. It provides foundational, reusable functions that serve as
building blocks for higher-level statistical and analytical packages.

## ⚙️ Installation

``` r
remotes::install_github("AndriSignorell/bedrock")
```

## 📚 Core Features

### 🔹 Data Manipulation

-   appendX()
-   appendRowNames()
-   collapseTable()
-   completeColumns()

### 🔹 Vector & Utilities

-   Coalesce()
-   closest()
-   between operators (%\[\]%, etc.)

### 🔹 Mathematical Functions

-   dotProd()
-   crossProd()
-   crossProdN()
-   distance()

### 🔹 Combinatorics

-   combN()
-   combSet()
-   combPairs()

### 🔹 String Utilities

-   as.CDateFmt()
-   charToAscii() / asciiToChar()
-   columnWrap()

### 🔹 Programming Utilities

-   callIf()
-   extractArgs()
-   mergeArgs()

### 🔹 File Utilities

-   buildPath()
-   fileExistURL()
-   courseData()

## 🚀 Design Principles

-   Modular
-   Fast (Rcpp)
-   Robust
-   Generic

## 🧪 Example

``` r
library(bedrock)

x <- 1:10
x %[]% c(3, 6)

Coalesce(c(NA, NA, 5, 3))

combSet(letters[1:4], 2)

as.CDateFmt("yyyy-mm-dd")
```

## 📜 License

GPL (≥ 2)
