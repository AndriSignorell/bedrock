# Recode a Variable

Combining or rearranging a factor can be tedious if it has many levels.
`recodeX()` supports this step by accepting a direct definition of new
levels by enumerating old levelnames as argument and adding an
`"elseLevel"` option. If new levels are given as integer values they
will be translated in the according levels.

## Usage

``` r
recodeX(
  x,
  ...,
  keep = NULL,
  elseLevel = NA,
  ref = NULL,
  useEmpty = FALSE,
  num = FALSE
)
```

## Arguments

- x:

  the factor whose levels are to be altered. If x is `character` it will
  be factorized (using `factor` defaults) but returned as `character`
  again.

- ...:

  the old levels (combined by `c`() if there are several) named with the
  new level:  
  `newlevel_a = c("old_a", "old_b"), `  
  `newlevel_b = c("old_c", "old_d")`  
  See examples.

- keep:

  vector of levels that should be left untouched.

- elseLevel:

  the value for levels, which are not matched by newlevel list. If this
  is set to `NULL`, the elseLevels will be left unchanged. If set to
  `NA` (default) non matched levels will be set to `NA`.

- ref:

  the reference level, typically a string.

- useEmpty:

  logical. Defines how a new level, which can't be found in x, should be
  handled. Should it be left in the level's list or be dropped? The
  default is `FALSE`, which drops empty levels.

- num:

  logical. If set to `TRUE` the result will be numeric. This is useful
  if you want to recode strings to specific numeric values.

## Value

the factor having the new levels applied.  
if `x` was a `character` vector, the result will also be `character`

## See also

[`factor`](https://rdrr.io/r/base/factor.html),
[`levels`](https://rdrr.io/r/base/levels.html),
[`relevel`](https://rdrr.io/r/stats/relevel.html),
[`reorder`](https://rdrr.io/r/stats/reorder.factor.html)  
There's another solution for this problem in the package **car**.

Other data.manipulation: [`appendEnum()`](appendEnum.md),
[`appendRowNames()`](appendRowNames.md), [`appendX()`](appendX.md),
[`as.array.xtabs()`](as.array.xtabs.md),
[`columnWrap()`](columnWrap.md), [`combLevels()`](combLevels.md),
[`nf()`](nf.md), [`parseSASDatalines()`](parseSASDatalines.md),
[`recycle()`](recycle.md), [`revCode()`](revCode.md),
[`revX()`](revX.md),
[`setAttr-removeAttr-keepAttr`](setAttr-removeAttr-keepAttr.md),
[`setNamesX()`](setNamesX.md), [`sortX()`](sortX.md),
[`splitX()`](splitX.md), [`stringsAsFactors()`](stringsAsFactors.md),
[`toBaseR()`](toBaseR.md), [`untable()`](untable.md)

## Examples

``` r

set.seed(1984)
x <- factor(sample(1:15, 20, replace=TRUE))
levels(x) <- paste("old", levels(x), sep="_")

y <- recodeX(x,
            "new_1"   = c("old_1","old_4","old_5"),
            "new_2"   = c("old_6","old_10","old_11"),
            "new_3"   = c("old_12","old_13"),
            elseLevel = "other")
data.frame(x=x, y=y)
#>         x     y
#> 1   old_8 other
#> 2  old_14 other
#> 3   old_4 new_1
#> 4  old_10 new_2
#> 5   old_1 new_1
#> 6   old_5 new_1
#> 7  old_14 other
#> 8   old_3 other
#> 9  old_15 other
#> 10  old_8 other
#> 11  old_1 new_1
#> 12 old_13 new_3
#> 13  old_4 new_1
#> 14  old_8 other
#> 15  old_1 new_1
#> 16  old_4 new_1
#> 17  old_6 new_2
#> 18 old_15 other
#> 19 old_13 new_3
#> 20 old_13 new_3

# Coding NAs, NA is recoded to new_1
x[5:6] <- NA
x <- x[1:7] 

data.frame(
  x, 
  RecodeNA = recodeX(x,
                    "new_1"   = c("old_4","old_8", NA),
                    elseLevel = "other"),
       
  # NAs remain unaffected, unless specified to be processed      
  NoRecodeNA = recodeX(x,
                      "new_1"   = c("old_4","old_8"),
                      elseLevel = "other")
)         
#>        x RecodeNA NoRecodeNA
#> 1  old_8    new_1      new_1
#> 2 old_14    other      other
#> 3  old_4    new_1      new_1
#> 4 old_10    other      other
#> 5   <NA>    new_1       <NA>
#> 6   <NA>    new_1       <NA>
#> 7 old_14    other      other

# keep some levels, collapse others and reset the reference level
ff <- factor(c("apple","pear","banana","kiwi",
               "mango","peach","grape","plum"))
recodeX(ff, 
       stone=c("peach", "plum"), 
       keep=c("apple","banana"),
       elseLevel = "other", ref="stone")
#> [1] apple  other  banana other  other  stone  other  stone 
#> Levels: stone apple banana other


x <- factor(letters[1:6])

z1 <- recodeX(x, AB=c("a","b"), CD=c("c","d"), elseLevel="none of these")
z2 <- recodeX(x, AB=c("a","b"), CD=c("c","d"), elseLevel=NA)
z3 <- recodeX(x, AB=c("a","b"), CD=c("c","d"), elseLevel=NULL)
z4 <- recodeX(x, AB=c("a","b"), GH=c("g","h"), elseLevel=NA, useEmpty=TRUE)
z5 <- recodeX(x, AB=c("a","b"), GH=c("g","h"), elseLevel=NA, useEmpty=FALSE)

data.frame(z1, z2, z3, z4, z5)
#>              z1   z2 z3   z4   z5
#> 1            AB   AB AB   AB   AB
#> 2            AB   AB AB   AB   AB
#> 3            CD   CD CD <NA> <NA>
#> 4            CD   CD CD <NA> <NA>
#> 5 none of these <NA>  e <NA> <NA>
#> 6 none of these <NA>  f <NA> <NA>

lapply(data.frame(z1, z2, z3, z4, z5), levels)
#> $z1
#> [1] "AB"            "CD"            "none of these"
#> 
#> $z2
#> [1] "AB" "CD"
#> 
#> $z3
#> [1] "AB" "CD" "e"  "f" 
#> 
#> $z4
#> [1] "AB" "GH"
#> 
#> $z5
#> [1] "AB"
#> 

# empty level GH exists in z4...
table(z4, useNA="ifany")
#> z4
#>   AB   GH <NA> 
#>    2    0    4 
# and is dropped in z5
table(z5, useNA="ifany")
#> z5
#>   AB <NA> 
#>    2    4 

# use integers to define the groups to collapse
set.seed(1972)
(likert <- factor(sample(1:10, size=15, replace=TRUE),
                  levels=1:10, labels=gettextf("(%s)", 1:10)))
#>  [1] (4)  (10) (9)  (1)  (3)  (7)  (10) (7)  (8)  (10) (1)  (6)  (9)  (10) (5) 
#> Levels: (1) (2) (3) (4) (5) (6) (7) (8) (9) (10)
recodeX(likert, det=1:6, pas=7:8, pro=9:10)
#>  [1] det pro pro det det pas pro pas pas pro det det pro pro det
#> Levels: det pas pro

# or directly turned to numeric
recodeX(likert, "1"=1:6, "2"=7:8, "5"=9:10, num=TRUE)
#>  [1] 1 5 5 1 1 2 5 2 2 5 1 1 5 5 1
```
