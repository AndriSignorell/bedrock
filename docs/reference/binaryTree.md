# Binary Tree

Create a binary tree of a given number of nodes `n`. Can be used to
organize a sorted numeric vector as a binary tree.

## Usage

``` r
binaryTree(n)
```

## Arguments

- n:

  integer, size of the tree

## Value

an integer vector of length n

## Details

If we index the nodes of the tree as 1 for the top, 2–3 for the next
horizontal row, 4–7 for the next, ... then the parent-child traversal
becomes particularly easy. The basic idea is that the rows of the tree
start at indices 1, 2, 4, ....

`binaryTree(13)` yields the vector
`c(8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 7)` meaning that the smallest
element will be in position 8 of the tree, the next smallest in position
4, etc.

## Note

Substantially based on code by Terry Therneau, with major extensions and
improvements by the package author.

## See also

[`plotBinaryTree`](https://rdrr.io/pkg/aurora/man/binaryTree.html)

Other data.order: [`revX()`](revX.md), [`sortX()`](sortX.md)

## Examples

``` r

binaryTree(12)
#>  [1]  8  4  9  2 10  5 11  1 12  6  3  7
```
