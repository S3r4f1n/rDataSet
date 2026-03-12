
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rDataSet

<!-- badges: start -->

<!-- badges: end -->

The goal of rDataSet is to treat data frames as mathematical sets. It
provides set-theoretic operations (union, intersection, difference,
equality) for data frames where values are identified by row IDs and
column names, and presence/absence is determined by `NA` values.

## Installation

You can install the development version of rDataSet from local source
like so:

``` r
devtools::install("/path/to/rDataSet")
# or with
install.packages("/path/to/rDataSet", repo = NULL, type = "source")
```

## Overview

rDataSet introduces a `dataset` S3 class that extends data frames with
designated ID columns. Set operations work on a cell-by-cell basis:

- **ID columns** uniquely identify each row (one-to-one relation)
- **Value presence** is determined by `is.na()` - a value is present if
  not `NA`
- **Set operations** compare cells at the same (row_id, col_id) position

## Set Operations

Each set operation works in two stages:

1.  **Row/Column Alignment**: Determines which rows and columns appear
    in the result
2.  **Value Operation**: Decides which values to keep for each cell

| Operation | Rows in Result | Columns in Result | Value Operation |
|----|----|----|----|
| Difference (`A - B`) | All rows from A | All value columns from A | Keep values from A where B has `NA` |
| Intersection (`A > B`) | Only rows present in **both** A and B (inner join on IDs) | Only common value columns | Keep values from A where B has non-`NA` |
| Union (`A + B`) | All rows from both A and B (full join on IDs) | All value columns from both | Use A’s value; fall back to B where A has `NA` |
| Equality (`A == B`) | Must match exactly (same rows) | Must match exactly (same columns) | `TRUE` if equal, `FALSE` if different, `NA` if both missing |

**Note**: ID columns must match between datasets for all operations.
Value columns that exist in only one dataset are handled according to
the join type (dropped, kept, or error).

## Example

``` r
library(rDataSet)
library(tibble)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Create datasets with 'i' as the ID column
A <- dataset_build(
  tibble(i = 1:10, b = if_else(1:10 %% 2 == 0, NA, 1:10)),
  ids = "i"
)

B <- dataset_build(
  tibble(i = 1:10, b = na_if(1:10, 3)),
  ids = "i"
)

# Dataset C has fewer rows (1:5) and an extra column 'c'
C <- dataset_build(
  tibble(i = 1:5, b = 10:6, c = 2),
  ids = "i"
)

# View the datasets
A
#> # A tibble: 10 × 2
#>        i     b
#>    <int> <int>
#>  1     1     1
#>  2     2    NA
#>  3     3     3
#>  4     4    NA
#>  5     5     5
#>  6     6    NA
#>  7     7     7
#>  8     8    NA
#>  9     9     9
#> 10    10    NA
B
#> # A tibble: 10 × 2
#>        i     b
#>    <int> <int>
#>  1     1     1
#>  2     2     2
#>  3     3    NA
#>  4     4     4
#>  5     5     5
#>  6     6     6
#>  7     7     7
#>  8     8     8
#>  9     9     9
#> 10    10    10
C
#> # A tibble: 5 × 3
#>       i     b     c
#>   <int> <int> <dbl>
#> 1     1    10     2
#> 2     2     9     2
#> 3     3     8     2
#> 4     4     7     2
#> 5     5     6     2
```

### Set Difference

Keep values from A where B has `NA` (uses left join, so all rows from
A):

``` r
A - B
#> # A tibble: 10 × 2
#>        i     b
#>    <int> <int>
#>  1     1    NA
#>  2     2    NA
#>  3     3     3
#>  4     4    NA
#>  5     5    NA
#>  6     6    NA
#>  7     7    NA
#>  8     8    NA
#>  9     9    NA
#> 10    10    NA
```

Difference with C shows how non-common columns are dropped:

``` r
A - C  # Only column 'b' remains (common), row ids 1:5 from A
#> # A tibble: 10 × 2
#>        i     b
#>    <int> <int>
#>  1     1    NA
#>  2     2    NA
#>  3     3    NA
#>  4     4    NA
#>  5     5    NA
#>  6     6    NA
#>  7     7     7
#>  8     8    NA
#>  9     9     9
#> 10    10    NA
```

### Set Intersection

Keep values from A where B has non-`NA` values (inner join on rows,
common columns only):

``` r
A > B
#> # A tibble: 10 × 2
#>        i     b
#>    <int> <int>
#>  1     1     1
#>  2     2    NA
#>  3     3    NA
#>  4     4    NA
#>  5     5     5
#>  6     6    NA
#>  7     7     7
#>  8     8    NA
#>  9     9     9
#> 10    10    NA
```

Intersection with C - note only rows 1:5 and column ‘b’ are kept:

``` r
A > C
#> # A tibble: 5 × 2
#>       i     b
#>   <int> <int>
#> 1     1     1
#> 2     2    NA
#> 3     3     3
#> 4     4    NA
#> 5     5     5
```

### Set Union

Combine values from both datasets (full join on rows, all columns):

``` r
A + B
#> # A tibble: 10 × 2
#>        i     b
#>    <int> <int>
#>  1     1     1
#>  2     2     2
#>  3     3     3
#>  4     4     4
#>  5     5     5
#>  6     6     6
#>  7     7     7
#>  8     8     8
#>  9     9     9
#> 10    10    10
```

Union with C - includes all rows (1:10) and all columns (b, c):

``` r
A + C
#> # A tibble: 10 × 3
#>        i     b     c
#>    <int> <int> <dbl>
#>  1     1     1     2
#>  2     2     9     2
#>  3     3     3     2
#>  4     4     7     2
#>  5     5     5     2
#>  6     6    NA    NA
#>  7     7     7    NA
#>  8     8    NA    NA
#>  9     9     9    NA
#> 10    10    NA    NA
```

### Set Equality

Compare datasets cell-by-cell (requires matching rows and columns):

``` r
A == A  # All TRUE (or NA for missing values)
#> # A tibble: 10 × 2
#>        i b    
#>    <int> <lgl>
#>  1     1 TRUE 
#>  2     2 NA   
#>  3     3 TRUE 
#>  4     4 NA   
#>  5     5 TRUE 
#>  6     6 NA   
#>  7     7 TRUE 
#>  8     8 NA   
#>  9     9 TRUE 
#> 10    10 NA
A == B  # Shows which cells match
#> # A tibble: 10 × 2
#>        i b    
#>    <int> <lgl>
#>  1     1 TRUE 
#>  2     2 FALSE
#>  3     3 FALSE
#>  4     4 FALSE
#>  5     5 TRUE 
#>  6     6 FALSE
#>  7     7 TRUE 
#>  8     8 FALSE
#>  9     9 TRUE 
#> 10    10 FALSE
```

Equality fails with mismatched rows or columns:

``` r
A == C  # Error: rows and columns don't match
#> Error in dataset_equality(a, b): cols don't align
#>  cols left: ib
#>  cols right: ibc
```

## Helper Functions

``` r
# Extract ID columns
ids(A)
#> # A tibble: 10 × 1
#>        i
#>    <int>
#>  1     1
#>  2     2
#>  3     3
#>  4     4
#>  5     5
#>  6     6
#>  7     7
#>  8     8
#>  9     9
#> 10    10

# Extract value columns (non-ID columns)
vals(A)
#> # A tibble: 10 × 1
#>        b
#>    <int>
#>  1     1
#>  2    NA
#>  3     3
#>  4    NA
#>  5     5
#>  6    NA
#>  7     7
#>  8    NA
#>  9     9
#> 10    NA
```

## Mathematical Properties

The set operations satisfy the fundamental identity:
`a = (a ∩ b) ∪ (a \ b)`

``` r
# Verify: A == (A > B) + (A - B)
all(vals(A == (A > B) + (A - B)), na.rm = TRUE)
#> [1] TRUE
```

## License

GPL (\>= 3)
