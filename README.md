---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rDataSet

<!-- badges: start -->
<!-- badges: end -->

The goal of rDataSet is to treat data frames as mathematical sets. It provides set-theoretic operations (union, intersection, difference, equality) for data frames where values are identified by row IDs and column names, and presence/absence is determined by `NA` values.

## Installation

You can install the development version of rDataSet from local source like so:

``` r
devtools::install("/path/to/rDataSet")
```

## Overview

rDataSet introduces a `dataset` S3 class that extends data frames with designated ID columns. Set operations work on a cell-by-cell basis:

- **ID columns** uniquely identify each row (one-to-one relation)
- **Value presence** is determined by `is.na()` - a value is present if not `NA`
- **Set operations** compare cells at the same (row_id, col_id) position

## Set Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Difference | `A - B` | Values in A where corresponding cells in B are `NA` |
| Intersection | `A > B` | Values in A where corresponding cells in B are not `NA` |
| Union | `A + B` | Values from A, falling back to B where A has `NA` |
| Equality | `A == B` | Cell-wise comparison returning logical values |

## Example


``` r
library(rDataSet)
library(tibble)
library(dplyr)

# Create two datasets with 'i' as the ID column
A <- dataset_build(
  tibble(i = 1:10, b = if_else(1:10 %% 2 == 0, NA, 1:10)),
  ids = "i"
)

B <- dataset_build(
  tibble(i = 1:10, b = na_if(1:10, 3)),
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
```

### Set Difference

Keep values from A where B has `NA`:


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

### Set Intersection

Keep values from A where B has non-`NA` values (B acts as a filter):


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

### Set Union

Combine values from both datasets, with A taking precedence:


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

### Set Equality

Compare datasets cell-by-cell:


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

The set operations satisfy the fundamental identity: `a = (a ∩ b) ∪ (a \ b)`


``` r
# Verify: A == (A > B) + (A - B)
all(vals(A == (A > B) + (A - B)), na.rm = TRUE)
#> [1] TRUE
```

## License

GPL (>= 3)
