# Convert a grouped variable to a long-format data frame

Convert a grouped variable to a long-format data frame

## Usage

``` r
# S3 method for class 'aeme_grouped_var'
as.data.frame(x, ...)
```

## Arguments

- x:

  an `aeme_grouped_var` object (see
  [`new_grouped_var()`](https://limnotrack.com/reference/new_grouped_var.md)).

- ...:

  unused.

## Value

data.frame with one row per combination of the variable's dimension
values, a column per dimension (named after that dimension and holding
its coordinate/index values – a `"time"` dimension becomes a `Date`
column), and a `value` column.
