# Read and write AED parameter CSV files These functions handle reading and writing AED parameter CSV files, ensuring that single quotes are properly managed in column names and specific columns.

Read and write AED parameter CSV files These functions handle reading
and writing AED parameter CSV files, ensuring that single quotes are
properly managed in column names and specific columns.

## Usage

``` r
read_aed_param_csv(file)

write_aed_param_csv(df, file)
```

## Arguments

- file:

  The path to the CSV file.

- df:

  A data frame to be written to CSV.

## Value

For `read_aed_param_csv`, a data frame read from the CSV file.
