# Remove outflow data from Aeme object

Remove specified outflow data from an Aeme object.

## Usage

``` r
remove_outflow(aeme, outflow_id = NULL, all = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- outflow_id:

  A string specifying the identifier for the outflow.

- all:

  A logical value indicating whether to remove all inflows. If TRUE, the
  inflow_id argument is ignored and all inflows are removed.

## Value

Aeme object with outflow removed
