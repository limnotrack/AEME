# Remove inflow data from Aeme object

Remove inflow data from an Aeme object. You can specify a particular
inflow to remove by providing its identifier, or you can remove all
inflows at once. If you choose to remove all inflows, the inflow_id
argument will be ignored.

## Usage

``` r
remove_inflow(aeme, inflow_id = NULL, all = FALSE)
```

## Arguments

- aeme:

  aeme; object.

- inflow_id:

  A string specifying the identifier for the inflow.

- all:

  A logical value indicating whether to remove all inflows. If TRUE, the
  inflow_id argument is ignored and all inflows are removed.

## Value

Aeme object with inflow removed
