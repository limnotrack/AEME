# Construct a "grouped" (non depth x time) model output variable

Some GLM-AED output variables have dimensions AEME has no fixed
convention for (e.g. `nzones`, `particle`, `sed_layers`, `lon`, `lat`),
unlike the package's usual `(time)`-vector / `(z, time)`-matrix output
shapes. Rather than force such a variable into the depth x time
convention – which would silently misinterpret the extra axis as depth
or time –
[`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md)
keeps it as its own labelled array via this constructor, so the actual
index/coordinate values for every dimension are always available
alongside the data, ready to be interpreted properly later.

## Usage

``` r
new_grouped_var(value, dim_names, dim_values)
```

## Arguments

- value:

  array; the variable's data, with dimensions in the same order as
  `dim_names`.

- dim_names:

  character; the name of each dimension of `value`, in order (e.g.
  `c("nzones", "time")`).

- dim_values:

  named list; one element per entry in `dim_names`, giving the
  coordinate/index values along that dimension (e.g. zone numbers, or a
  `Date` vector for a `"time"` dimension).

## Value

An object of class `aeme_grouped_var`.
