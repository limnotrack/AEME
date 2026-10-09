# Upgrade an Aeme object to the current AEME version

Older `Aeme` objects – loaded from `.rds` files written by a previous
version of AEME – can be missing list elements, use since-renamed slot
names, or carry data frames with an older column layout. Most of the
package tolerates this, but some code paths (and
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) in
particular) assume the current layout.

`upgrade_aeme()` applies every structural migration AEME knows about, in
order, each one idempotent so the function is safe to run repeatedly. It
does **not** rebuild model configuration
(`configuration$<model>$hydrodynamic`) or model output – those only come
from [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) /
[`run_aeme()`](https://limnotrack.com/reference/run_aeme.md). Run
`upgrade_aeme()` first, then rebuild if you need the model files
refreshed.

Migrations applied (see also migrate_aeme(), the silent worker):

- `time$spin_up`, `inflows$factor`, `outflows$factor`, `configuration`:
  backfill entries for models added to AEME after the object was created
  (e.g. `simstrat_aed2`, `simstrat_aed`).

- `outflows`: rename the legacy `lvl` / `outflow_lvl` element to
  `elevation`.

- `output`: add a `NULL` placeholder per model and coerce `n_members` to
  integer.

- `observations$level`: coerce a legacy tibble to a plain data frame and
  ensure a `var_aeme` column.

- `observations$lake`: collapse the legacy `depth_from` / `depth_to`
  column pair to a single `depth` column (interval midpoint), keeping
  `depth_to` only where it records a genuine integrated sample.

- `observations$lake` / `observations$level`: convert a `Date` `Date`
  column to noon-anchored UTC `POSIXct` (`<date> 12:00:00`), so daily
  observations match a sub-daily model axis unambiguously.

- `configuration`: backfill scalar build defaults (`ext_elev`,
  `calc_wbal`, `wb_method`, `calc_wlev`, `hum_type`, `est_swr_hr`,
  `use_bgc`) from `config_defaults()`.

- `parameters`: reorder columns to
  [`param_colnames()`](https://limnotrack.com/reference/param_colnames.md)
  order.

## Usage

``` r
upgrade_aeme(aeme, quiet = FALSE)
```

## Arguments

- aeme:

  An `Aeme` object.

- quiet:

  Logical; suppress the summary of applied changes. Default `FALSE`.

## Value

The `Aeme` object, migrated to the current layout, with
`configuration$aeme_upgraded` set to the installed AEME version.

## See also

[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md),
[`check_aeme()`](https://limnotrack.com/reference/check_aeme.md)
