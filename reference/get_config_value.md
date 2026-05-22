# Get a configuration value from the aeme object. If the key is not present, return the default value.

Get a configuration value from the aeme object. If the key is not
present, return the default value.

## Usage

``` r
get_config_value(aeme, key, default = NULL)
```

## Arguments

- aeme:

  Aeme object.

- key:

  The name of the configuration value to retrieve.

- default:

  The default value to return if the key is not present in the
  configuration. If NULL, the default value from config_defaults() will
  be used. If the key is not present in config_defaults(), NULL will be
  returned.

## Value

The value of the configuration key, or the default value if the key is
not present in the configuration.
