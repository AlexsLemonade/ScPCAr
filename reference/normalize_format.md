# Internal helper to validate and normalize formats for the ScPCA API

Note that while spatial format strings are included here, the API does
not currently accommodate a format code for spatial data; that is
designated in the modality field for individual samples or precomputed
datasets.

## Usage

``` r
normalize_format(format, allow_spatial = TRUE)
```

## Arguments

- format:

  The input format string

- allow_spatial:

  Whether to allow spatial format strings (default TRUE)

## Value

The normalized format string for API use
