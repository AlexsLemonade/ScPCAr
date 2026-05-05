# Get sample metadata by sample ID

Get sample metadata by sample ID

## Usage

``` r
get_sample_info(sample_id, simplifyVector = TRUE)
```

## Arguments

- sample_id:

  The ScPCA sample ID (e.g. "SCPCS000001")

- simplifyVector:

  Simplify the returned list structure, creating vectors and data frames
  instead of lists when possible. Default is TRUE.

## Value

A nested list of sample metadata from the ScPCA API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get metadata for a specific sample
sample_info <- get_sample_info("SCPCS000001")
} # }
```
