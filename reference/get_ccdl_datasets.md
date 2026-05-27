# Get CCDL dataset objects from the ScPCA API

Get CCDL dataset objects from the ScPCA API

## Usage

``` r
get_ccdl_datasets(
  project_id = NULL,
  modality = NULL,
  format = NULL,
  merged = NULL,
  include_multiplexed = NULL,
  metadata_only = FALSE,
  auth_token = ""
)
```

## Arguments

- project_id:

  Optional ScPCA project ID to filter by (e.g. "SCPCP000001")

- modality:

  Optional modality string to filter by (mapped to `ccdl_modality`)

- format:

  Optional format string to filter by (mapped to `ccdl_format`)

- merged:

  Optional logical to filter merged datasets (mapped to
  `ccdl_is_merged`)

- include_multiplexed:

  Optional logical to filter by whether the dataset includes multiplexed
  files (mapped to `includes_files_multiplexed`)

- metadata_only:

  Logical; if TRUE maps to `ccdl_name = "ALL_METADATA"`

- auth_token:

  Optional API authentication token; when non-empty adds `api-key`
  header

  No validation is performed on parameter values; invalid values are
  passed directly to the API and will result in an API error.

## Value

a list of CCDL dataset objects
