# Build the data object for the ScPCA datasets API from sample and project IDs

Build the data object for the ScPCA datasets API from sample and project
IDs

## Usage

``` r
build_dataset_data(samples = NULL, projects = NULL, include_bulk = FALSE)
```

## Arguments

- samples:

  optional character vector of sample IDs (SCPCS format)

- projects:

  optional character vector of project IDs (SCPCP format)

- include_bulk:

  logical; whether to include bulk RNA-seq files per project

## Value

a nested list suitable for the `data` field of the datasets API
