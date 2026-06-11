# Get a summary of a custom ScPCA dataset

Fetches a custom dataset and returns a structured summary of its
contents, including its processing status and a per-sample table
describing the modality of each sample.

## Usage

``` r
get_dataset_info(dataset, auth_token = Sys.getenv("SCPCA_AUTH_TOKEN"))
```

## Arguments

- dataset:

  the dataset UUID string (such as the value returned by
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)),
  or a list with an `$id` element (such as the value returned by this
  function).

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

a named list with the following elements:

- `id`: the dataset UUID string

- `format`: the dataset file format (e.g. "SINGLE_CELL_EXPERIMENT",
  "ANN_DATA")

- `status`: the processing status — one of "pending", "processing",
  "succeeded", "failed", or "expired" (see
  [`get_dataset_status()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_status.md))

- `n_samples`: the total number of samples in the dataset, taken from
  the API's `total_sample_count`

- `n_projects`: the number of projects in the dataset

- `sample_info`: a data frame with one row per included sample and the
  following columns:

  - `scpca_sample_id`

  - `scpca_project_id`

  - `seq_unit` ("cell" or "nucleus", or `NA` if the sample is not
    included as single-cell)

  - `has_spatial`

  - `has_bulk`

  - `has_cite_seq`

  - `has_multiplexed`

- `merged_projects`: a character vector of project IDs whose single-cell
  data is merged; `character(0)` when none

## Details

For each project, the included samples and their modality details are
looked up from the project's sample records (one request per project),
so merged projects (whose individual sample IDs are not enumerated in
the dataset record) are expanded to all of their single-cell samples.
Projects whose single-cell data is merged are also listed in
`merged_projects`.

## Examples

``` r
if (FALSE) { # \dontrun{
ds_id <- create_dataset(samples = c("SCPCS000001", "SCPCS000002"))
info <- get_dataset_info(ds_id)
info$status
info$sample_info
} # }
```
