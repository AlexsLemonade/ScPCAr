# Get the processing status of a custom dataset

Returns a single string describing where a dataset is in the processing
lifecycle, by fetching the dataset detail and translating its status
fields (`is_started`, `is_succeeded`, `is_failed`). A dataset that has
been started but has neither succeeded nor failed is reported as
"processing".

## Usage

``` r
get_dataset_status(dataset, auth_token = Sys.getenv("SCPCA_AUTH_TOKEN"))
```

## Arguments

- dataset:

  the dataset UUID string, or a list with an `$id` element, such as the
  return value of
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md).

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

a single character string: one of "pending", "processing", "succeeded",
"failed", or "expired".

## Details

Possible values are:

- "pending":

  the dataset has not been started

- "processing":

  the dataset has been started but is not yet finished

- "succeeded":

  processing finished and the dataset is ready to download

- "failed":

  processing failed

## Examples

``` r
if (FALSE) { # \dontrun{
get_dataset_status(ds)
} # }
```
