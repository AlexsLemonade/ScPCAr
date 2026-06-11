# Get the processing status of a custom dataset

Returns a single string describing where a dataset is in the processing
lifecycle.

## Usage

``` r
get_dataset_status(dataset, auth_token = Sys.getenv("SCPCA_AUTH_TOKEN"))
```

## Arguments

- dataset:

  the dataset UUID string (such as the value returned by
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)),
  or a list with an `$id` element (such as the value returned by
  [`get_dataset_detail()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_detail.md)).

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

- `"pending"`: the dataset has not been started

- `"processing"`: the dataset has been started but is not yet finished

- `"succeeded"`: processing finished and the dataset is ready to
  download

- `"expired"`: processing completed but the generated download has since
  expired and must be regenerated

- `"failed"`: processing failed

## Examples

``` r
if (FALSE) { # \dontrun{
get_dataset_status(ds_id)
} # }
```
