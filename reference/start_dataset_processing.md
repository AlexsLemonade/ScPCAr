# Start processing a custom dataset

Starts processing of an existing custom dataset so that its files can be
built for download, by sending a PUT request that sets `start = TRUE`.
Optionally sets the notification email as part of the same request.

## Usage

``` r
start_dataset_processing(
  dataset,
  email = NULL,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- dataset:

  the dataset UUID string, or a list with an `$id` element, such as the
  return value of
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md).

- email:

  optional email address for the download notification. When supplied,
  it is set as part of the same request that starts processing.

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

the updated dataset detail as a list (invisibly)

## Details

Once processing has started a dataset is locked and can no longer be
modified; attempting to modify or re-start it will raise an error.

## Examples

``` r
if (FALSE) { # \dontrun{
ds <- create_dataset(samples = c("SCPCS000001", "SCPCS000002"))
start_dataset_processing(ds, email = "user@example.com")
} # }
```
