# Wait for a custom dataset to finish processing

Polls the dataset processing status until it reaches "succeeded", then
returns the dataset ID invisibly. Errors if processing fails, expires
unexpectedly, or the timeout is reached.

## Usage

``` r
await_dataset_processing(
  dataset,
  poll_interval = 0.5,
  timeout = 60,
  quiet = FALSE,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- dataset:

  the dataset UUID string (such as the value returned by
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)),
  or a list with an `$id` element (such as the value returned by
  [`get_dataset_detail()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_detail.md)).

- poll_interval:

  Number of minutes to wait between status checks when
  `await_processing = TRUE`. Default is 0.5 (30 seconds).

- timeout:

  Maximum number of minutes to wait for processing to complete when
  `await_processing = TRUE`. Use `Inf` to wait indefinitely. Default is
  60 (1 hour).

- quiet:

  Whether to suppress download progress messages. Default is FALSE.

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

the dataset ID string (invisibly)

## Details

This function only waits — it does not start processing or download
data. To start processing, use
[`start_dataset_processing()`](https://alexslemonade.github.io/ScPCAr/reference/start_dataset_processing.md)
first, or call
[`download_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/download_dataset.md)
with `await_processing = TRUE` to start, wait, and download in one step.

## Examples

``` r
if (FALSE) { # \dontrun{
ds <- create_dataset(samples = c("SCPCS000001", "SCPCS000002"))
start_dataset_processing(ds, email = "user@example.com")
await_dataset_processing(ds)
download_dataset(ds)
} # }
```
