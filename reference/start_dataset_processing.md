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

  the dataset UUID string (such as the value returned by
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)),
  or a list with an `$id` element (such as the value returned by
  [`get_dataset_detail()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_detail.md)).

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

the dataset ID as a character string (invisibly)

## Details

Before sending the request the current dataset status is checked via
[`get_dataset_status()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_status.md):

- A `"pending"` or `"expired"` dataset is started normally.

- A `"failed"` dataset is retried with a warning.

- A `"processing"` or `"succeeded"` dataset is already underway or done;
  a message is emitted and no request is sent.

## Examples

``` r
if (FALSE) { # \dontrun{
ds_id <- create_dataset(samples = c("SCPCS000001", "SCPCS000002"))
start_dataset_processing(ds_id, email = "user@example.com")
} # }
```
