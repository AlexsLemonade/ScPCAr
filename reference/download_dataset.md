# Download a custom dataset's files from the ScPCA Portal

Downloads and optionally extracts the files for a custom dataset. If the
dataset has completed processing and has a status of "succeeded", as
determined by
[`get_dataset_status()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_status.md),
then the download will proceed immediately.

## Usage

``` r
download_dataset(
  dataset,
  destination = "scpca_data",
  overwrite = FALSE,
  redownload = FALSE,
  unzip = TRUE,
  quiet = FALSE,
  await_processing = FALSE,
  poll_interval = 0.5,
  timeout = 60,
  auth_token = Sys.getenv("SCPCA_AUTH_TOKEN")
)
```

## Arguments

- dataset:

  the dataset UUID string (such as the value returned by
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)),
  or a list with an `$id` element (such as the value returned by
  [`get_dataset_detail()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_detail.md)).

- destination:

  The path to the directory where the unzipped file directory should be
  saved. Default is "scpca_data".

- overwrite:

  Whether to overwrite files in existing directories if they already
  exist. Note that files in existing directories that do not have the
  same name as one of the downloaded files will not be deleted. Default
  is FALSE.

- redownload:

  Whether to re-download if files from the same dataset already exist.
  If FALSE, existing files will be returned. Default is FALSE.

- unzip:

  Whether to unzip the downloaded file. Default is TRUE. When FALSE, the
  zip file is saved directly to `destination` and its path is returned.

- quiet:

  Whether to suppress download progress messages. Default is FALSE.

- await_processing:

  Whether to wait for dataset processing to complete before downloading.
  If `TRUE` and the dataset has not yet been started, processing will be
  started automatically. Default is FALSE.

- poll_interval:

  Number of minutes to wait between status checks when
  `await_processing = TRUE`. Default is 0.5 (30 seconds).

- timeout:

  Maximum number of minutes to wait for processing to complete when
  `await_processing = TRUE`. Use `Inf` to wait indefinitely. Default is
  60 (1 hour).

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

a vector of file paths for the downloaded files (invisibly)

## Details

If the dataset is not yet ready, or if processing has not yet been
initiated with
[`start_dataset_processing()`](https://alexslemonade.github.io/ScPCAr/reference/start_dataset_processing.md),
you can set `await_processing = TRUE` to have `download_dataset()` wait
for processing to complete (starting processing if needed) before
downloading. Note that processing can take many minutes, with a minimum
wait of approximately 1 minute for very small datasets. If you do not
want to wait with a blocked R session, it may be better to manually
handle checking
[`get_dataset_status()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_status.md)
before download.

The downloaded files are saved in a subdirectory of `destination`, named
from the dataset's download filename (which includes the dataset ID,
format, and date).

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a dataset
ds <- create_dataset(samples = c("SCPCS000001", "SCPCS000002"))

# Manually start processing, check status, and download
# when the status is "succeeded"
start_dataset_processing(ds, email = "user@example.com")
get_dataset_status(ds) # or wait for an email after successful processing
download_dataset(ds, destination = "scpca_data")

# Alternatively, start processing (if needed), wait for processing
# to complete, and download in a single call
download_dataset(ds, await_processing = TRUE)
} # }
```
