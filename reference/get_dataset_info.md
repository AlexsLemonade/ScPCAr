# Get the status and contents of a custom dataset

Returns the full dataset detail, including the `$data` field showing
which samples are included and processing status fields such as
`$is_started`, `$is_processing`, `$is_succeeded`, and `$is_failed`.

## Usage

``` r
get_dataset_info(dataset, auth_token)
```

## Arguments

- dataset:

  the dataset UUID string, or a list with an `$id` element such as the
  return value of
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)
  or `get_dataset_info()`.

- auth_token:

  an authorization token obtained from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md);
  must match the token used to create the dataset.

## Value

the dataset detail as a list

## Examples

``` r
if (FALSE) { # \dontrun{
status <- get_dataset_info("your-dataset-uuid", auth_token = token)
status$data         # nested list of projects and samples
status$is_succeeded # TRUE when the dataset file is ready for download

# You can also pass the result of a previous get_dataset_info() call:
status <- get_dataset_info(status, auth_token = token)
} # }
```
