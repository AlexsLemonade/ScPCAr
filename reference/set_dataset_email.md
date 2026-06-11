# Set the notification email for a custom dataset

Updates the email address the ScPCA Portal will use to notify you when
the dataset is ready for download, by sending a PUT request with a new
`email` value. The dataset's samples, projects, and format are left
unchanged.

## Usage

``` r
set_dataset_email(dataset, email, auth_token = Sys.getenv("SCPCA_AUTH_TOKEN"))
```

## Arguments

- dataset:

  the dataset UUID string, or a list with an `$id` element.

- email:

  the email address to use for the download notification.

- auth_token:

  an authorization token from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).
  Defaults to the `SCPCA_AUTH_TOKEN` environment variable, which
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  sets automatically.

## Value

the dataset ID as a character string (invisibly)

## Details

A dataset that has already been started cannot be modified.

## Examples

``` r
if (FALSE) { # \dontrun{
set_dataset_email(ds_id, email = "user@example.com")
} # }
```
