# Set the notification email for a custom dataset

Updates the email address the ScPCA Portal will use to notify you when
the dataset is ready for download, by sending a PUT request with a new
`email` value. The dataset's samples, projects, and format are left
unchanged.

## Usage

``` r
set_dataset_email(dataset, auth_token, email)
```

## Arguments

- dataset:

  the dataset UUID string, or a list with an `$id` element.

- auth_token:

  an authorization token obtained from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md).

- email:

  the email address to use for the download notification.

## Value

the updated dataset detail as a list (invisibly)

## Details

A dataset that has already been started cannot be modified.

## Examples

``` r
if (FALSE) { # \dontrun{
set_dataset_email(ds, auth_token = token, email = "user@example.com")
} # }
```
