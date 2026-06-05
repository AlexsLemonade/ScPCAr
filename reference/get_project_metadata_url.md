# Get project metadata download URL

Get project metadata download URL

## Usage

``` r
get_project_metadata_url(project_id, auth_token)
```

## Arguments

- project_id:

  The ScPCA project ID (e.g. "SCPCP000001")

- auth_token:

  An authorization token obtained from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)

## Value

A named character vector: the value is the signed download URL for the
project metadata file and the name is the download filename. Errors if
no metadata dataset is available for the project, and warns if more than
one is found (returning the first value).

## Examples

``` r
if (FALSE) { # \dontrun{
# First get an auth token
token <- get_auth("me@email.net", agree = TRUE)
# Get metadata for a specific project
project_info <- get_project_metadata_url("SCPCP000001", token)
} # }
```
