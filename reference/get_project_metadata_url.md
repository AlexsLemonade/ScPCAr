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

A signed download URL for the project metadata file as would be found
from the ScPCA Portal.

## Examples

``` r
if (FALSE) { # \dontrun{
# First get an auth token
token <- get_auth("me@email.net", agree = TRUE)
# Get metadata for a specific project
project_info <- get_project_metadata_url("SCPCP000001", token)
} # }
```
