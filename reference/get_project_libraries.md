# Get metadata for all libraries in a given project

This function downloads and reads the library metadata file for a given
ScPCA project. The data frame returned will be the same as the project
metadata file available from the ScPCA Portal website for each project,
including information about each library that is part of the project.

## Usage

``` r
get_project_libraries(project_id, auth_token)
```

## Arguments

- project_id:

  The ScPCA project ID (e.g. "SCPCP000001")

- auth_token:

  An authorization token obtained from
  [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)

## Value

A data frame (tibble) of library metadata for the specified project.

## Examples

``` r
if (FALSE) { # \dontrun{
# First get an auth token
token <- get_auth("me@email.net", agree = TRUE)
# Get library metadata for a specific project
libraries_df <- get_project_libraries("SCPCP000001", token)
} # }
```
