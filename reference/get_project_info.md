# Get project metadata by project ID

Get project metadata by project ID

## Usage

``` r
get_project_info(project_id, simplifyVector = TRUE)
```

## Arguments

- project_id:

  The ScPCA project ID (e.g. "SCPCP000001")

- simplifyVector:

  Simplify the returned list structure, creating vectors and data frames
  instead of lists when possible. Default is TRUE.

## Value

A nested list of project metadata from the ScPCA API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get metadata for a specific project
project_info <- get_project_info("SCPCP000001")
} # }
```
