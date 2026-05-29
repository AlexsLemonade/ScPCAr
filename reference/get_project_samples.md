# Get a data frame of all samples in a given project

This function retrieves a data frame of all biological samples
associated with a SCPCA project, including sample-level metadata. By
default, list columns are removed to create a simplified data frame, but
this can be disabled by setting `simplify = FALSE`. The unsimplified
data frame contains nested list columns with additional details, such as
the experimental modalities associated with each sample.

## Usage

``` r
get_project_samples(project_id, simplify = TRUE)
```

## Arguments

- project_id:

  The ScPCA project ID (e.g. "SCPCP000001")

- simplify:

  A logical indicating whether to simplify the resulting data frame by
  removing list columns. Default is TRUE.

## Value

A data frame (tibble) of sample information for the specified project
from the ScPCA API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get sample info for a specific project
samples_df <- get_project_samples("SCPCP000001")

# Get sample info without simplifying
samples_df_full <- get_project_samples("SCPCP000001", simplify = FALSE)
} # }
```
