# ScPCAr 0.2.0

## New features

- Updated all functions to support the current scpca.alexslemonade.org API.
- Added support for custom datasets, including new functions to create, manage, and download custom datasets.
  Core functions include:
  - `create_dataset()` creates a new custom dataset with specified samples or projects.
  - `get_dataset_status()` checks the processing status of a dataset.
  - `start_dataset_processing()` triggers processing of a dataset.
  - `download_dataset()` downloads the processed files for a dataset.
- `get_auth()` now stores the authorization token in the `SCPCA_AUTH_TOKEN` environment variable automatically, so it does not need to be passed explicitly to other functions.

## Deprecated

- Downloading individual samples has been deprecated, along with all functions that rely on it.
  The `computed-files` endpoint that these functions relied on is being removed from the API.
  These functions will be removed in an imminent release, so their use should be discontinued as soon as possible. 
  Please use `create_dataset()` to submit a request, then `download_dataset()` to retrieve files.
  Affected functions include:
  - `download_sample()`
  - `get_computed_file_ids()`
  - `computed_files_filter()`



# ScPCAr 0.1.0

Initial release
