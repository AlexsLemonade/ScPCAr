# Changelog

## ScPCAr 0.2.0

### New features

- Updated all functions to support the current scpca.alexslemonade.org
  API.
- Added support for custom datasets, including new functions to create,
  manage, and download custom datasets. Core functions include:
  - [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)
    creates a new custom dataset with specified samples or projects.
  - [`get_dataset_status()`](https://alexslemonade.github.io/ScPCAr/reference/get_dataset_status.md)
    checks the processing status of a dataset.
  - [`start_dataset_processing()`](https://alexslemonade.github.io/ScPCAr/reference/start_dataset_processing.md)
    triggers processing of a dataset.
  - [`download_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/download_dataset.md)
    downloads the processed files for a dataset.
- [`get_auth()`](https://alexslemonade.github.io/ScPCAr/reference/get_auth.md)
  now stores the authorization token in the `SCPCA_AUTH_TOKEN`
  environment variable automatically, so it does not need to be passed
  explicitly to other functions.

### Deprecated

- Downloading individual samples has been deprecated, along with all
  functions that rely on it. The `computed-files` endpoint that these
  functions relied on is being removed from the API. These functions
  will be removed in an imminent release, so their use should be
  discontinued as soon as possible. Please use
  [`create_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/create_dataset.md)
  to submit a request, then
  [`download_dataset()`](https://alexslemonade.github.io/ScPCAr/reference/download_dataset.md)
  to retrieve files. Affected functions include:
  - [`download_sample()`](https://alexslemonade.github.io/ScPCAr/reference/download_sample.md)
  - [`get_computed_file_ids()`](https://alexslemonade.github.io/ScPCAr/reference/get_computed_file_ids.md)
  - [`computed_files_filter()`](https://alexslemonade.github.io/ScPCAr/reference/computed_files_filter.md)

## ScPCAr 0.1.0

Initial release
