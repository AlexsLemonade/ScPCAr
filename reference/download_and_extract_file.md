# Download and extract a single file from a URL

Download and extract a single file from a URL

## Usage

``` r
download_and_extract_file(url, parent_dir, overwrite, redownload, quiet)
```

## Arguments

- url:

  The download URL

- parent_dir:

  The parent directory where files should be extracted

- overwrite:

  Whether to overwrite existing directories

- redownload:

  Whether to re-download if files from the same url already exist (if
  FALSE, existing files will be returned)

- quiet:

  Whether to suppress progress messages

## Value

A character vector of extracted file paths
