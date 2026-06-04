# Download and extract a single file from a URL

Download and extract a single file from a URL

## Usage

``` r
download_and_extract_file(
  url,
  parent_dir,
  overwrite,
  redownload,
  quiet,
  unzip = TRUE
)
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

- unzip:

  Whether to unzip the downloaded file. Default is TRUE. When FALSE, the
  zip file is saved directly to `parent_dir` and its path is returned.

## Value

A character vector of extracted file paths, or the zip file path when
`unzip = FALSE`.
