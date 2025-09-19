test_that("validate_format works correctly", {
  expect_equal(validate_format("sce"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(validate_format("SCE"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(validate_format("SingleCellExperiment"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(validate_format("single-cell-experiment"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(validate_format("single_cell_experiment"), "SINGLE_CELL_EXPERIMENT")

  expect_equal(validate_format("anndata"), "ANN_DATA")
  expect_equal(validate_format("H5AD"), "ANN_DATA")

  expect_equal(validate_format("spatial"), "SPATIAL")
  expect_equal(validate_format("SpaceRanger"), "SPATIAL")
  expect_equal(validate_format("space ranger"), "SPATIAL")

  expect_error(validate_format("invalid"), "Invalid format")
  expect_error(validate_format(123), "format must be a single string")
  expect_error(validate_format(c("sce", "anndata")), "format must be a single string")
})

test_that("parse_download_file extracts filename correctly", {
  # Test URL with response-content-disposition parameter
  test_url <- paste0(
    "https://example.com/download?response-content-disposition=attachment%3B%20filename%3D",
    "SCPCS000001_SINGLE-CELL_SINGLE-CELL-EXPERIMENT_2025-09-09.zip"
  )
  result <- parse_download_file(test_url)
  expect_equal(result, "SCPCS000001_SINGLE-CELL_SINGLE-CELL-EXPERIMENT_2025-09-09.zip")

  # Test URL with different format
  test_url2 <- paste0(
    "https://example.com/file?response-content-disposition=inline%3B%20filename%3D",
    "SCPCP000001_SINGLE-CELL_ANN-DATA_2025-09-09.zip"
  )
  result2 <- parse_download_file(test_url2)
  expect_equal(result2, "SCPCP000001_SINGLE-CELL_ANN-DATA_2025-09-09.zip")
})


test_that("download_sample validates input parameters", {
  # Test missing auth token
  expect_error(
    download_sample("SCPCS000001", ""),
    "Authorization token must be provided"
  )

  expect_error(
    download_sample("SCPCS000001", character(0)),
    "Authorization token must be provided"
  )

  # Test invalid quiet parameter
  expect_error(
    download_sample("SCPCS000001", "valid-token", quiet = "not-logical"),
    "quiet must be a logical value"
  )

  expect_error(
    download_sample("SCPCS000001", "valid-token", quiet = c(TRUE, FALSE)),
    "quiet must be a logical value"
  )
})

test_that("download_project validates input parameters", {
  # Test missing auth token
  expect_error(
    download_project("SCPCP000001", ""),
    "Authorization token must be provided"
  )

  # Test invalid quiet parameter
  expect_error(
    download_project("SCPCP000001", "valid-token", quiet = "not-logical"),
    "quiet must be a logical value"
  )

  # Test invalid merged parameter
  expect_error(
    download_project("SCPCP000001", "valid-token", merged = "not-logical"),
    "merged must be a logical value"
  )

  # Test invalid include_multiplexed parameter
  expect_error(
    download_project("SCPCP000001", "valid-token", include_multiplexed = "not-logical"),
    "include_multiplexed must be NULL or a logical value"
  )
})

test_that("download_project validates format and merged combinations", {
  # Mock dependencies to avoid network calls
  local_mocked_bindings(
    get_project_info = function(id) list(has_multiplexed_data = FALSE),
    get_computed_file_ids = function(...) character(0)
  )

  # Test that spatial + merged fails
  expect_error(
    download_project("SCPCP000001", "valid-token", format = "spatial", merged = TRUE),
    "Merged spatial files are not available"
  )
})

test_that("download_project sets include_multiplexed defaults correctly", {
  # Mock dependencies to avoid network calls
  local_mocked_bindings(
    get_project_info = function(id) list(has_multiplexed_data = TRUE),
    get_computed_file_ids = function(...) character(0)
  )

  # For SCE format, include_multiplexed should default to TRUE
  expect_error(
    download_project("SCPCP000001", "valid-token", format = "sce"),
    "No.*multiplexed.*files found"
  )

  # For AnnData format, include_multiplexed should default to FALSE
  expect_error(
    download_project("SCPCP000001", "valid-token", format = "anndata"),
    "No files found for project.*in format anndata"
  )

  # For spatial format, include_multiplexed should default to FALSE
  expect_error(
    download_project("SCPCP000001", "valid-token", format = "spatial"),
    "No files found for project.*in format spatial"
  )
})

test_that("download_and_extract_file respects overwrite parameter", {
  # Create a temporary directory for testing
  temp_dir <- file.path(tempdir(), "test_overwrite")
  test_dest_dir <- file.path(temp_dir, "test_file")

  # Create the destination directory to simulate existing files
  dir.create(test_dest_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Mock parse_download_file to return consistent filename
  local_mocked_bindings(
    parse_download_file = function(url) "test_file.zip"
  )

  # Test with overwrite = FALSE (should skip and return empty)
  expect_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
      temp_dir,
      overwrite = FALSE,
      quiet = TRUE
    )
  )

  expect_equal(result, c())
  expect_true(dir.exists(test_dest_dir))
})

test_that("download_and_extract_file handles file unzipping", {
  # Create a temporary directory and a test zip file
  temp_dir <- file.path(tempdir(), "test_unzip")
  zip_file <- file.path(tempdir(), "test_real.zip")
  on.exit(
    {
      unlink(temp_dir, recursive = TRUE)
      unlink(zip_file)
    },
    add = TRUE
  )

  # Create a real zip file for testing
  test_content_dir <- file.path(tempdir(), "zip_content")
  dir.create(test_content_dir, showWarnings = FALSE)
  writeLines(c("line1", "line2"), file.path(test_content_dir, "test.txt"))
  writeLines(c("data1", "data2"), file.path(test_content_dir, "data.csv"))
  utils::zip(zip_file, test_content_dir, flags = "-rq")

  local_mocked_bindings(
    parse_download_file = function(url) basename(zip_file),
    curl_download = function(url, destfile, quiet) {
      file.copy(zip_file, destfile)
    }
  )

  download_and_extract_file(
    "https://example.com/test.zip",
    temp_dir,
    quiet = TRUE
  )

  # Check that files were extracted
  expect_true(dir.exists(temp_dir))

  # Check extracted files exist
  extracted_files <- list.files(temp_dir, recursive = TRUE)
  expect_setequal(basename(extracted_files), c("test.txt", "data.csv"))
  unzipped_files <- basename(list.files(temp_dir, recursive = TRUE))
  expect_setequal(unzipped_files, c("test.txt", "data.csv"))
})
