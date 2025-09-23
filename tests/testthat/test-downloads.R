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

  # Test with overwrite = FALSE (should skip and return empty)
  expect_message(
    result <- download_and_extract_file(
      "https://example.com/test_file.zip",
      "test_file.zip",
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
  withr::with_dir(test_content_dir, {
    utils::zip(zip_file, ".", flags = "-rq")
  })

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      file.copy(zip_file, destfile)
    }
  )

  download_and_extract_file(
    "https://example.com/test.zip",
    "test.zip",
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

test_that("download_and_extract_file uses existing directory with same prefix when redownload = FALSE", {
  # Create a temporary directory for testing
  temp_dir <- file.path(tempdir(), "test_existing_prefix")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  dir.create(temp_dir, showWarnings = FALSE)

  # Create an existing directory with same base name but different date
  existing_dir <- file.path(temp_dir, "SCPCS000001_2024-01-15")
  dir.create(existing_dir, showWarnings = FALSE)

  # Add some test files to the existing directory
  writeLines(c("existing", "content"), file.path(existing_dir, "existing.txt"))
  writeLines(c("more", "data"), file.path(existing_dir, "data.csv"))

  # a filename with newer date but same prefix
  new_file <- "SCPCS000001_2024-02-20.zip"

  # Test with redownload = FALSE (default behavior)
  expect_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
      new_file,
      temp_dir,
      overwrite = FALSE,
      redownload = FALSE,
      quiet = TRUE
    ),
  )

  # Should return paths from the existing directory
  expected_files <- c(
    file.path(existing_dir, "existing.txt"),
    file.path(existing_dir, "data.csv")
  )
  expect_setequal(result, expected_files)
})

test_that("download_and_extract_file chooses latest directory when multiple exist", {
  # Create a temporary directory for testing
  temp_dir <- file.path(tempdir(), "test_multiple_existing")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  dir.create(temp_dir, showWarnings = FALSE)

  # Create multiple existing directories with same base name but different dates
  older_dir <- file.path(temp_dir, "SCPCS000001_2024-01-01")
  newer_dir <- file.path(temp_dir, "SCPCS000001_2025-01-21")
  dir.create(older_dir, showWarnings = FALSE)
  dir.create(newer_dir, showWarnings = FALSE)

  # Add different content to each directory
  writeLines(c("old", "content"), file.path(older_dir, "old.txt"))
  writeLines(c("new", "content"), file.path(newer_dir, "new.txt"))

  # new filename with same prefix
  new_file <- "SCPCS000001_2025-04-01.zip"

  # Test with redownload = FALSE
  expect_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
      new_file,
      temp_dir,
      overwrite = FALSE,
      redownload = FALSE,
      quiet = TRUE
    )
  )

  # Should return paths from the newer directory (alphabetically latest date)
  expected_files <- file.path(newer_dir, "new.txt")
  expect_setequal(result, expected_files)
})

test_that("download_and_extract_file proceeds with download when redownload = TRUE", {
  # Create a temporary directory for testing
  new_name <- "SCPCS000001_2025-02-22"
  temp_dir <- file.path(tempdir(), "test_redownload_true")
  zip_file <- file.path(tempdir(), glue::glue("{new_name}.zip"))
  on.exit(
    {
      unlink(temp_dir, recursive = TRUE)
      unlink(zip_file)
    },
    add = TRUE
  )
  dir.create(temp_dir, showWarnings = FALSE)

  # Create an existing directory with same base name but different date
  existing_dir <- file.path(temp_dir, "SCPCS000001_2025-01-01")
  dir.create(existing_dir, showWarnings = FALSE)
  writeLines(c("existing", "content"), file.path(existing_dir, "existing.txt"))

  # Create a real zip file for testing
  test_content_dir <- file.path(tempdir(), "new_zip_content")
  dir.create(test_content_dir, showWarnings = FALSE)
  writeLines(c("new", "downloaded", "content"), file.path(test_content_dir, "new.txt"))
  withr::with_dir(test_content_dir, {
    utils::zip(zip_file, ".", flags = "-rq")
  })

  # Mock functions
  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      file.copy(zip_file, destfile)
    }
  )

  # new filename with same prefix
  new_file <- paste0(new_name, ".zip")

  # Test with redownload = TRUE - should proceed with download
  expect_no_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
      new_file,
      temp_dir,
      overwrite = FALSE,
      redownload = TRUE,
      quiet = TRUE
    )
  )

  # Should have created new directory and extracted files
  new_dir <- file.path(temp_dir, new_name)
  expect_true(dir.exists(new_dir))
  expect_true(file.exists(file.path(new_dir, "new.txt")))

  # Both directories should exist
  expect_true(dir.exists(existing_dir))
  expect_true(dir.exists(new_dir))
})

test_that("download_and_extract_file redownloads when exact directory exists, overwrite=TRUE and redownload=FALSE", {
  # Create a temporary directory for testing
  dir_name <- "SCPCS000001_2025-03-15"
  temp_dir <- file.path(tempdir(), "test_overwrite_redownload")
  zip_file <- file.path(tempdir(), glue::glue("{dir_name}.zip"))
  on.exit(
    {
      unlink(temp_dir, recursive = TRUE)
      unlink(zip_file)
    },
    add = TRUE
  )
  dir.create(temp_dir, showWarnings = FALSE)

  # Create the EXACT destination directory that would be created by the download
  existing_dir <- file.path(temp_dir, dir_name)
  dir.create(existing_dir, showWarnings = FALSE)
  writeLines(c("old", "existing", "content"), file.path(existing_dir, "old.txt"))

  # Create a real zip file for testing with new content
  test_content_dir <- file.path(tempdir(), "overwrite_zip_content")
  dir.create(test_content_dir, showWarnings = FALSE)
  writeLines(c("new", "overwritten", "content"), file.path(test_content_dir, "new.txt"))
  withr::with_dir(test_content_dir, {
    utils::zip(zip_file, ".", flags = "-rq")
  })

  # Mock functions
  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      file.copy(zip_file, destfile)
    }
  )
  # new filename matching the existing directory
  new_file <- paste0(dir_name, ".zip")

  # Test with overwrite=TRUE and redownload=FALSE when exact destination exists
  # This should proceed with download because the exact destination exists AND overwrite=TRUE
  expect_no_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
      new_file,
      temp_dir,
      overwrite = TRUE,
      redownload = FALSE,
      quiet = TRUE
    )
  )

  # Should have overwritten the directory and extracted new files
  expect_true(dir.exists(existing_dir))
  expect_true(file.exists(file.path(existing_dir, "new.txt")))

  # Should return files from the newly downloaded directory
  expected_files <- file.path(existing_dir, "new.txt")
  expect_setequal(result, expected_files)

  # The old file should still exist
  # (overwrite doesn't clear directory to prevent user error if they put additional files there)
  expect_true(file.exists(file.path(existing_dir, "old.txt")))
})
