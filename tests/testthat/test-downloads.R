test_that("normalize_format works correctly", {
  expect_equal(normalize_format("sce"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(normalize_format("SCE"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(normalize_format("SingleCellExperiment"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(normalize_format("single-cell-experiment"), "SINGLE_CELL_EXPERIMENT")
  expect_equal(normalize_format("single_cell_experiment"), "SINGLE_CELL_EXPERIMENT")

  expect_equal(normalize_format("anndata"), "ANN_DATA")
  expect_equal(normalize_format("H5AD"), "ANN_DATA")

  expect_equal(normalize_format("spatial"), "SPATIAL")
  expect_equal(normalize_format("SpaceRanger"), "SPATIAL")
  expect_equal(normalize_format("space ranger"), "SPATIAL")
  expect_equal(normalize_format("spatial_spaceranger"), "SPATIAL")

  expect_error(normalize_format("invalid"), "Invalid format")
  expect_error(normalize_format(123), "format must be a single string")
  expect_error(normalize_format(c("sce", "anndata")), "format must be a single string")
})

test_that("normalize_format with allow_spatial = FALSE rejects spatial formats", {
  expect_error(
    normalize_format("spatial", allow_spatial = FALSE),
    "Invalid format"
  )
  expect_error(
    normalize_format("SpaceRanger", allow_spatial = FALSE),
    "Invalid format"
  )
})

test_that("normalize_format with allow_spatial = FALSE still accepts sce and anndata", {
  expect_equal(normalize_format("sce", allow_spatial = FALSE), "SINGLE_CELL_EXPERIMENT")
  expect_equal(normalize_format("anndata", allow_spatial = FALSE), "ANN_DATA")
})

test_that("normalize_format with allow_spatial = FALSE still errors on invalid formats", {
  expect_error(
    normalize_format("invalid", allow_spatial = FALSE),
    "Invalid format"
  )
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
    download_sample("SCPCS000001", auth_token = ""),
    "Authorization token must be provided"
  )

  expect_error(
    download_sample("SCPCS000001", auth_token = character(0)),
    "Authorization token must be provided"
  )

  # Test invalid quiet parameter
  expect_error(
    download_sample("SCPCS000001", auth_token = "valid-token", quiet = "not-logical"),
    "quiet must be a logical value"
  )

  expect_error(
    download_sample("SCPCS000001", auth_token = "valid-token", quiet = c(TRUE, FALSE)),
    "quiet must be a logical value"
  )
})

test_that("check_destination warns when destination looks like an auth token (UUID)", {
  # auth_token is the last argument, so a positionally-passed token lands in destination
  expect_warning(
    check_destination("123e4567-e89b-12d3-a456-426614174000"),
    "looks like an authorization token"
  )
  # a normal directory path does not warn
  expect_no_warning(check_destination("scpca_data"))
})

test_that("download_project validates input parameters", {
  # Test invalid project_id format
  expect_error(
    download_project("invalid", auth_token = "valid-token"),
    "Invalid project_id"
  )
  expect_error(
    download_project("SCPCS000001", auth_token = "valid-token"),
    "Invalid project_id"
  )

  # Test missing auth token
  expect_error(
    download_project("SCPCP000001", auth_token = ""),
    "Authorization token must be provided"
  )

  # Test invalid quiet parameter
  expect_error(
    download_project("SCPCP000001", auth_token = "valid-token", quiet = "not-logical"),
    "quiet must be a logical value"
  )

  # Test invalid merged parameter
  expect_error(
    download_project("SCPCP000001", auth_token = "valid-token", merged = "not-logical"),
    "merged must be a logical value"
  )

  # Test invalid include_multiplexed parameter
  expect_error(
    download_project(
      "SCPCP000001",
      auth_token = "valid-token",
      include_multiplexed = "not-logical"
    ),
    "include_multiplexed must be NULL or a logical value"
  )
})

test_that("download_project validates format and merged combinations", {
  expect_error(
    download_project("SCPCP000001", auth_token = "valid-token", format = "spatial", merged = TRUE),
    "Merged spatial files are not available"
  )
})

test_that("download_project uses the available dataset when include_multiplexed = NULL and no multiplexed data", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = FALSE),
    get_ccdl_datasets = function(...) {
      list(
        list(is_succeeded = TRUE, includes_files_multiplexed = FALSE, id = "no-multi-id")
      )
    },
    get_ccdl_dataset_detail = function(id, ...) {
      list(
        download_url = "https://example.com/no-multi-id.zip",
        download_filename = "no-multi-id.zip"
      )
    },
    download_and_extract_file = function(url, ...) unname(url)
  )
  result <- download_project("SCPCP000001", auth_token = "valid-token", format = "sce")
  expect_match(result, "no-multi-id")
})

test_that("download_project warns when include_multiplexed = TRUE but project has no multiplexed data", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = FALSE),
    get_ccdl_datasets = function(...) {
      list(
        list(is_succeeded = TRUE, includes_files_multiplexed = FALSE, id = "no-multi-id")
      )
    },
    get_ccdl_dataset_detail = function(id, ...) {
      list(
        download_url = "https://example.com/no-multi-id.zip",
        download_filename = "no-multi-id.zip"
      )
    },
    download_and_extract_file = function(url, ...) unname(url)
  )
  expect_warning(
    result <- download_project(
      "SCPCP000001",
      auth_token = "valid-token",
      format = "sce",
      include_multiplexed = TRUE
    ),
    "Multiplexed data not available"
  )
  expect_match(result, "no-multi-id")
})

test_that("download_project uses multiplexed dataset when include_multiplexed = NULL and project has multiplexed data", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = TRUE),
    get_ccdl_datasets = function(...) {
      list(
        list(is_succeeded = TRUE, includes_files_multiplexed = TRUE, id = "multi-id")
      )
    },
    get_ccdl_dataset_detail = function(id, ...) {
      list(
        download_url = "https://example.com/multi-id.zip",
        download_filename = "multi-id.zip"
      )
    },
    download_and_extract_file = function(url, ...) unname(url)
  )
  result <- download_project("SCPCP000001", auth_token = "valid-token", format = "sce")
  expect_match(result, "multi-id")
})

test_that("download_project uses non-multiplexed dataset when include_multiplexed = FALSE and project has multiplexed data", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = TRUE),
    get_ccdl_datasets = function(...) {
      list(
        list(is_succeeded = TRUE, includes_files_multiplexed = FALSE, id = "no-multi-id")
      )
    },
    get_ccdl_dataset_detail = function(id, ...) {
      list(
        download_url = "https://example.com/no-multi-id.zip",
        download_filename = "no-multi-id.zip"
      )
    },
    download_and_extract_file = function(url, ...) unname(url)
  )
  result <- download_project(
    "SCPCP000001",
    auth_token = "valid-token",
    format = "sce",
    include_multiplexed = FALSE
  )
  expect_match(result, "no-multi-id")
})

test_that("download_project errors when unexpected number of datasets returned", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = FALSE),
    get_ccdl_datasets = function(...) {
      list(
        list(is_succeeded = TRUE, includes_files_multiplexed = FALSE, id = "id-1"),
        list(is_succeeded = TRUE, includes_files_multiplexed = FALSE, id = "id-2")
      )
    }
  )
  expect_error(
    download_project("SCPCP000001", auth_token = "valid-token", format = "sce"),
    "Multiple pre-built datasets found"
  )
})

test_that("download_project downloads when a matching CCDL dataset exists", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = TRUE),
    get_ccdl_datasets = function(...) {
      list(
        list(is_succeeded = TRUE, includes_files_multiplexed = TRUE, id = "abc123")
      )
    },
    get_ccdl_dataset_detail = function(id, ...) {
      list(
        download_url = "https://example.com/SCPCP000001_SCE.zip",
        download_filename = "SCPCP000001_SCE.zip"
      )
    },
    download_and_extract_file = function(url, ...) c("path/to/file1.rds", "path/to/file2.rds")
  )
  result <- download_project("SCPCP000001", auth_token = "valid-token", format = "sce")
  expect_equal(result, c("path/to/file1.rds", "path/to/file2.rds"))
})

test_that("download_project errors when no CCDL dataset is found", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = FALSE),
    get_ccdl_datasets = function(...) list()
  )
  expect_error(
    download_project("SCPCP000001", auth_token = "valid-token", format = "sce"),
    "SCPCP000001"
  )
})

test_that("download_project error mentions relevant options when none found", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = TRUE),
    get_ccdl_datasets = function(...) list()
  )
  expect_error(
    download_project("SCPCP000001", auth_token = "valid-token", format = "sce", merged = TRUE),
    "merged = TRUE"
  )
  expect_error(
    download_project(
      "SCPCP000001",
      auth_token = "valid-token",
      format = "sce",
      include_multiplexed = FALSE
    ),
    "include_multiplexed = FALSE"
  )
  expect_error(
    download_project(
      "SCPCP000001",
      auth_token = "valid-token",
      format = "sce",
      include_multiplexed = TRUE
    ),
    "include_multiplexed = TRUE"
  )
})

test_that("download_project errors when no dataset has is_succeeded = TRUE", {
  local_mocked_bindings(
    get_project_info = function(...) list(has_multiplexed_data = FALSE),
    get_ccdl_datasets = function(...) {
      list(
        list(
          is_succeeded = FALSE,
          includes_files_multiplexed = FALSE,
          download_url = "https://example.com/x.zip"
        )
      )
    }
  )
  expect_error(
    download_project("SCPCP000001", auth_token = "valid-token", format = "sce"),
    "No pre-built dataset found"
  )
})


test_that("download_and_extract_file respects overwrite parameter", {
  # Create a temporary directory for testing
  temp_dir <- file.path(tempdir(), "test_overwrite")
  test_dest_dir <- file.path(temp_dir, "test_file")

  # Create the destination directory to simulate existing files
  dir.create(test_dest_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(test_dest_dir, "existing_file.txt"))
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Mock parse_download_file to return consistent filename
  local_mocked_bindings(
    parse_download_file = function(url) "test_file.zip"
  )

  # Test with overwrite = FALSE (should skip and return existing directory contents)
  expect_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
      temp_dir,
      overwrite = FALSE,
      quiet = TRUE
    )
  )

  expect_equal(result, file.path(test_dest_dir, c("existing_file.txt")))
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
    parse_download_file = function(url) basename(zip_file),
    req_perform = function(req, path = NULL, ...) {
      file.copy(zip_file, path)
      invisible(NULL)
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

  # Mock parse_download_file to return a filename with newer date but same prefix
  local_mocked_bindings(
    parse_download_file = function(url) "SCPCS000001_2024-02-20.zip"
  )

  # Test with redownload = FALSE (default behavior)
  expect_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
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

  # Mock parse_download_file to return a filename with same prefix
  local_mocked_bindings(
    parse_download_file = function(url) "SCPCS000001_2025-04-01.zip"
  )

  # Test with redownload = FALSE
  expect_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
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
    parse_download_file = function(url) glue::glue("{new_name}.zip"),
    req_perform = function(req, path = NULL, ...) {
      file.copy(zip_file, path)
      invisible(NULL)
    }
  )

  # Test with redownload = TRUE - should proceed with download
  expect_no_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
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
    parse_download_file = function(url) glue::glue("{dir_name}.zip"),
    req_perform = function(req, path = NULL, ...) {
      file.copy(zip_file, path)
      invisible(NULL)
    }
  )

  # Test with overwrite=TRUE and redownload=FALSE when exact destination exists
  # This should proceed with download because the exact destination exists AND overwrite=TRUE
  expect_no_message(
    result <- download_and_extract_file(
      "https://example.com/test.zip",
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

# download_and_extract_file unzip = FALSE tests

test_that("download_and_extract_file with unzip = FALSE saves the zip and returns its path", {
  temp_dir <- file.path(tempdir(), "test_no_unzip")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      writeLines("fake zip", path)
      invisible(NULL)
    }
  )

  result <- download_and_extract_file(
    setNames("https://example.com/dataset.zip", "dataset.zip"),
    temp_dir,
    overwrite = FALSE,
    redownload = FALSE,
    quiet = TRUE,
    unzip = FALSE
  )

  expect_equal(result, file.path(temp_dir, "dataset.zip"))
  expect_true(file.exists(result))
  expect_false(dir.exists(file.path(temp_dir, "dataset")))
})

test_that("download_and_extract_file with unzip = FALSE skips when file exists and overwrite = FALSE", {
  temp_dir <- file.path(tempdir(), "test_no_unzip_skip")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  zip_path <- file.path(temp_dir, "dataset.zip")
  writeLines("existing", zip_path)

  expect_message(
    result <- download_and_extract_file(
      setNames("https://example.com/dataset.zip", "dataset.zip"),
      temp_dir,
      overwrite = FALSE,
      redownload = FALSE,
      quiet = TRUE,
      unzip = FALSE
    ),
    "already exists"
  )
  expect_equal(result, zip_path)
  # file should not have been overwritten
  expect_equal(readLines(zip_path), "existing")
})

# download_dataset tests

test_that("download_dataset downloads a succeeded dataset", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        is_succeeded = TRUE,
        download_url = "https://example.com/dataset.zip",
        download_filename = "dataset.zip"
      )
    },
    download_and_extract_file = \(url, ...) c("scpca_data/dataset/file.rds")
  )

  result <- download_dataset(
    "00000000-0000-0000-0000-000000000001",
    auth_token = "token"
  )
  expect_equal(result, c("scpca_data/dataset/file.rds"))
})


test_that("download_dataset errors when dataset is not succeeded", {
  for (detail in list(
    list(is_started = FALSE),
    list(is_started = TRUE),
    list(is_started = TRUE, is_failed = TRUE)
  )) {
    local_mocked_bindings(
      get_dataset_detail = \(dataset, auth_token) detail
    )
    expect_error(
      download_dataset("00000000-0000-0000-0000-000000000001", auth_token = "token"),
      "not ready for download"
    )
  }
})

test_that("download_dataset error names the current status", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) list(is_started = TRUE)
  )
  expect_error(
    download_dataset("00000000-0000-0000-0000-000000000001", auth_token = "token"),
    "processing"
  )
})

test_that("download_dataset errors when dataset is expired", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) list(is_succeeded = TRUE, is_expired = TRUE)
  )
  expect_error(
    download_dataset("00000000-0000-0000-0000-000000000001", auth_token = "token"),
    "expired"
  )
})

test_that("download_dataset passes unzip = FALSE to download_and_extract_file", {
  captured_unzip <- NULL
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        is_succeeded = TRUE,
        download_url = "https://example.com/dataset.zip",
        download_filename = "dataset.zip"
      )
    },
    download_and_extract_file = \(url, parent_dir, overwrite, redownload, quiet, unzip = TRUE) {
      captured_unzip <<- unzip
      "scpca_data/dataset.zip"
    }
  )

  download_dataset("00000000-0000-0000-0000-000000000001", unzip = FALSE, auth_token = "token")
  expect_false(captured_unzip)
})

test_that("download_dataset errors when auth_token is empty", {
  expect_error(
    download_dataset("00000000-0000-0000-0000-000000000001", auth_token = ""),
    "Authorization token must be provided"
  )
})

# wait_and_download_dataset tests

test_that("wait_and_download_dataset downloads a dataset that is already succeeded", {
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "succeeded",
    download_dataset = \(dataset, ...) c("scpca_data/dataset/file.rds")
  )

  result <- wait_and_download_dataset(
    "00000000-0000-0000-0000-000000000001",
    quiet = TRUE,
    poll_interval = 0,
    auth_token = "token"
  )
  expect_equal(result, c("scpca_data/dataset/file.rds"))
})

test_that("wait_and_download_dataset polls until succeeded", {
  call_count <- 0L
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) {
      call_count <<- call_count + 1L
      if (call_count < 3L) "processing" else "succeeded"
    },
    download_dataset = \(dataset, ...) c("scpca_data/dataset/file.rds")
  )

  result <- wait_and_download_dataset(
    "00000000-0000-0000-0000-000000000001",
    quiet = TRUE,
    poll_interval = 0,
    auth_token = "token"
  )
  expect_equal(result, c("scpca_data/dataset/file.rds"))
  expect_gte(call_count, 3L)
})


test_that("wait_and_download_dataset errors when dataset fails", {
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "failed"
  )

  expect_error(
    wait_and_download_dataset(
      "00000000-0000-0000-0000-000000000001",
      quiet = TRUE,
      poll_interval = 0,
      auth_token = "token"
    ),
    "processing failed"
  )
})

test_that("wait_and_download_dataset restarts processing when dataset is expired", {
  started <- FALSE
  call_count <- 0L
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) {
      call_count <<- call_count + 1L
      if (call_count == 1L) "expired" else "succeeded"
    },
    start_dataset_processing = \(dataset, email = NULL, auth_token) {
      started <<- TRUE
      invisible(list())
    },
    download_dataset = \(dataset, ...) c("scpca_data/dataset/file.rds")
  )

  result <- wait_and_download_dataset(
    "00000000-0000-0000-0000-000000000001",
    quiet = TRUE,
    poll_interval = 0,
    auth_token = "token"
  )
  expect_true(started)
  expect_equal(result, c("scpca_data/dataset/file.rds"))
})

test_that("wait_and_download_dataset errors with unexpected message if expired during polling", {
  call_count <- 0L
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) {
      call_count <<- call_count + 1L
      if (call_count == 1L) "processing" else "expired"
    },
    start_dataset_processing = \(dataset, email = NULL, auth_token) invisible(list())
  )

  expect_error(
    wait_and_download_dataset(
      "00000000-0000-0000-0000-000000000001",
      quiet = TRUE,
      poll_interval = 0,
      auth_token = "token"
    ),
    "unexpectedly expired"
  )
})

test_that("wait_and_download_dataset errors on timeout", {
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "processing"
  )

  expect_error(
    wait_and_download_dataset(
      "00000000-0000-0000-0000-000000000001",
      quiet = TRUE,
      poll_interval = 0,
      timeout = 0,
      auth_token = "token"
    ),
    "Timed out"
  )
})

test_that("wait_and_download_dataset errors when auth_token is empty", {
  expect_error(
    wait_and_download_dataset("00000000-0000-0000-0000-000000000001", auth_token = ""),
    "Authorization token must be provided"
  )
})
