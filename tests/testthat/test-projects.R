# uses recorded API responses from httptest2
test_that("scpca_projects returns simplified data frame by default", {
  with_mock_dir("scpca_projects", {
    projects_df <- scpca_projects()

    # Check that it returns a data frame
    expect_s3_class(projects_df, "data.frame")

    # Check that we have rows and columns
    expect_gt(nrow(projects_df), 0)
    expect_gt(ncol(projects_df), 0)

    # Check that list columns are removed (simplified)
    list_columns <- sapply(projects_df, is.list)
    expect_false(any(list_columns))

    # Check for expected renamed columns
    expect_contains(colnames(projects_df), "scpca_project_id")

    # Check that date columns are properly converted
    expect_s3_class(projects_df$created_at, "POSIXct")
    expect_s3_class(projects_df$updated_at, "POSIXct")
  })

  test_that("scpca_projects returns full data frame when simplify = FALSE", {
    projects_df <- scpca_projects(simplify = FALSE)

    # Check that it returns a data frame
    expect_s3_class(projects_df, "data.frame")

    # Check that we have rows and columns
    expect_gt(nrow(projects_df), 0)
    expect_gt(ncol(projects_df), 0)

    # Check that list columns are present (not simplified)
    list_columns <- sapply(projects_df, is.list)
    expect_true(any(list_columns))

    # Check for expected key columns
    expect_contains(colnames(projects_df), "scpca_project_id")

    # Check that date columns are properly converted
    expect_s3_class(projects_df$created_at, "POSIXct")
    expect_s3_class(projects_df$updated_at, "POSIXct")
  })
})

test_that("get_project_info validates project_id format", {
  # Test invalid project IDs
  expect_error(
    get_project_info("invalid"),
    "Invalid project_id"
  )

  expect_error(
    get_project_info("SCPCP00001"), # too few digits
    "Invalid project_id"
  )

  expect_error(
    get_project_info("SCPCP0000001"), # too many digits
    "Invalid project_id"
  )

  expect_error(
    get_project_info("SCPCS000001"), # wrong prefix
    "Invalid project_id"
  )
})


test_that("get_project_info works with valid project_id", {
  # Mock the API response for a valid project
  with_mock_dir("project_info", {
    result <- get_project_info("SCPCP000001")

    # Verify the result structure
    expect_type(result, "list")
    expect_equal(result$scpca_id, "SCPCP000001")
    expect_contains(
      names(result),
      c("computed_files", "diagnoses", "abstract", "title", "additional_metadata_keys")
    )
  })
})

test_that("get_project_info handles 404 errors correctly", {
  # Mock check_api to return TRUE (API is reachable)
  local_mocked_bindings(
    check_api = function() TRUE
  )

  with_mock_dir("project_info_404", {
    expect_error(
      get_project_info("SCPCP999999"),
      "Project `SCPCP999999` not found."
    )
  })
})

test_that("get_project_samples returns simplified data frame by default", {
  with_mock_dir("project_info", {
    samples_df <- get_project_samples("SCPCP000001")

    # Check that it returns a data frame
    expect_s3_class(samples_df, "data.frame")

    # Check that we have rows and columns
    expect_gt(nrow(samples_df), 0)
    expect_gt(ncol(samples_df), 0)

    # Check that list columns are removed (simplified)
    list_columns <- sapply(samples_df, is.list)
    expect_false(any(list_columns))

    # Check for expected renamed columns
    expect_contains(colnames(samples_df), c("scpca_sample_id", "scpca_project_id"))

    # Check for (some) expanded additional_metadata columns
    expect_contains(colnames(samples_df), c("disease_ontology_term_id", "organism"))

    # Check that date columns are properly converted
    expect_s3_class(samples_df$created_at, "POSIXct")
    expect_s3_class(samples_df$updated_at, "POSIXct")
  })
})


test_that("get_project_metadata_url works as expected", {
  with_mock_dir("project_info", {
    token <- "mock-token-123"
    download_url <- get_project_metadata_url("SCPCP000001", token)

    # Verify the download URL format
    expect_type(download_url, "character")
    expect_true(grepl("^https://", download_url))
    expect_true(grepl("SCPCP000001", download_url))
  })
})

test_that("get_project_libraries reads data correctly", {
  # Mock the network functions manually
  local_mocked_bindings(
    get_project_metadata_url = function(project_id, auth_token) {
      "https://example.com/mock_metadata.zip"
    },
    download_and_extract_file = function(url, dest, overwrite = TRUE, quiet = TRUE) {
      # return paths to files in the testthat/project_metadata/
      list.files("project_metadata/SCPCP000001_ALL_METADATA", full.names = TRUE)
    }
  )
  libraries_df <- get_project_libraries("SCPCP000001", "mock-token-123")

  # Check that it returns a data frame
  expect_s3_class(libraries_df, "data.frame")
  # Check that we have rows and columns
  expect_gt(nrow(libraries_df), 0)
  expect_gt(ncol(libraries_df), 0)
  # Check for expected columns
  expect_contains(
    colnames(libraries_df),
    c("scpca_project_id", "scpca_sample_id", "scpca_library_id", "seq_unit", "technology")
  )

  # Check that logical columns are properly converted
  is_cols <- stringr::str_subset(colnames(libraries_df), "^(is|has|includes)_")
  expect_true(all(sapply(libraries_df[is_cols], is.logical)))

  # Check that numeric columns are properly converted
  numeric_cols <- c(
    "age",
    "total_reads",
    "mapped_reads",
    "unfiltered_cells",
    "filtered_cell_count",
    "processed_cells",
    "prob_compromised_cutoff",
    "min_gene_cutoff"
  )
  expect_true(all(sapply(libraries_df[numeric_cols], is.numeric)))

  # Check that date columns are properly converted
  expect_s3_class(libraries_df$date_processed, "POSIXct")
})
