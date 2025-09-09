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
