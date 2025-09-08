with_mock_dir("scpca_projects", {
  # uses recorded API responses from httptest2
  test_that("scpca_projects returns simplified data frame by default", {
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

with_mock_dir("get_project_samples", {
  test_that("get_project_samples returns simplified data frame by default", {
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
