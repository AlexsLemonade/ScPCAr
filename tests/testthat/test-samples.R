# Tests for samples.R functions using httptest2

library(httptest2)

test_that("get_sample_info validates sample_id format", {
  # Test invalid sample IDs
  expect_error(
    get_sample_info("invalid"),
    "Invalid sample_id"
  )

  expect_error(
    get_sample_info("SCPCS00001"), # too few digits
    "Invalid sample_id"
  )

  expect_error(
    get_sample_info("SCPCS0000001"), # too many digits
    "Invalid sample_id"
  )

  expect_error(
    get_sample_info("SCPCL000001"), # wrong prefix
    "Invalid sample_id"
  )
})

test_that("get_sample_info works with valid sample_id", {
  # Mock the API response for a valid sample
  with_mock_dir("sample_info", {
    result <- get_sample_info("SCPCS000001")

    # Verify the result structure
    expect_type(result, "list")
    expect_equal(result$scpca_id, "SCPCS000001")
    expect_contains(
      names(result),
      c("computed_files", "project", "additional_metadata")
    )
  })
})

test_that("get_sample_info handles 404 errors correctly", {
  # Mock check_api to return TRUE (API is reachable)
  local_mocked_bindings(
    check_api = function() TRUE
  )

  with_mock_dir("sample_info_404", {
    expect_error(
      get_sample_info("SCPCS999999"),
      "Sample `SCPCS999999` not found."
    )
  })
})
