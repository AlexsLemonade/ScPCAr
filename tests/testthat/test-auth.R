test_that("get_auth validates input parameters", {
  # Test that agree parameter must be TRUE
  expect_error(
    get_auth("test@example.com", FALSE),
    "You must agree to the terms of service to get a token."
  )

  expect_error(
    get_auth("test@example.com", agree = FALSE),
    "You must agree to the terms of service to get a token."
  )

  # Test email validation - invalid emails should error
  expect_error(
    get_auth("invalid-email", TRUE),
    "Invalid email address."
  )

  expect_error(
    get_auth("@example.com", TRUE),
    "Invalid email address."
  )

  expect_error(
    get_auth("test@", TRUE),
    "Invalid email address."
  )

  expect_error(
    get_auth("test.example.com", TRUE),
    "Invalid email address."
  )
})

test_that("get_auth accepts valid email formats", {
  # Mock httr2 functions to avoid actual API calls
  # not using httptest2::with_mock_* because we just want to test validation here
  local_mocked_bindings(
    req_perform = \(req) httr2::response_json() # nolint
  )
  # These should all pass validation
  expect_no_error(get_auth("user@example.com", TRUE))
  expect_no_error(get_auth("test.user+tag@subdomain.example.org", TRUE))
  expect_no_error(get_auth("user123@company-name.co.uk", TRUE))
})

test_that("get_auth gets a token", {
  with_mock_dir("auth", {
    result <- get_auth("scpca@alexslemonade.org", TRUE)

    # Check that the function returns the token ID
    expect_equal(result, "mock-token-123")
  })
})
