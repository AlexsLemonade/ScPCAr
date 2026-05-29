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
  withr::local_envvar(SCPCA_AUTH_TOKEN = "")
  # Mock httr2 functions to avoid actual API calls
  # not using httptest2::with_mock_* because we just want to test validation here
  local_mocked_bindings(
    req_perform = \(req) json_response(list(id = "mock-token-123"))
  )
  # These should all pass validation
  expect_no_error(get_auth("user@example.com", TRUE))
  expect_no_error(get_auth("test.user+tag@subdomain.example.org", TRUE))
  expect_no_error(get_auth("user123@company-name.co.uk", TRUE))
})

test_that("get_auth gets a token", {
  withr::local_envvar(SCPCA_AUTH_TOKEN = "")
  with_mock_dir("auth", {
    result <- get_auth("scpca@alexslemonade.org", TRUE)

    # Check that the function returns the token ID
    expect_equal(result, "mock-token-123")
  })
})

test_that("get_auth stores the token in the SCPCA_AUTH_TOKEN environment variable", {
  withr::local_envvar(SCPCA_AUTH_TOKEN = "")
  with_mock_dir("auth", {
    get_auth("scpca@alexslemonade.org", TRUE)

    # the token should have been written to the environment
    expect_equal(Sys.getenv("SCPCA_AUTH_TOKEN"), "mock-token-123")
  })
})

test_that("resolve_auth_token falls back to the environment and validates", {
  # an explicit token is returned unchanged
  expect_equal(resolve_auth_token("explicit-token"), "explicit-token")

  # falls back to the environment variable when no argument is supplied
  withr::local_envvar(SCPCA_AUTH_TOKEN = "env-token")
  expect_equal(resolve_auth_token(), "env-token")

  # an explicit token still overrides the environment value
  expect_equal(resolve_auth_token("explicit-token"), "explicit-token")

  # errors when neither an argument nor the environment provides a token
  withr::local_envvar(SCPCA_AUTH_TOKEN = "")
  expect_error(resolve_auth_token(), "Authorization token must be provided")
  expect_error(resolve_auth_token(""), "Authorization token must be provided")
})
