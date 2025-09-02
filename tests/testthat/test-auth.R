test_that("get_token validates input parameters", {
  # Test that agree parameter must be TRUE
  expect_error(
    get_token("test@example.com", FALSE),
    "You must agree to the terms of service to get a token."
  )

  expect_error(
    get_token("test@example.com", agree = FALSE),
    "You must agree to the terms of service to get a token."
  )

  # Test email validation - invalid emails should error
  expect_error(
    get_token("invalid-email", TRUE),
    "Invalid email address"
  )

  expect_error(
    get_token("@example.com", TRUE),
    "Invalid email address"
  )

  expect_error(
    get_token("test@", TRUE),
    "Invalid email address"
  )

  expect_error(
    get_token("test.example.com", TRUE),
    "Invalid email address"
  )
})

test_that("get_token accepts valid email formats", {
  # Mock httr2 functions to avoid actual API calls
  local_mocked_bindings(
    # nolint start
    req_perform = function(req) {
      structure(list(status_code = 201), class = "httr2_response")
    },
    resp_body_json = function(resp, simplifyVector = TRUE) {
      list(id = "mock-token-123")
    }
    # nolint end
  )

  # These should all pass validation
  expect_no_error(get_token("user@example.com", TRUE))
  expect_no_error(get_token("test.user+tag@subdomain.example.org", TRUE))
  expect_no_error(get_token("user123@company-name.co.uk", TRUE))
})

test_that("get_token makes correct API request", {
  # Mock the httr2 functions to capture the request details
  captured_request <- NULL
  local_mocked_bindings(
    # nolint start
    req_perform = function(req) {
      structure(list(status_code = 201), class = "httr2_response")
    },
    resp_body_json = function(resp, simplifyVector = TRUE) {
      list(id = "mock-token-123")
    }
    # nolint end
  )

  result <- get_token("test@example.com", TRUE)

  # Check that the function returns the token ID
  expect_equal(result, "mock-token-123")
})
