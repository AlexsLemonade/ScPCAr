# tests for R/api.R

test_that("scpca_request creates correct httr2 request object", {
  # Test basic request creation
  req <- scpca_request("projects")

  # Check that it returns an httr2 request object
  expect_s3_class(req, "httr2_request")

  # Check the URL is constructed correctly
  expect_match(req$url, "https://api.scpca.alexslemonade.org/v1/projects")

  # Check default parameters are applied
  expect_equal(req$options$useragent, "ScPCA R API Client")
})

test_that("scpca_request handles empty body correctly", {
  # Test with empty body
  req <- scpca_request("projects", body = list())

  # Check that no body is included in the request
  expect_null(req$body)
})

test_that("scpca_request handles non-empty body correctly", {
  # Test with non-empty body
  body_content <- list(name = "Test Project", description = "A test project")
  req <- scpca_request("projects", body = body_content)

  # Check that body is included in the request
  expect_equal(req$body$data, body_content)
})

test_that("scpca_request handles custom parameters", {
  # Test with custom limit and offset
  req <- scpca_request("samples", limit = 50, offset = 10)

  # Check URL includes query parameters
  expect_match(req$url, "limit=50")
  expect_match(req$url, "offset=10")
  expect_match(req$url, "samples")
})

test_that("scpca_request handles authentication token", {
  # Test with authentication token
  req <- scpca_request("projects", auth_token = "testtoken123")

  expect_equal(req_get_headers(req, "reveal")$`api-key`, "testtoken123")
})

test_that("scpca_request without authentication token behaves as expected", {
  # Test with authentication token
  req <- scpca_request("projects", auth_token = "")

  expect_null(req_get_headers(req, "reveal")$`api-key`)
})

test_that("check_api works as expected", {
  with_mock_dir("check_api", {
    expect_true(check_api())
  })

  without_internet(
    expect_error(check_api())
  )
})

# scpca_perform and with_scpca_errors tests

test_that("scpca_perform passes a successful response through unchanged", {
  local_mocked_bindings(
    req_perform = \(req, ...) json_response(list(id = "abc123"))
  )
  req <- scpca_request("datasets")
  resp <- scpca_perform(req)
  expect_s3_class(resp, "httr2_response")
  expect_equal(resp_body_json(resp)$id, "abc123")
})

test_that("scpca_perform surfaces a generic 403 message by default", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_403", message = "Forbidden")
  )
  expect_error(
    scpca_perform(scpca_request("datasets")),
    "Authorization failed"
  )
})

test_that("scpca_perform uses a custom forbidden_msg for 403", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_403", message = "Forbidden")
  )
  expect_error(
    scpca_perform(scpca_request("datasets"), forbidden_msg = "custom 403 message"),
    "custom 403 message"
  )
})

test_that("scpca_perform surfaces a not_found_msg on 404 when API is reachable", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_404", message = "Not Found"),
    check_api = function() TRUE
  )
  expect_error(
    scpca_perform(scpca_request("projects/SCPCP999999"), not_found_msg = "Project not found."),
    "Project not found."
  )
})

test_that("scpca_perform propagates the API-down error from check_api on 404", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_404", message = "Not Found"),
    check_api = function() stop("The API may be down or unreachable.")
  )
  expect_error(
    scpca_perform(scpca_request("projects/SCPCP999999")),
    "API may be down"
  )
})

test_that("scpca_perform surfaces a generic 404 message when not_found_msg is NULL", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_404", message = "Not Found"),
    check_api = function() TRUE
  )
  expect_error(
    scpca_perform(scpca_request("projects/SCPCP999999")),
    "not found"
  )
})

test_that("scpca_perform surfaces a conflict_msg on 409", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_409", message = "Conflict")
  )
  expect_error(
    scpca_perform(scpca_request("datasets"), conflict_msg = "custom conflict"),
    "custom conflict"
  )
})

test_that("scpca_perform surfaces a generic 409 message when conflict_msg is NULL", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_409", message = "Conflict")
  )
  expect_error(
    scpca_perform(scpca_request("datasets")),
    "locked"
  )
})

test_that("scpca_perform lets unhandled statuses (e.g. 400) propagate unchanged", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_400", message = "Bad Request")
  )
  # 400 should propagate as httr2_http_400, not be swallowed by the wrapper
  expect_error(
    scpca_perform(scpca_request("tokens")),
    class = "httr2_http_400"
  )
})
