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

test_that("scpca_request handles custom parameters", {
  # Test with custom limit and offset
  req <- scpca_request("samples", limit = 50, offset = 10)

  # Check URL includes query parameters
  expect_match(req$url, "limit=50")
  expect_match(req$url, "offset=10")
  expect_match(req$url, "samples")
})
