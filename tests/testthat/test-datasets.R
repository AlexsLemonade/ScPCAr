test_that("get_ccdl_datasets returns a list of dataset objects", {
  with_mock_dir("ccdl_datasets", {
    result <- get_ccdl_datasets()

    expect_type(result, "list")
    expect_length(result, 2)
    expect_type(result[[1]], "list")
    expect_contains(names(result[[1]]), c("id", "ccdl_name", "ccdl_project_id", "ccdl_modality"))
  })
})

test_that("get_ccdl_datasets returns empty list when API returns no results", {
  with_mock_dir("ccdl_datasets_empty", {
    result <- get_ccdl_datasets()

    expect_type(result, "list")
    expect_length(result, 0)
  })
})

test_that("get_ccdl_datasets combines results across pages", {
  with_mock_dir("ccdl_paged", {
    result <- get_ccdl_datasets()

    expect_type(result, "list")
    expect_length(result, 2)
    expect_equal(result[[1]]$id, 1)
    expect_equal(result[[2]]$id, 2)
  })
})

test_that("get_ccdl_datasets passes project_id as ccdl_project_id query parameter", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(project_id = "SCPCP000001")
  expect_match(captured_req$url, "ccdl_project_id=SCPCP000001")
})

test_that("get_ccdl_datasets passes modality as ccdl_modality query parameter", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(modality = "SINGLE_CELL")
  expect_match(captured_req$url, "ccdl_modality=SINGLE_CELL")
})

test_that("get_ccdl_datasets passes format as format query parameter", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(format = "ANN_DATA")
  expect_match(captured_req$url, "format=ANN_DATA")
})

test_that("get_ccdl_datasets passes merged as ccdl_is_merged query parameter", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(merged = TRUE)
  expect_match(captured_req$url, "ccdl_is_merged=TRUE")
})

test_that("get_ccdl_datasets passes metadata_only as ccdl_name=ALL_METADATA query parameter", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(metadata_only = TRUE)
  expect_match(captured_req$url, "ccdl_name=ALL_METADATA")
})

test_that("get_ccdl_datasets includes api-key header when auth_token is provided", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(auth_token = "test-token-abc")
  expect_equal(httr2::req_get_headers(captured_req, "reveal")$`api-key`, "test-token-abc")
})

test_that("get_ccdl_datasets does not include api-key header when auth_token is empty", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets()
  expect_null(httr2::req_get_headers(captured_req, "reveal")$`api-key`)
})

test_that("get_ccdl_dataset_detail returns dataset fields including download_url", {
  with_mock_dir("ccdl_dataset_detail", {
    result <- get_ccdl_dataset_detail("abc123", auth_token = "test-token")

    expect_type(result, "list")
    expect_equal(result$id, "abc123")
    expect_equal(result$download_url, "https://example.com/SCPCP000001_SCE.zip")
    expect_equal(result$download_filename, "SCPCP000001_SCE.zip")
  })
})
