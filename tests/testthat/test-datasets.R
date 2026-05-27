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

test_that("get_ccdl_datasets passes format as ccdl_format query parameter", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(format = "ANN_DATA")
  expect_match(captured_req$url, "ccdl_format=ANN_DATA")
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

# build_dataset_data tests

test_that("build_dataset_data builds correct structure for single-cell samples", {
  local_mocked_bindings(
    get_sample_info = \(sample_id, ...) list(
      scpca_id = sample_id,
      project = list(scpca_id = "SCPCP000001"),
      has_single_cell_data = TRUE,
      has_spatial_data = FALSE
    )
  )
  result <- build_dataset_data(samples = c("SCPCS000001", "SCPCS000002"))

  expect_equal(names(result), "SCPCP000001")
  expect_equal(result$SCPCP000001$SINGLE_CELL, list("SCPCS000001", "SCPCS000002"))
  expect_equal(result$SCPCP000001$SPATIAL, list())
  expect_false(result$SCPCP000001$includes_bulk)
})

test_that("build_dataset_data places spatial samples in SPATIAL list", {
  local_mocked_bindings(
    get_sample_info = \(sample_id, ...) list(
      scpca_id = sample_id,
      project = list(scpca_id = "SCPCP000001"),
      has_single_cell_data = FALSE,
      has_spatial_data = TRUE
    )
  )
  result <- build_dataset_data(samples = "SCPCS000003")

  expect_equal(result$SCPCP000001$SINGLE_CELL, list())
  expect_equal(result$SCPCP000001$SPATIAL, list("SCPCS000003"))
})

test_that("build_dataset_data groups samples by project", {
  local_mocked_bindings(
    get_sample_info = \(sample_id, ...) {
      project_id <- if (sample_id == "SCPCS000001") "SCPCP000001" else "SCPCP000002"
      list(
        scpca_id = sample_id,
        project = list(scpca_id = project_id),
        has_single_cell_data = TRUE,
        has_spatial_data = FALSE
      )
    }
  )
  result <- build_dataset_data(samples = c("SCPCS000001", "SCPCS000004"))

  expect_setequal(names(result), c("SCPCP000001", "SCPCP000002"))
})

test_that("build_dataset_data respects include_bulk parameter", {
  local_mocked_bindings(
    get_sample_info = \(sample_id, ...) list(
      scpca_id = sample_id,
      project = list(scpca_id = "SCPCP000001"),
      has_single_cell_data = TRUE,
      has_spatial_data = FALSE
    )
  )
  result <- build_dataset_data(samples = "SCPCS000001", include_bulk = TRUE)

  expect_true(result$SCPCP000001$includes_bulk)
})

# create_dataset tests

test_that("create_dataset errors when neither samples nor projects are provided", {
  expect_error(
    create_dataset(format = "sce", auth_token = "token"),
    "At least one of 'samples' or 'projects' must be provided"
  )
})

test_that("create_dataset errors when auth_token is empty", {
  expect_error(
    create_dataset(samples = "SCPCS000001", format = "sce", auth_token = ""),
    "Authorization token must be provided"
  )
})

test_that("create_dataset errors on invalid format", {
  expect_error(
    create_dataset(samples = "SCPCS000001", format = "invalid", auth_token = "token"),
    "Invalid format"
  )
})

test_that("create_dataset errors when spatial format is requested", {
  expect_error(
    create_dataset(samples = "SCPCS000001", format = "spatial", auth_token = "token"),
    "Space Ranger format"
  )
})

test_that("create_dataset POSTs with start = FALSE", {
  local_mocked_bindings(
    build_dataset_data = \(...) list(
      SCPCP000001 = list(SINGLE_CELL = list("SCPCS000001"), SPATIAL = list(), includes_bulk = FALSE)
    ),
    req_perform = \(req, ...) {
      body <- req$body$data
      json_response(c(body, list(id = "new-dataset-uuid")))
    }
  )

  result <- NULL
  expect_message(
    {result <- create_dataset(samples = "SCPCS000001", format = "sce", auth_token = "token")},
    "new-dataset-uuid"
  )
  expect_false(result$start)
})

test_that("create_dataset returns response invisibly and messages with dataset id", {
  local_mocked_bindings(
    build_dataset_data = \(...) list(
      SCPCP000001 = list(SINGLE_CELL = list("SCPCS000001"), SPATIAL = list(), includes_bulk = FALSE)
    ),
    scpca_request = \(...) httr2::request("https://api.scpca.alexslemonade.org"),
    req_perform = \(req, ...) json_response(
      list(id = "new-dataset-uuid", format = "ANN_DATA", data = list(), start = FALSE)
    )
  )

  result <- NULL
  expect_message(
    {result <- create_dataset(samples = "SCPCS000001", format = "anndata", auth_token = "token")},
    "new-dataset-uuid"
  )
  expect_equal(result$id, "new-dataset-uuid")
})

# get_dataset_status tests

test_that("get_dataset_status returns dataset with data and status fields", {
  with_mock_dir("get_dataset_status", {
    result <- get_dataset_status("test-dataset-uuid", auth_token = "test-token")

    expect_type(result, "list")
    expect_equal(result$id, "test-dataset-uuid")
    expect_equal(result$format, "SINGLE_CELL_EXPERIMENT")
    expect_false(result$is_started)
    expect_false(result$is_succeeded)
  })
})

test_that("get_dataset_status returns data field with project and sample structure", {
  with_mock_dir("get_dataset_status", {
    result <- get_dataset_status("test-dataset-uuid", auth_token = "test-token")

    expect_type(result$data, "list")
    expect_true("SCPCP000001" %in% names(result$data))
    expect_contains(
      result$data$SCPCP000001$SINGLE_CELL,
      c("SCPCS000001", "SCPCS000002")
    )
  })
})

test_that("get_dataset_status includes api-key header when auth_token is provided", {
  local_mocked_bindings(
    req_perform = \(req, ...) {
      json_response(list(
        id = "uuid",
        data = list(),
        api_key = httr2::req_get_headers(req, "reveal")$`api-key`
      ))
    }
  )

  result <- get_dataset_status("uuid", auth_token = "my-token")
  expect_equal(result$api_key, "my-token")
})

# update_dataset tests

test_that("update_dataset errors when no add/remove params provided", {
  expect_error(
    update_dataset("uuid", auth_token = "token"),
    "At least one of"
  )
})

test_that("update_dataset errors when auth_token is empty", {
  expect_error(
    update_dataset("uuid", add_samples = "SCPCS000001", auth_token = ""),
    "Authorization token must be provided"
  )
})

test_that("update_dataset merges new samples into existing data", {
  current_data <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001"),
      SPATIAL = list(),
      includes_bulk = FALSE
    )
  )

  local_mocked_bindings(
    get_dataset_status = \(...) list(data = current_data),
    build_dataset_data = \(...) list(
      SCPCP000001 = list(SINGLE_CELL = list("SCPCS000002"), SPATIAL = list(), includes_bulk = FALSE)
    ),
    req_perform = \(req, ...) {
      json_response(req$body$data)
    }
  )

  result <- update_dataset("uuid", add_samples = "SCPCS000002", auth_token = "token")
  merged_sc <- as.character(unlist(result$data$SCPCP000001$SINGLE_CELL))
  expect_setequal(merged_sc, c("SCPCS000001", "SCPCS000002"))
})

test_that("update_dataset removes specified samples", {
  current_data <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001", "SCPCS000002"),
      SPATIAL = list(),
      includes_bulk = FALSE
    )
  )

  local_mocked_bindings(
    get_dataset_status = \(...) list(data = current_data),
    req_perform = \(req, ...) {
      json_response(req$body$data)
    }
  )

  result <- update_dataset("uuid", remove_samples = "SCPCS000001", auth_token = "token")
  remaining_sc <- as.character(unlist(result$data$SCPCP000001$SINGLE_CELL))
  expect_equal(remaining_sc, "SCPCS000002")
})

test_that("update_dataset drops projects with no remaining samples after removal", {
  current_data <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001"),
      SPATIAL = list(),
      includes_bulk = FALSE
    )
  )

  local_mocked_bindings(
    get_dataset_status = \(...) list(data = current_data),
    req_perform = \(req, ...) {
      json_response(req$body$data)
    }
  )

  result <- update_dataset("uuid", remove_samples = "SCPCS000001", auth_token = "token")
  expect_null(result$data$SCPCP000001)
})

test_that("update_dataset removes entire project when remove_projects is specified", {
  current_data <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001"),
      SPATIAL = list(),
      includes_bulk = FALSE
    ),
    SCPCP000002 = list(
      SINGLE_CELL = list("SCPCS000004"),
      SPATIAL = list(),
      includes_bulk = FALSE
    )
  )

  local_mocked_bindings(
    get_dataset_status = \(...) list(data = current_data),
    req_perform = \(req, ...) {
      json_response(req$body$data)
    }
  )

  result <- update_dataset("uuid", remove_projects = "SCPCP000001", auth_token = "token")
  expect_null(result$data$SCPCP000001)
  expect_false(is.null(result$data$SCPCP000002))
})

test_that("update_dataset updates include_bulk for all projects", {
  current_data <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001"),
      SPATIAL = list(),
      includes_bulk = FALSE
    )
  )

  local_mocked_bindings(
    get_dataset_status = \(...) list(data = current_data),
    req_perform = \(req, ...) {
      json_response(req$body$data)
    }
  )

  result <- update_dataset("uuid", include_bulk = TRUE, auth_token = "token")
  expect_true(result$data$SCPCP000001$includes_bulk)
})

test_that("update_dataset uses PATCH method", {
  local_mocked_bindings(
    get_dataset_status = \(...) list(data = list(
      SCPCP000001 = list(SINGLE_CELL = list("SCPCS000001"), SPATIAL = list(), includes_bulk = FALSE)
    )),
    req_perform = \(req, ...) json_response(list(method = req$method))
  )

  result <- update_dataset("uuid", remove_samples = "SCPCS000001", auth_token = "token")
  expect_equal(result$method, "PATCH")
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
