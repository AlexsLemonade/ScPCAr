DATASET_ID <- "00000000-0000-0000-0000-000000000001"
DATASET_ID_404 <- "00000000-0000-0000-0000-000000000404"

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

test_that("get_ccdl_datasets passes include_multiplexed as includes_files_multiplexed query parameter", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform_iterative = function(req, ...) {
      captured_req <<- req
      list()
    }
  )

  get_ccdl_datasets(include_multiplexed = TRUE)
  expect_match(captured_req$url, "includes_files_multiplexed=TRUE")

  get_ccdl_datasets(include_multiplexed = FALSE)
  expect_match(captured_req$url, "includes_files_multiplexed=FALSE")
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
  expect_equal(
    httr2::req_get_headers(captured_req, "reveal")$`api-key`,
    "test-token-abc"
  )
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
    get_sample_info = \(sample_id, ...) {
      list(
        scpca_id = sample_id,
        project = list(scpca_id = "SCPCP000001"),
        has_single_cell_data = TRUE,
        has_spatial_data = FALSE
      )
    }
  )
  result <- build_dataset_data(samples = c("SCPCS000001", "SCPCS000002"))

  expect_equal(names(result), "SCPCP000001")
  expect_equal(result$SCPCP000001$SINGLE_CELL, list("SCPCS000001", "SCPCS000002"))
  expect_equal(result$SCPCP000001$SPATIAL, list())
  expect_false(result$SCPCP000001$includes_bulk)
})

test_that("build_dataset_data places spatial samples in SPATIAL list", {
  local_mocked_bindings(
    get_sample_info = \(sample_id, ...) {
      list(
        scpca_id = sample_id,
        project = list(scpca_id = "SCPCP000001"),
        has_single_cell_data = FALSE,
        has_spatial_data = TRUE
      )
    }
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

test_that("build_dataset_data expands project IDs to sample IDs", {
  local_mocked_bindings(
    get_project_samples = \(project_id, ...) {
      tibble::tibble(scpca_sample_id = c("SCPCS000001", "SCPCS000002"))
    },
    get_sample_info = \(sample_id, ...) {
      list(
        scpca_id = sample_id,
        project = list(scpca_id = "SCPCP000001"),
        has_single_cell_data = TRUE,
        has_spatial_data = FALSE
      )
    }
  )
  result <- build_dataset_data(projects = "SCPCP000001")

  expect_equal(names(result), "SCPCP000001")
  expect_setequal(
    as.character(result$SCPCP000001$SINGLE_CELL),
    c("SCPCS000001", "SCPCS000002")
  )
})

test_that("build_dataset_data respects include_bulk parameter", {
  local_mocked_bindings(
    get_sample_info = \(sample_id, ...) {
      list(
        scpca_id = sample_id,
        project = list(scpca_id = "SCPCP000001"),
        has_single_cell_data = TRUE,
        has_spatial_data = FALSE
      )
    }
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
  captured_req <- NULL
  local_mocked_bindings(
    build_dataset_data = \(...) {
      list(
        SCPCP000001 = list(
          SINGLE_CELL = list("SCPCS000001"),
          SPATIAL = list(),
          includes_bulk = FALSE
        )
      )
    },
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(list(id = "new-dataset-uuid"))
    }
  )

  result <- NULL
  expect_message(
    {
      result <- create_dataset(samples = "SCPCS000001", format = "sce", auth_token = "token")
    },
    "new-dataset-uuid"
  )
  expect_equal(httr2::req_get_method(captured_req), "POST")
  expect_false(captured_req$body$data$start)
  expect_equal(result, "new-dataset-uuid")
})

test_that("create_dataset returns the dataset id invisibly and messages with dataset id", {
  local_mocked_bindings(
    build_dataset_data = \(...) {
      list(
        SCPCP000001 = list(
          SINGLE_CELL = list("SCPCS000001"),
          SPATIAL = list(),
          includes_bulk = FALSE
        )
      )
    },
    scpca_request = \(...) httr2::request("https://api.scpca.alexslemonade.org"),
    req_perform = \(req, ...) {
      json_response(
        list(id = "new-dataset-uuid", format = "ANN_DATA", data = list(), start = FALSE)
      )
    }
  )

  result <- NULL
  expect_message(
    {
      result <- create_dataset(samples = "SCPCS000001", format = "anndata", auth_token = "token")
    },
    "new-dataset-uuid"
  )
  expect_equal(result, "new-dataset-uuid")
})

test_that("create_dataset reads auth_token from the SCPCA_AUTH_TOKEN environment variable", {
  withr::local_envvar(SCPCA_AUTH_TOKEN = "env-token")
  captured_req <- NULL
  local_mocked_bindings(
    build_dataset_data = \(...) list(),
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(list(id = "new-dataset-uuid"))
    }
  )

  # called without auth_token; the token should come from the environment
  result <- suppressMessages(create_dataset(samples = "SCPCS000001", format = "sce"))
  expect_equal(httr2::req_get_headers(captured_req, "reveal")$`api-key`, "env-token")
  expect_equal(result, "new-dataset-uuid")
})

# get_dataset_detail tests

test_that("get_dataset_detail returns dataset with data and status fields", {
  local_mocked_bindings(
    req_perform = \(req, ...) {
      json_response(list(
        id = DATASET_ID,
        format = "SINGLE_CELL_EXPERIMENT",
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001", "SCPCS000002"),
            SPATIAL = list(),
            includes_bulk = FALSE
          )
        ),
        is_started = FALSE,
        is_succeeded = FALSE,
        total_sample_count = 2,
        computed_file = NULL
      ))
    }
  )

  result <- get_dataset_detail(DATASET_ID, auth_token = "test-token")

  expect_type(result, "list")
  expect_equal(result$id, DATASET_ID)
  expect_equal(result$format, "SINGLE_CELL_EXPERIMENT")
  expect_false(result$is_started)
  expect_false(result$is_succeeded)
})

test_that("get_dataset_detail returns data field with project and sample structure", {
  local_mocked_bindings(
    req_perform = \(req, ...) {
      json_response(list(
        id = DATASET_ID,
        format = "SINGLE_CELL_EXPERIMENT",
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001", "SCPCS000002"),
            SPATIAL = list(),
            includes_bulk = FALSE
          )
        )
      ))
    }
  )

  result <- get_dataset_detail(DATASET_ID, auth_token = "test-token")

  expect_type(result$data, "list")
  expect_true("SCPCP000001" %in% names(result$data))
  expect_contains(
    result$data$SCPCP000001$SINGLE_CELL,
    c("SCPCS000001", "SCPCS000002")
  )
})

test_that("get_dataset_detail includes api-key header when auth_token is provided", {
  local_mocked_bindings(
    req_perform = \(req, ...) {
      json_response(list(
        id = DATASET_ID,
        data = list(),
        api_key = httr2::req_get_headers(req, "reveal")$`api-key`
      ))
    }
  )

  result <- get_dataset_detail(DATASET_ID, auth_token = "my-token")
  expect_equal(result$api_key, "my-token")
})

test_that("get_dataset_detail handles 404 errors correctly", {
  local_mocked_bindings(
    check_api = function() TRUE,
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_404", message = "Not Found")
  )

  expect_error(
    get_dataset_detail(DATASET_ID_404, auth_token = "test-token"),
    "not found"
  )
})

test_that("get_dataset_detail handles 403 errors with an authorization message", {
  # 403 should not invoke check_api() — confirm by not mocking it
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_403", message = "Forbidden")
  )

  expect_error(
    get_dataset_detail(DATASET_ID, auth_token = "bad-token"),
    "Authorization failed"
  )
})

test_that("get_ccdl_datasets handles 403 errors with an authorization message", {
  local_mocked_bindings(
    req_perform_iterative = \(req, ...) {
      rlang::abort(class = "httr2_http_403", message = "Forbidden")
    }
  )

  expect_error(
    get_ccdl_datasets(auth_token = "bad-token"),
    "Authorization failed"
  )
})

test_that("get_dataset_detail accepts a list with $id in place of a string", {
  local_mocked_bindings(
    req_perform = \(req, ...) {
      json_response(list(id = DATASET_ID, data = list()))
    }
  )

  dataset_list <- list(id = DATASET_ID, data = list())
  result <- get_dataset_detail(dataset_list, auth_token = "test-token")
  expect_equal(result$id, DATASET_ID)
})

test_that("get_dataset_detail errors when list has no $id element", {
  expect_error(
    get_dataset_detail(list(data = list()), auth_token = "test-token"),
    "dataset must be an id string or contain an \\$id element"
  )
})

test_that("get_dataset_detail errors when dataset is not a string or list", {
  expect_error(
    get_dataset_detail(123, auth_token = "test-token"),
    "dataset must be an id string or contain an \\$id element"
  )
})

# get_dataset_status tests

test_that("get_dataset_status maps detail status fields to a status string", {
  detail <- NULL
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) detail
  )

  detail <- list(is_started = FALSE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "pending")

  detail <- list(is_started = TRUE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "processing")

  detail <- list(is_processing = TRUE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "processing")

  detail <- list(is_processing = TRUE, is_started = FALSE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "processing")

  detail <- list(is_started = TRUE, is_succeeded = TRUE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "succeeded")

  detail <- list(is_started = TRUE, is_failed = TRUE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "failed")

  # a failed dataset is reported as failed even if is_succeeded is also set
  detail <- list(is_succeeded = TRUE, is_failed = TRUE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "failed")

  detail <- list(is_expired = TRUE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "expired")

  # expired takes priority over succeeded (expired datasets likely still have is_succeeded = TRUE)
  detail <- list(is_succeeded = TRUE, is_expired = TRUE)
  expect_equal(get_dataset_status(DATASET_ID, auth_token = "token"), "expired")
})

test_that("get_dataset_status passes the resolved auth_token to get_dataset_detail", {
  captured_token <- NULL
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      captured_token <<- auth_token
      list(is_started = TRUE, is_succeeded = TRUE)
    }
  )

  get_dataset_status(DATASET_ID, auth_token = "my-token")
  expect_equal(captured_token, "my-token")
})

test_that("get_dataset_status reads auth_token from the SCPCA_AUTH_TOKEN environment variable", {
  withr::local_envvar(SCPCA_AUTH_TOKEN = "env-token")
  captured_token <- NULL
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      captured_token <<- auth_token
      list(is_started = TRUE, is_succeeded = TRUE)
    }
  )

  get_dataset_status(DATASET_ID)
  expect_equal(captured_token, "env-token")
})

test_that("get_dataset_status errors when auth_token is empty", {
  expect_error(
    get_dataset_status(DATASET_ID, auth_token = ""),
    "Authorization token must be provided"
  )
})


# get_dataset_info tests

test_that("get_dataset_info builds a per-sample table from project sample data", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        id = DATASET_ID,
        format = "SINGLE_CELL_EXPERIMENT",
        is_started = FALSE,
        is_succeeded = FALSE,
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001", "SCPCS000002"),
            SPATIAL = list(),
            includes_bulk = FALSE
          ),
          SCPCP000002 = list(
            SINGLE_CELL = list("SCPCS000003"),
            SPATIAL = list("SCPCS000004"),
            includes_bulk = TRUE
          )
        ),
        total_sample_count = 4
      )
    },
    get_project_samples = \(project_id, simplify = TRUE) {
      if (project_id == "SCPCP000001") {
        # SCPCS000099 belongs to the project but is not in the dataset request
        tibble::tibble(
          scpca_sample_id = c("SCPCS000001", "SCPCS000002", "SCPCS000099"),
          scpca_project_id = project_id,
          has_single_cell_data = TRUE,
          has_spatial_data = FALSE,
          has_bulk_rna_seq = FALSE,
          has_cite_seq_data = FALSE,
          has_multiplexed_data = FALSE,
          seq_units = list("cell", "cell", "cell")
        )
      } else {
        tibble::tibble(
          scpca_sample_id = c("SCPCS000003", "SCPCS000004"),
          scpca_project_id = project_id,
          has_single_cell_data = c(TRUE, FALSE),
          has_spatial_data = c(FALSE, TRUE),
          has_bulk_rna_seq = c(TRUE, FALSE),
          has_cite_seq_data = c(TRUE, FALSE),
          has_multiplexed_data = c(FALSE, FALSE),
          seq_units = list(c("cell", "bulk"), "spot")
        )
      }
    }
  )

  info <- get_dataset_info(DATASET_ID, auth_token = "token")

  expect_equal(info$id, DATASET_ID)
  expect_equal(info$format, "SINGLE_CELL_EXPERIMENT")
  expect_equal(info$status, "pending")
  expect_equal(info$n_projects, 2)
  expect_equal(info$n_samples, 4)
  expect_equal(info$merged_projects, character(0))
  expect_null(info$bulk_projects)
  expect_s3_class(info$samples, "data.frame")
  expect_setequal(
    colnames(info$samples),
    c(
      "scpca_sample_id",
      "scpca_project_id",
      "seq_unit",
      "has_spatial",
      "has_bulk",
      "has_cite_seq",
      "has_multiplexed"
    )
  )
  # one row per included sample; the unrequested SCPCS000099 is filtered out
  expect_equal(nrow(info$samples), 4)
  expect_false("SCPCS000099" %in% info$samples$scpca_sample_id)

  field <- \(col, id) info$samples[[col]][info$samples$scpca_sample_id == id]
  # seq_unit is the single-cell unit, or NA for a spatial-only sample
  expect_equal(field("seq_unit", "SCPCS000001"), "cell")
  expect_equal(field("seq_unit", "SCPCS000003"), "cell")
  expect_true(is.na(field("seq_unit", "SCPCS000004")))

  # only requested modalities are reported
  expect_true(field("has_spatial", "SCPCS000004"))
  expect_false(field("has_spatial", "SCPCS000001"))

  expect_true(field("has_cite_seq", "SCPCS000003"))
  expect_false(field("has_cite_seq", "SCPCS000001"))

  # has_bulk reflects the request AND availability
  expect_true(field("has_bulk", "SCPCS000003")) # requested + available
  expect_false(field("has_bulk", "SCPCS000001")) # project did not request bulk
  expect_false(field("has_bulk", "SCPCS000004")) # requested but sample has none
  expect_false(any(info$samples$has_multiplexed))
})

test_that("get_dataset_info combines modalities for a sample included as single-cell and spatial", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        id = DATASET_ID,
        format = "SINGLE_CELL_EXPERIMENT",
        is_started = FALSE,
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001"),
            SPATIAL = list("SCPCS000001"),
            includes_bulk = FALSE
          )
        ),
        total_sample_count = 1
      )
    },
    get_project_samples = \(project_id, simplify = TRUE) {
      tibble::tibble(
        scpca_sample_id = "SCPCS000001",
        scpca_project_id = project_id,
        has_single_cell_data = TRUE,
        has_spatial_data = TRUE,
        has_bulk_rna_seq = FALSE,
        has_cite_seq_data = FALSE,
        has_multiplexed_data = FALSE,
        seq_units = list(c("cell", "spot"))
      )
    }
  )

  info <- get_dataset_info(DATASET_ID, auth_token = "token")

  # one row for the sample: single-cell unit plus spatial
  expect_equal(nrow(info$samples), 1)
  expect_equal(info$samples$seq_unit, "cell")
  expect_true(info$samples$has_spatial)
})

test_that("get_dataset_info returns empty samples data frame with correct schema for empty dataset", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        id = DATASET_ID,
        format = "ANN_DATA",
        is_started = FALSE,
        data = list(),
        total_sample_count = 0
      )
    }
  )

  info <- get_dataset_info(DATASET_ID, auth_token = "token")

  expect_equal(info$n_samples, 0)
  expect_equal(info$n_projects, 0)
  expect_equal(nrow(info$samples), 0)
  expect_null(info$bulk_projects)
  expect_setequal(
    colnames(info$samples),
    c(
      "scpca_sample_id",
      "scpca_project_id",
      "seq_unit",
      "has_spatial",
      "has_bulk",
      "has_cite_seq",
      "has_multiplexed"
    )
  )
})

test_that("get_dataset_info expands merged projects to all their single-cell samples", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        id = DATASET_ID,
        format = "SINGLE_CELL_EXPERIMENT",
        is_started = FALSE,
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001"),
            SPATIAL = list(),
            includes_bulk = FALSE
          ),
          SCPCP000005 = list(
            SINGLE_CELL = "MERGED",
            SPATIAL = list(),
            includes_bulk = FALSE
          )
        ),
        total_sample_count = 4
      )
    },
    get_project_samples = \(project_id, simplify = TRUE) {
      if (project_id == "SCPCP000001") {
        tibble::tibble(
          scpca_sample_id = "SCPCS000001",
          scpca_project_id = project_id,
          has_single_cell_data = TRUE,
          has_spatial_data = FALSE,
          has_bulk_rna_seq = FALSE,
          has_cite_seq_data = FALSE,
          has_multiplexed_data = FALSE,
          seq_units = list("cell")
        )
      } else {
        # merged project: all single-cell samples are included; the
        # non-single-cell SCPCS000053 is not
        tibble::tibble(
          scpca_sample_id = c("SCPCS000050", "SCPCS000051", "SCPCS000052", "SCPCS000053"),
          scpca_project_id = project_id,
          has_single_cell_data = c(TRUE, TRUE, TRUE, FALSE),
          has_spatial_data = c(FALSE, FALSE, FALSE, TRUE),
          has_bulk_rna_seq = FALSE,
          has_cite_seq_data = FALSE,
          has_multiplexed_data = FALSE,
          seq_units = list("cell", "cell", "nucleus", "spot")
        )
      }
    }
  )

  info <- get_dataset_info(DATASET_ID, auth_token = "token")

  # merged project still surfaced in merged_projects
  expect_equal(info$merged_projects, "SCPCP000005")
  # its single-cell samples are expanded into the table; SCPCS000053 is excluded
  expect_setequal(
    info$samples$scpca_sample_id,
    c("SCPCS000001", "SCPCS000050", "SCPCS000051", "SCPCS000052")
  )
  expect_false("SCPCS000053" %in% info$samples$scpca_sample_id)
  # the nucleus seq_unit is reported for that sample
  expect_equal(
    info$samples$seq_unit[info$samples$scpca_sample_id == "SCPCS000052"],
    "nucleus"
  )
  expect_equal(info$n_projects, 2)
  expect_equal(info$n_samples, 4)
})

test_that("get_dataset_info derives status from detail without a second API call", {
  call_count <- 0
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      call_count <<- call_count + 1
      list(
        id = DATASET_ID,
        format = "ANN_DATA",
        is_started = TRUE,
        is_succeeded = TRUE,
        data = list(),
        total_sample_count = 0
      )
    }
  )

  info <- get_dataset_info(DATASET_ID, auth_token = "token")

  expect_equal(call_count, 1)
  expect_equal(info$status, "succeeded")
})

test_that("get_dataset_info prunes projects where nothing is requested", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        id = DATASET_ID,
        format = "SINGLE_CELL_EXPERIMENT",
        is_started = FALSE,
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001"),
            SPATIAL = list(),
            includes_bulk = FALSE
          ),
          SCPCP000002 = list(
            SINGLE_CELL = list(),
            SPATIAL = list(),
            includes_bulk = FALSE
          )
        ),
        total_sample_count = 1
      )
    },
    # only SCPCP000001 should be queried; SCPCP000002 requests nothing
    get_project_samples = \(project_id, simplify = TRUE) {
      tibble::tibble(
        scpca_sample_id = "SCPCS000001",
        scpca_project_id = project_id,
        has_single_cell_data = TRUE,
        has_spatial_data = FALSE,
        has_bulk_rna_seq = FALSE,
        has_cite_seq_data = FALSE,
        has_multiplexed_data = FALSE,
        seq_units = list("cell")
      )
    }
  )

  info <- get_dataset_info(DATASET_ID, auth_token = "token")

  expect_equal(nrow(info$samples), 1)
  expect_equal(info$samples$scpca_project_id, "SCPCP000001")
  expect_false("SCPCP000002" %in% info$samples$scpca_project_id)
})

test_that("get_dataset_info errors when auth_token is empty", {
  expect_error(
    get_dataset_info(DATASET_ID, auth_token = ""),
    "Authorization token must be provided"
  )
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

# resolve_dataset_id tests

test_that("resolve_dataset_id accepts a string and a list with $id", {
  expect_equal(
    resolve_dataset_id(DATASET_ID),
    DATASET_ID
  )
  expect_equal(
    resolve_dataset_id(list(id = DATASET_ID, data = list())),
    DATASET_ID
  )
})

test_that("resolve_dataset_id errors on invalid input", {
  expect_error(resolve_dataset_id(list(data = list())), "id string or contain an \\$id element")
  expect_error(resolve_dataset_id(123), "id string or contain an \\$id element")
})

test_that("resolve_dataset_id errors when id is not a valid UUID", {
  expect_error(resolve_dataset_id("not-a-uuid"), "valid UUID")
  expect_error(
    resolve_dataset_id(list(id = "not-a-uuid", data = list())),
    "valid UUID"
  )
})

# replace_dataset_data tests

test_that("replace_dataset_data errors when neither samples nor projects are provided", {
  expect_error(
    replace_dataset_data(DATASET_ID, auth_token = "token"),
    "At least one of 'samples' or 'projects' must be provided"
  )
})

test_that("replace_dataset_data PUTs a rebuilt data field without a format", {
  captured_req <- NULL
  local_mocked_bindings(
    build_dataset_data = \(samples = NULL, projects = NULL, include_bulk = FALSE) {
      list(
        SCPCP000001 = list(
          SINGLE_CELL = list("SCPCS000001"),
          SPATIAL = list(),
          includes_bulk = include_bulk
        )
      )
    },
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(req$body$data)
    }
  )

  result <- replace_dataset_data(
    DATASET_ID,
    auth_token = "token",
    samples = "SCPCS000001"
  )

  expect_equal(httr2::req_get_method(captured_req), "PUT")
  expect_match(captured_req$url, paste0("datasets/", DATASET_ID))
  expect_null(captured_req$body$data$format)
  expect_true("SCPCP000001" %in% names(captured_req$body$data$data))
  expect_equal(result, DATASET_ID)
})

# set_dataset_email tests

test_that("set_dataset_email PUTs a new email", {
  captured_req <- NULL
  local_mocked_bindings(
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(req$body$data)
    }
  )

  result <- set_dataset_email(
    DATASET_ID,
    auth_token = "token",
    email = "user@example.com"
  )
  expect_equal(httr2::req_get_method(captured_req), "PUT")
  expect_match(captured_req$url, paste0("datasets/", DATASET_ID))
  expect_equal(captured_req$body$data$email, "user@example.com")
  expect_equal(result, DATASET_ID)
})

test_that("set_dataset_email errors when email is not a single string", {
  expect_error(
    set_dataset_email(
      DATASET_ID,
      auth_token = "token",
      email = c("a@b.com", "c@d.com")
    ),
    "email must be a single character string"
  )
})

test_that("set_dataset_email surfaces the generic conflict error on a locked dataset", {
  local_mocked_bindings(
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_409", message = "Conflict")
  )

  expect_error(
    set_dataset_email(
      DATASET_ID,
      email = "user@example.com",
      auth_token = "token"
    ),
    "Cannot modify ScPCA dataset"
  )
})

# start_dataset_processing tests

test_that("start_dataset_processing PUTs start = TRUE for a pending dataset", {
  captured_req <- NULL
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "pending",
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(req$body$data)
    }
  )

  result <- NULL
  expect_message(
    {
      result <- start_dataset_processing(
        DATASET_ID,
        auth_token = "token"
      )
    },
    "processing started"
  )
  expect_equal(httr2::req_get_method(captured_req), "PUT")
  expect_match(captured_req$url, paste0("datasets/", DATASET_ID))
  expect_true(captured_req$body$data$start)
  expect_null(captured_req$body$data$email)
  expect_equal(result, DATASET_ID)
})

test_that("start_dataset_processing includes email in the same request when provided", {
  captured_req <- NULL
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "pending",
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(req$body$data)
    }
  )

  result <- suppressMessages(
    start_dataset_processing(
      DATASET_ID,
      email = "user@example.com",
      auth_token = "token"
    )
  )
  expect_equal(httr2::req_get_method(captured_req), "PUT")
  expect_true(captured_req$body$data$start)
  expect_equal(captured_req$body$data$email, "user@example.com")
  expect_equal(result, DATASET_ID)
})

test_that("start_dataset_processing errors when email is not a single string", {
  expect_error(
    start_dataset_processing(
      DATASET_ID,
      email = c("a@b.com", "c@d.com"),
      auth_token = "token"
    ),
    "email must be a single character string"
  )
})

test_that("start_dataset_processing errors when auth_token is empty", {
  expect_error(
    start_dataset_processing(DATASET_ID, auth_token = ""),
    "Authorization token must be provided"
  )
})

test_that("start_dataset_processing emits a message and sends no request when already processing", {
  put_called <- FALSE
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "processing",
    req_perform = \(req, ...) {
      put_called <<- TRUE
      json_response(list())
    }
  )

  expect_message(
    result <- start_dataset_processing(DATASET_ID, auth_token = "token"),
    "is already processing"
  )
  expect_equal(result, DATASET_ID)
  expect_false(put_called)
})

test_that("start_dataset_processing emits a message and sends no request when already succeeded", {
  put_called <- FALSE
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "succeeded",
    req_perform = \(req, ...) {
      put_called <<- TRUE
      json_response(list())
    }
  )

  expect_message(
    result <- start_dataset_processing(DATASET_ID, auth_token = "token"),
    "has already completed processing"
  )
  expect_equal(result, DATASET_ID)
  expect_false(put_called)
})

test_that("start_dataset_processing warns and retries when previously failed", {
  captured_req <- NULL
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "failed",
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(req$body$data)
    }
  )

  expect_warning(
    suppressMessages(
      start_dataset_processing(DATASET_ID, auth_token = "token")
    ),
    "previously failed to process"
  )
  expect_equal(httr2::req_get_method(captured_req), "PUT")
  expect_true(captured_req$body$data$start)
})

test_that("start_dataset_processing restarts an expired dataset", {
  captured_req <- NULL
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "expired",
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(req$body$data)
    }
  )

  suppressMessages(
    start_dataset_processing(DATASET_ID, auth_token = "token")
  )
  expect_equal(httr2::req_get_method(captured_req), "PUT")
  expect_true(captured_req$body$data$start)
})

test_that("start_dataset_processing surfaces a locked-dataset error on a 409 race", {
  local_mocked_bindings(
    get_dataset_status = \(dataset, auth_token) "pending",
    req_perform = \(req, ...) rlang::abort(class = "httr2_http_409", message = "Conflict")
  )

  expect_error(
    start_dataset_processing(DATASET_ID, auth_token = "token"),
    "Cannot modify ScPCA dataset"
  )
})

# merge_dataset_data / remove_from_dataset_data unit tests

test_that("merge_dataset_data unions sample IDs within shared projects", {
  existing <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001"),
      SPATIAL = list(),
      includes_bulk = TRUE
    )
  )
  additions <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001", "SCPCS000002"),
      SPATIAL = list(),
      includes_bulk = FALSE
    )
  )

  result <- merge_dataset_data(existing, additions, include_bulk = FALSE)
  expect_setequal(
    as.character(result$SCPCP000001$SINGLE_CELL),
    c("SCPCS000001", "SCPCS000002")
  )
  # existing project keeps its includes_bulk value
  expect_true(result$SCPCP000001$includes_bulk)
})

test_that("merge_dataset_data adds new projects with the supplied include_bulk", {
  existing <- list(
    SCPCP000001 = list(SINGLE_CELL = list("SCPCS000001"), SPATIAL = list(), includes_bulk = FALSE)
  )
  additions <- list(
    SCPCP000002 = list(SINGLE_CELL = list("SCPCS000003"), SPATIAL = list(), includes_bulk = FALSE)
  )

  result <- merge_dataset_data(existing, additions, include_bulk = TRUE)
  expect_setequal(names(result), c("SCPCP000001", "SCPCP000002"))
  expect_true(result$SCPCP000002$includes_bulk)
})

test_that("merge_dataset_data errors on a merged project", {
  existing <- list(
    SCPCP000001 = list(SINGLE_CELL = "MERGED", SPATIAL = list(), includes_bulk = FALSE)
  )
  additions <- list(
    SCPCP000001 = list(SINGLE_CELL = list("SCPCS000002"), SPATIAL = list(), includes_bulk = FALSE)
  )

  expect_error(merge_dataset_data(existing, additions), "merged single-cell data")
})

test_that("remove_from_dataset_data removes samples and prunes empty projects", {
  existing <- list(
    SCPCP000001 = list(
      SINGLE_CELL = list("SCPCS000001", "SCPCS000002"),
      SPATIAL = list(),
      includes_bulk = FALSE
    ),
    SCPCP000002 = list(
      SINGLE_CELL = list("SCPCS000003"),
      SPATIAL = list(),
      includes_bulk = FALSE
    )
  )

  result <- remove_from_dataset_data(existing, samples = c("SCPCS000002", "SCPCS000003"))
  expect_equal(as.character(result$SCPCP000001$SINGLE_CELL), "SCPCS000001")
  expect_false("SCPCP000002" %in% names(result))
})

test_that("remove_from_dataset_data drops whole projects", {
  existing <- list(
    SCPCP000001 = list(SINGLE_CELL = list("SCPCS000001"), SPATIAL = list(), includes_bulk = FALSE),
    SCPCP000002 = list(SINGLE_CELL = list("SCPCS000003"), SPATIAL = list(), includes_bulk = FALSE)
  )

  result <- remove_from_dataset_data(existing, projects = "SCPCP000002")
  expect_equal(names(result), "SCPCP000001")
})

# add_dataset_samples / remove_dataset_samples tests

test_that("add_dataset_samples PUTs", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(id = DATASET_ID, data = list())
    },
    build_dataset_data = \(...) list(),
    req_perform = \(req, ...) json_response(list(id = DATASET_ID))
  )

  result <- add_dataset_samples(DATASET_ID, auth_token = "token", samples = "SCPCS000002")
  expect_equal(result, DATASET_ID)
})

test_that("remove_dataset_samples PUTs", {
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(id = DATASET_ID, data = list())
    },
    req_perform = \(req, ...) json_response(list(id = DATASET_ID))
  )

  result <- remove_dataset_samples(DATASET_ID, auth_token = "token", projects = "SCPCP000002")
  expect_equal(result, DATASET_ID)
})
