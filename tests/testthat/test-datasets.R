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
      body <- req$body$data
      json_response(c(body, list(id = "new-dataset-uuid")))
    }
  )

  result <- NULL
  expect_message(
    {
      result <- create_dataset(samples = "SCPCS000001", format = "sce", auth_token = "token")
    },
    "new-dataset-uuid"
  )
  expect_false(result$start)
})

test_that("create_dataset returns response invisibly and messages with dataset id", {
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
  expect_equal(result$id, "new-dataset-uuid")
})

# get_dataset_detail tests

test_that("get_dataset_detail returns dataset with data and status fields", {
  with_mock_dir("ds_status", {
    result <- get_dataset_detail("ds-uuid", auth_token = "test-token")

    expect_type(result, "list")
    expect_equal(result$id, "ds-uuid")
    expect_equal(result$format, "SINGLE_CELL_EXPERIMENT")
    expect_false(result$is_started)
    expect_false(result$is_succeeded)
  })
})

test_that("get_dataset_detail returns data field with project and sample structure", {
  with_mock_dir("ds_status", {
    result <- get_dataset_detail("ds-uuid", auth_token = "test-token")

    expect_type(result$data, "list")
    expect_true("SCPCP000001" %in% names(result$data))
    expect_contains(
      result$data$SCPCP000001$SINGLE_CELL,
      c("SCPCS000001", "SCPCS000002")
    )
  })
})

test_that("get_dataset_detail includes api-key header when auth_token is provided", {
  local_mocked_bindings(
    req_perform = \(req, ...) {
      json_response(list(
        id = "uuid",
        data = list(),
        api_key = httr2::req_get_headers(req, "reveal")$`api-key`
      ))
    }
  )

  result <- get_dataset_detail("uuid", auth_token = "my-token")
  expect_equal(result$api_key, "my-token")
})

test_that("get_dataset_detail handles 404 errors correctly", {
  local_mocked_bindings(
    check_api = function() TRUE
  )

  with_mock_dir("ds_status_404", {
    expect_error(
      get_dataset_detail("no-uuid", auth_token = "test-token"),
      "Dataset `no-uuid` not found."
    )
  })
})

test_that("get_dataset_detail accepts a list with $id in place of a string", {
  local_mocked_bindings(
    req_perform = \(req, ...) json_response(list(id = "ds-uuid", data = list()))
  )

  dataset_list <- list(id = "ds-uuid", data = list())
  result <- get_dataset_detail(dataset_list, auth_token = "test-token")
  expect_equal(result$id, "ds-uuid")
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
  expect_equal(resolve_dataset_id("ds-uuid"), "ds-uuid")
  expect_equal(resolve_dataset_id(list(id = "ds-uuid", data = list())), "ds-uuid")
})

test_that("resolve_dataset_id errors on invalid input", {
  expect_error(resolve_dataset_id(list(data = list())), "id string or contain an \\$id element")
  expect_error(resolve_dataset_id(123), "id string or contain an \\$id element")
})

# replace_dataset_data tests

test_that("replace_dataset_data errors when neither samples nor projects are provided", {
  expect_error(
    replace_dataset_data("ds-uuid", auth_token = "token"),
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
    "ds-uuid",
    auth_token = "token",
    samples = "SCPCS000001"
  )

  expect_equal(captured_req$method, "PUT")
  expect_match(captured_req$url, "datasets/ds-uuid")
  expect_null(result$format)
  expect_true("SCPCP000001" %in% names(result$data))
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

  result <- set_dataset_email("ds-uuid", auth_token = "token", email = "user@example.com")
  expect_equal(captured_req$method, "PUT")
  expect_match(captured_req$url, "datasets/ds-uuid")
  expect_equal(result$email, "user@example.com")
})

test_that("set_dataset_email errors when email is not a single string", {
  expect_error(
    set_dataset_email("ds-uuid", auth_token = "token", email = c("a@b.com", "c@d.com")),
    "email must be a single character string"
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

test_that("add_dataset_samples merges new samples into existing data and PUTs", {
  captured_req <- NULL
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        id = "ds-uuid",
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001"),
            SPATIAL = list(),
            includes_bulk = FALSE
          )
        )
      )
    },
    build_dataset_data = \(samples = NULL, projects = NULL, include_bulk = FALSE) {
      list(
        SCPCP000001 = list(
          SINGLE_CELL = list("SCPCS000002"),
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

  result <- add_dataset_samples("ds-uuid", auth_token = "token", samples = "SCPCS000002")
  expect_equal(captured_req$method, "PUT")
  expect_setequal(
    as.character(result$data$SCPCP000001$SINGLE_CELL),
    c("SCPCS000001", "SCPCS000002")
  )
})

test_that("remove_dataset_samples removes a project and PUTs", {
  captured_req <- NULL
  local_mocked_bindings(
    get_dataset_detail = \(dataset, auth_token) {
      list(
        id = "ds-uuid",
        data = list(
          SCPCP000001 = list(
            SINGLE_CELL = list("SCPCS000001"),
            SPATIAL = list(),
            includes_bulk = FALSE
          ),
          SCPCP000002 = list(
            SINGLE_CELL = list("SCPCS000003"),
            SPATIAL = list(),
            includes_bulk = FALSE
          )
        )
      )
    },
    req_perform = \(req, ...) {
      captured_req <<- req
      json_response(req$body$data)
    }
  )

  result <- remove_dataset_samples("ds-uuid", auth_token = "token", projects = "SCPCP000002")
  expect_equal(captured_req$method, "PUT")
  expect_equal(names(result$data), "SCPCP000001")
})
