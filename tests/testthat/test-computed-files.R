# tests/testthat/test-computed-files.R

test_that("get_computed_file_ids validates input parameters", {
  # Test that info_list must contain computed_files element
  expect_error(
    get_computed_file_ids(list()),
    "info_list must contain a computed_files element"
  )

  expect_error(
    get_computed_file_ids(list(computed_files = list())),
    "no computed files found in info_list"
  )

  # Test that all computed files must have an id element
  expect_error(
    get_computed_file_ids(list(
      computed_files = list(
        list(format = "SCE"),
        list(id = 123, format = "ANN_DATA")
      )
    )),
    "all computed_files must have an id element"
  )

  # Test that filters must be a named list
  info_list <- list(
    computed_files = list(
      list(id = 123, format = "SCE")
    )
  )

  expect_error(
    get_computed_file_ids(info_list, filters = c("format", "SCE")),
    "filters must be a named list of filtering criteria"
  )
})

test_that("get_computed_file_ids returns all IDs with no filters", {
  info_list <- list(
    computed_files = list(
      list(id = 1, format = "SINGLE_CELL_EXPERIMENT", modality = "SINGLE_CELL"),
      list(id = 2, format = "ANN_DATA", modality = "SINGLE_CELL"),
      list(id = 3, format = "SPATIAL", modality = "SPATIAL")
    )
  )

  result <- get_computed_file_ids(info_list)

  expect_setequal(result, c("1", "2", "3"))
  expect_type(result, "character")
})

test_that("get_computed_file_ids filters correctly", {
  info_list <- list(
    computed_files = list(
      list(id = 1, format = "SINGLE_CELL_EXPERIMENT", modality = "SINGLE_CELL"),
      list(id = 2, format = "ANN_DATA", modality = "SINGLE_CELL"),
      list(id = 3, format = "SINGLE_CELL_EXPERIMENT", modality = "SPATIAL")
    )
  )

  # Filter for SCE format
  result_sce <- get_computed_file_ids(
    info_list,
    filters = list(format = "SINGLE_CELL_EXPERIMENT", modality = "SINGLE_CELL")
  )
  expect_equal(result_sce, "1")

  result_sce2 <- get_computed_file_ids(
    info_list,
    filters = list(format = "SINGLE_CELL_EXPERIMENT", modality = "!SPATIAL")
  )
  expect_equal(result_sce2, "1")

  # Filter for AnnData format
  result_anndata <- get_computed_file_ids(info_list, filters = list(format = "ANN_DATA"))
  expect_equal(result_anndata, "2")

  # Filter for modality
  result_spatial <- get_computed_file_ids(info_list, filters = list(modality = "SPATIAL"))
  expect_equal(result_spatial, "3")
})

test_that("get_computed_file_ids returns empty vector when no matches found", {
  info_list <- list(
    computed_files = list(
      list(id = 1, format = "SINGLE_CELL_EXPERIMENT", modality = "SINGLE_CELL"),
      list(id = 2, format = "ANN_DATA", modality = "SINGLE_CELL")
    )
  )

  # Filter for non-existent format
  result <- get_computed_file_ids(info_list, filters = list(format = "NONEXISTENT"))
  expect_equal(result, character(0))

  # Filter for non-existent modality
  result2 <- get_computed_file_ids(info_list, filters = list(modality = "NONEXISTENT"))
  expect_equal(result2, character(0))
})

test_that("get_computed_file_ids handles filters for non-existent fields", {
  info_list <- list(
    computed_files = list(
      list(id = 1, format = "SINGLE_CELL_EXPERIMENT", modality = "SINGLE_CELL"),
      list(id = 2, format = "ANN_DATA", modality = "SINGLE_CELL")
    )
  )

  # Filter for non-existent field should return no matches
  result <- get_computed_file_ids(info_list, filters = list(nonexistent_field = "value"))
  expect_equal(result, character(0))
})
