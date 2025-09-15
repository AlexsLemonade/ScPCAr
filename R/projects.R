#' Get data frame of all ScPCA projects
#'
#' This retrieves the full list of projects from the ScPCA Portal and returns
#' a data frame of project metadata. By default, list columns are removed to create
#' a simplified data frame, but this can be disabled by setting `simplify = FALSE`.
#' The unsimplified data frame contains nested list columns with additional details
#' about the samples within each project, such as the set of diagnoses associated with
#' each project and the individual sample ids.
#'
#' @param simplify A logical indicating whether to simplify the resulting data frame
#'  by removing list columns. Default is TRUE.
#'
#' @returns a data frame of project information from the ScPCA API.
#'
#' @import httr2
#' @importFrom dplyr .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # a simplified data frame of all projects
#' project_df <- scpca_projects()
#'
#' # a data frame of all projects without simplification
#' project_df_full <- scpca_projects(simplify = FALSE)
#' }
scpca_projects <- function(simplify = TRUE) {
  responses <- scpca_request("projects", limit = 50) |>
    req_perform_iterative(iterate_scpca)

  project_df <- responses |>
    resps_data(\(resp) {
      resp_body_json(resp, simplifyVector = TRUE)$results |>
        as.data.frame()
    })

  if (simplify) {
    project_df <- project_df |>
      dplyr::select(dplyr::where(\(col) !is.list(col)))
  }

  # convert types and relocate columns
  project_df <- project_df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), \(x) dplyr::na_if(x, "NA")),
      created_at = as.POSIXct(.data$created_at),
      updated_at = as.POSIXct(.data$updated_at)
    ) |>
    dplyr::relocate(scpca_project_id = "scpca_id")

  project_df
}

#' Get project metadata by project ID
#'
#' @param project_id The ScPCA project ID (e.g. "SCPCP000001")
#' @param simplifyVector Simplify the returned list structure,
#'  creating vectors and data frames instead of lists when possible.
#'  Default is FALSE.
#'
#' @returns A nested list of project metadata from the ScPCA API.
#'
#' @import httr2
#'
#' @export
#' @examples
#' \dontrun{
#' # Get metadata for a specific project
#' project_info <- get_project_info("SCPCP000001")
#' }
get_project_info <- function(project_id, simplifyVector = FALSE) {
  stopifnot(
    "Invalid project_id." = grepl("^SCPCP\\d{6}$", project_id)
  )

  response <- tryCatch(
    {
      scpca_request(paste0("projects/", project_id)) |>
        req_perform()
    },
    # return NULL if 404 and the API is reachable (check_api will raise error if not)
    httr2_http_404 = \(cnd) if (check_api()) NULL
  )
  # if we get a 404, check if the API then give project error if not found
  if (is.null(response)) {
    stop(glue::glue("Project `{project_id}` not found."))
  }

  resp_body_json(response, simplifyVector = simplifyVector)
}

#' Get a data frame of all samples in a given project
#'
#' This function retrievs a data frame of all biological samples associated with a SCPCA project,
#' including sample-level metadata. By default, list columns are removed to create
#' a simplified data frame, but this can be disabled by setting `simplify = FALSE`.
#' The unsimplified data frame contains nested list columns with additional details,
#' such as the experimental modalities associated with each sample.
#'
#' @param project_id The ScPCA project ID (e.g. "SCPCP000001")
#' @param simplify A logical indicating whether to simplify the resulting data frame
#'  by removing list columns. Default is TRUE.
#'
#' @returns A data frame of sample information for the specified project from the ScPCA API.
#'
#' @import httr2
#' @importFrom dplyr .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get sample info for a specific project
#' samples_df <- get_project_samples("SCPCP000001")
#'
#' # Get sample info without simplifying
#' samples_df_full <- get_project_samples("SCPCP000001", simplify = FALSE)
#' }
get_project_samples <- function(project_id, simplify = TRUE) {
  project_info <- get_project_info(project_id, simplifyVector = TRUE)
  sample_df <- project_info$samples |>
    as.data.frame() |>
    # unnest additional_metadata column
    tidyr::unnest("additional_metadata")

  if (simplify) {
    sample_df <- sample_df |>
      dplyr::select(dplyr::where(\(col) !is.list(col)))
  }

  # reorganize and recast columns
  sample_df <- sample_df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), \(x) dplyr::na_if(x, "NA")),
      age = as.numeric(.data$age),
      created_at = as.POSIXct(.data$created_at),
      updated_at = as.POSIXct(.data$updated_at)
    ) |>
    dplyr::relocate(scpca_sample_id = "scpca_id", scpca_project_id = "project")

  sample_df
}


#' Get metadata for all libraries in a given project
#'
#' This function downloads and reads the library metadata file for a given ScPCA project.
#' The data frame returned will be the same as the project metadata file available from the
#' ScPCA Portal website for each project, including information about each library
#' that is part of the project.
#'
#' @param project_id The ScPCA project ID (e.g. "SCPCP000001")
#' @param auth_token An authorization token obtained from `get_auth()`
#'
#' @returns A data frame (tibble) of library metadata for the specified project.
#'
#' @export
#' @examples
#' \dontrun{
#' # First get an auth token
#' token <- get_auth("me@email.net", agree = TRUE)
#' # Get library metadata for a specific project
#' libraries_df <- get_project_libraries("SCPCP000001", token)
#' }
#'
get_project_libraries <- function(project_id, auth_token) {
  download_url <- get_project_metadata_url(project_id, auth_token)

  file_paths <- download_and_extract_file(download_url, tempfile(), overwrite = TRUE, quiet = TRUE)
  if (!is_testing()) {
    on.exit(unlink(file_paths), add = TRUE)
  }
  metadata_file <- file_paths[basename(file_paths) == "metadata.tsv"]
  if (length(metadata_file) == 0) {
    stop("Metadata file not found in downloaded archive.")
  }
  library_metadata <- readr::read_tsv(
    metadata_file,
    col_types = readr::cols(
      .default = "c",
      age = "d",
      total_reads = "d",
      mapped_reads = "d",
      unfiltered_cells = "i",
      filtered_cell_count = "i",
      processed_cells = "i",
      has_cellhash = "l",
      includes_anndata = "l",
      is_cell_line = "l",
      is_multiplexed = "l",
      is_xenograft = "l",
      prob_compromised_cutoff = "d",
      min_gene_cutoff = "d",
      date_processed = "T"
    )
  )

  # convert any missed "is_" "has_" or "includes_" columns to logical
  is_cols <- stringr::str_subset(colnames(library_metadata), "^(is|has|includes)_")
  library_metadata <- library_metadata |>
    dplyr::mutate(dplyr::across(dplyr::all_of(is_cols), as.logical))
  library_metadata
}


#' Get project metadata download URL
#'
#' @param project_id The ScPCA project ID (e.g. "SCPCP000001")
#' @param auth_token An authorization token obtained from `get_auth()`
#'
#' @returns A signed download URL for the project metadata file as would be found
#'  from the ScPCA Portal.
#'
#' @import httr2
#'
#' @examples
#' \dontrun{
#' # First get an auth token
#' token <- get_auth("me@email.net", agree = TRUE)
#' # Get metadata for a specific project
#' project_info <- get_project_metadata_url("SCPCP000001", token)
#' }
get_project_metadata_url <- function(project_id, auth_token) {
  project_info <- get_project_info(project_id, simplifyVector = FALSE)
  metadata_id <- project_info$computed_files |>
    purrr::keep(\(file) file$metadata_only) |>
    purrr::map_chr(\(f) as.character(f$id))

  if (length(metadata_id) == 0) {
    stop(glue::glue("No metadata file found for project `{project_id}`."))
  }
  if (length(metadata_id) > 1) {
    warning(glue::glue(
      "Multiple metadata files found for project `{project_id}`.",
      " Using the first one."
    ))
    metadata_id <- metadata_id[1]
  }

  download_url <- scpca_request(
    resource = paste0("computed-files/", metadata_id),
    auth_token = auth_token
  ) |>
    req_perform() |>
    resp_body_json() |>
    purrr::pluck("download_url")

  download_url
}
