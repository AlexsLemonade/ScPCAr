#' Base request object for ScPCA API
#'
#' @param resource API resource to query, e.g. "projects", default is "" (base URL)
#' @param body optional named list to include as JSON body in the request
#' @param auth_token optional API authentication token
#' @param method optional HTTP method to use (e.g. "PATCH"). When NULL (the
#'   default), httr2 infers the method: POST when a body is present, otherwise GET.
#' @param ... additional query parameters to include in the request
#'
#' @keywords internal
#'
#' @import httr2
#'
#' @returns a httr2 request object
scpca_request <- function(resource = "", body = list(), auth_token = "", method = NULL, ...) {
  stopifnot(
    "body must be a named list" = is.list(body) && (length(body) == 0 || !is.null(names(body)))
  )
  api_base <- "https://api.scpca.alexslemonade.org/v1" # nolint
  user_agent <- "ScPCA R API Client" # nolint

  req <- httr2::request(api_base) |>
    req_user_agent(user_agent) |>
    req_url_path_append(resource) |>
    req_throttle(capacity = 60) |> # No more than 60 calls per minute
    req_url_query(...)

  if (length(body) > 0) {
    req <- req |> req_body_json(body)
  }

  if (nchar(auth_token) > 0) {
    req <- req |> req_headers_redacted(`api-key` = auth_token)
  }

  if (!is.null(method)) {
    req <- req |> req_method(method)
  }

  req
}

#' Helper function for iterating through paginated ScPCA API results
#'
#' @param resp httr2 response object
#' @param req httr2 request object
#'
#' @returns updated httr2 request object for the next page, or NULL if there are no more pages
#'
#' @keywords internal
#'
#' @import httr2
iterate_scpca <- function(resp, req) {
  body <- resp_body_json(resp)
  url <- body[["next"]]
  if (is.null(url)) {
    return(NULL)
  }
  # calculate total pages and signal to req_perform_iterative
  pages <- ceiling(body[["count"]] / length(body[["results"]]))
  signal_total_pages(pages)
  req |> req_url(url)
}


#' Centralized error handler for ScPCA API requests
#'
#' Wraps an expression that performs an httr2 request and converts common HTTP
#' error statuses into informative messages. All three statuses can be
#' overridden with a custom message via the corresponding argument.
#'
#' @param expr An expression that performs an httr2 request, evaluated lazily
#'   inside `tryCatch()`.
#' @param not_found_msg Character string to use when the API returns 404.
#'   Defaults to a generic "not found" message.
#' @param conflict_msg Character string to use when the API returns 409.
#'   Defaults to a generic "conflict" message.
#' @param forbidden_msg Character string to use when the API returns 403.
#'   Defaults to a generic authorization-failure message.
#'
#' @returns The value of `expr` if the request succeeds (i.e. an httr2
#'   response object, or whatever the expression returns on success).
#'
#' @keywords internal
#'
#' @import httr2
with_scpca_errors <- function(
  expr,
  not_found_msg = NULL,
  conflict_msg = NULL,
  forbidden_msg = NULL
) {
  tryCatch(
    expr,
    httr2_http_403 = \(cnd) {
      msg <- if (is.null(forbidden_msg)) {
        paste0(
          "Authorization failed (HTTP 403): your token may be invalid",
          " or not permitted to access this resource.",
          " Check your token or use `get_auth()` to obtain a new token."
        )
      } else {
        forbidden_msg
      }
      stop(msg, call. = FALSE)
    },
    httr2_http_404 = \(cnd) {
      # check_api() will raise "API may be down" if the API is unreachable;
      # if it returns TRUE the resource itself was not found.
      check_api()
      msg <- if (is.null(not_found_msg)) {
        "The requested resource was not found."
      } else {
        not_found_msg
      }
      stop(msg, call. = FALSE)
    },
    httr2_http_409 = \(cnd) {
      msg <- if (is.null(conflict_msg)) {
        paste0(
          "The request could not be completed because the resource is locked or in conflict.",
          " Datasets are locked once processing has started."
        )
      } else {
        conflict_msg
      }
      stop(msg, call. = FALSE)
    }
  )
}


#' Perform an ScPCA API request with centralized error handling
#'
#' A pipe-friendly drop-in for [httr2::req_perform()] that centralizes handling
#' of common HTTP error statuses (403, 404, 409). Returns the httr2 response
#' object on success so downstream `resp_body_json()` calls are unaffected.
#'
#' @param req An httr2 request object, typically built with [scpca_request()].
#' @param not_found_msg Character string to use when the API returns 404.
#' @param conflict_msg Character string to use when the API returns 409.
#' @param forbidden_msg Character string to use when the API returns 403.
#' @param ... Additional arguments forwarded to [httr2::req_perform()].
#'
#' @returns An httr2 response object.
#'
#' @keywords internal
#'
#' @import httr2
scpca_perform <- function(
  req,
  not_found_msg = NULL,
  conflict_msg = NULL,
  forbidden_msg = NULL,
  ...
) {
  # req_perform() is called unqualified (no httr2:: prefix) so that
  # testthat::local_mocked_bindings(req_perform = ...) can intercept it in tests.
  with_scpca_errors(
    req_perform(req, ...),
    not_found_msg = not_found_msg,
    conflict_msg = conflict_msg,
    forbidden_msg = forbidden_msg
  )
}


# NOTE: check_api() must NOT be migrated to use scpca_perform() or
# with_scpca_errors(). scpca_perform()'s 404 handler calls check_api() to
# distinguish "resource not found" from "API down", so routing check_api()
# through scpca_perform() would cause infinite recursion:
#   404 -> check_api() -> scpca_perform() -> 404 -> check_api() -> ...
#
#' Check if the ScPCA API is reachable
#'
#' This function performs a simple GET request to the ScPCA API to verify that it is reachable.
#'
#' @returns TRUE if the API is reachable, otherwise an error is raised.
#' @import httr2
#'
#' @keywords internal
check_api <- function() {
  status <- tryCatch(
    {
      scpca_request() |>
        req_perform() |>
        resp_status()
    },
    httr2_http_404 = \(cnd) NULL
  )
  if (is.null(status) || status != 200) {
    stop(
      "The API may be down or unreachable. Please check your internet connection or try again later."
    )
  }
  TRUE
}


#' Internal helper to validate and normalize formats for the ScPCA API
#'
#' Note that while spatial format strings are included here, the API does not
#' currently accommodate a format code for spatial data; that is designated in the
#' modality field for individual samples or precomputed datasets.
#'
#' @keywords internal
#'
#' @param format The input format string
#' @param allow_spatial Whether to allow spatial format strings (default TRUE)
#' @returns The normalized format string for API use
normalize_format <- function(format, allow_spatial = TRUE) {
  stopifnot(
    "format must be a single string" = is.character(format) && length(format) == 1,
    "allow_spatial must be a single logical value" = is.logical(allow_spatial) &&
      length(allow_spatial) == 1
  )
  # Accepted format strings (case insensitive) for the `format` argument in download functions
  sce_formats <- c(
    "sce",
    "singlecellexperiment",
    "single-cell-experiment",
    "single_cell_experiment"
  )
  anndata_formats <- c("anndata", "h5ad", "ann-data")
  spatial_formats <- c(
    "spatial",
    "spaceranger",
    "space ranger",
    "spatial_spaceranger",
    "spatial-spaceranger"
  )

  format_lower <- tolower(format)
  format_str <- dplyr::case_when(
    format_lower %in% sce_formats ~ "SINGLE_CELL_EXPERIMENT",
    format_lower %in% anndata_formats ~ "ANN_DATA",
    format_lower %in% spatial_formats && allow_spatial ~ "SPATIAL",
    .default = NA_character_
  )
  if (is.na(format_str)) {
    allowed_formats <- c("'sce'", "'anndata'")
    if (allow_spatial) {
      allowed_formats <- c(allowed_formats, "'spatial'")
    }
    stop(
      "Invalid format: `",
      format,
      "`.\n",
      " Expected format strings are ",
      stringr::str_flatten_comma(allowed_formats, last = ", or "),
      ", with some handling of common variants."
    )
  }
  format_str
}
