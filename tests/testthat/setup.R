library(httptest2)

# Helper to create a minimal httr2 JSON response for use in local_mocked_bindings
json_response <- function(body_list, status = 200L) {
  structure(
    list(
      method = "GET",
      url = "https://example.com",
      status_code = status,
      headers = structure(
        list(`content-type` = "application/json"),
        class = "httr2_headers"
      ),
      body = charToRaw(jsonlite::toJSON(body_list, auto_unbox = TRUE)),
      timing = c(redirect = 0, namelookup = 0, connect = 0,
                 pretransfer = 0, starttransfer = 0, total = 0),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}
