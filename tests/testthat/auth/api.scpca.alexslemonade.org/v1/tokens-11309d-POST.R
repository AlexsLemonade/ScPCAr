structure(
  list(
    method = "POST",
    url = "https://api.scpca.alexslemonade.org/v1/tokens",
    status_code = 201L,
    headers = structure(
      list(
        Server = "nginx/1.18.0 (Ubuntu)",
        Date = "Mon, 08 Sep 2025 22:19:57 GMT",
        `Content-Type` = "application/json",
        `Content-Length` = "102",
        Connection = "keep-alive",
        Vary = "Accept, Cookie, Origin",
        Allow = "POST, OPTIONS",
        `X-Frame-Options` = "DENY",
        `X-Content-Type-Options` = "nosniff",
        `Referrer-Policy` = "same-origin"
      ),
      class = "httr2_headers"
    ),
    body = charToRaw(
      "{\"id\":\"mock-token-123\",\"is_activated\":true,\"terms_and_conditions\":\"PLACEHOLDER\"}"
    ),
    timing = c(
      redirect = 0,
      namelookup = 0.023242,
      connect = 0.034899,
      pretransfer = 0.048263,
      starttransfer = 0.086918,
      total = 0.08695
    ),
    cache = new.env(parent = emptyenv())
  ),
  class = "httr2_response"
)
