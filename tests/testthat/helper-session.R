# The `rowCount` deprecation warns once per session; many tests still exercise
# that path, so mark it as already shown. The deprecation test resets it.
assign("warned_rowcount", TRUE, envir = muiDataGrid:::.state)

# Serve DataGridServer()'s HTTP endpoints in tests. MockShinySession's
# registerDataObj() is a no-op, so record each request handler instead and
# return a URL as a real session does. Returns the handler store.
local_endpoints <- function(env = parent.frame()) {
  endpoints <- new.env()
  testthat::local_mocked_bindings(
    .register_rows_endpoint = function(session, inputId, fetch) {
      endpoints[[inputId]] <- muiDataGrid:::.rows_endpoint_handler(inputId, fetch)
      paste0("session/test/dataobj/", inputId)
    },
    .package = "muiDataGrid",
    .env = env
  )
  endpoints
}

# POST `body` (a list) to a recorded endpoint; returns status and parsed JSON.
request_endpoint <- function(endpoints, name, body) {
  json <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
  req <- list(rook.input = list(read = function() charToRaw(json)))
  res <- endpoints[[name]](NULL, req)
  list(
    status = res$status,
    body = jsonlite::fromJSON(res$content, simplifyVector = FALSE)
  )
}

page_request <- function(page = 0, pageSize = 25, sort = list(), items = list()) {
  list(
    pagination_model = list(page = page, pageSize = pageSize),
    sort_model = sort,
    filter_model = list(items = items)
  )
}
