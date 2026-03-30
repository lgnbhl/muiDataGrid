#' DataGrid
#'
#' @param rows rows
#' @param columns columns
#' @param ... Other props
#'
#' @rdname DataGrid
#' @export
DataGrid <- function(rows = NULL, columns = NULL, ...) {
  if (is.null(columns)) {
    columns <- data.frame(
      field = names(rows)
    )
  }
  if (!"id" %in% names(rows)) {
    rows$id <- rownames(rows)
  }
  tag <- shiny.react::reactElement(
    module = "@mui/x-data-grid",
    name = "DataGrid",
    props = shiny.react::asProps(rows = rows, columns = columns, ...),
    deps = muiDataGridDependency()
  )
  class(tag) <- c("muiDataGrid", class(tag))
  tag
}

#' Process server-side grid parameters
#'
#' Applies pagination, sorting, and filtering from \code{DataGridServer}
#' parameters to a data.frame. Use inside \code{renderReact()} with
#' \code{input$<inputId>} to get the current page of data.
#'
#' @param data A data.frame with all rows.
#' @param params The grid params list from \code{input$<inputId>}, or NULL
#'   on initial render.
#' @param pageSize Default page size when params is NULL. Default 10.
#'
#' @return A list with \code{rows} (data.frame for the current page) and
#'   \code{rowCount} (integer, total matching rows).
#'
#' @examples
#' \dontrun{
#' output$grid <- renderReact({
#'   result <- processGridParams(my_data, input$grid_params)
#'   DataGridServer("grid_params", rows = result$rows, rowCount = result$rowCount)
#' })
#' }
#'
#' @rdname processGridParams
#' @export
processGridParams <- function(data, params, pageSize = 10L) {
  page <- if (!is.null(params)) params$pagination_model$page else 0
  ps <- if (!is.null(params)) params$pagination_model$pageSize else pageSize

  # Sorting
  if (!is.null(params$sort_model) && length(params$sort_model) > 0) {
    sort_field <- params$sort_model[[1]]$field
    sort_dir <- params$sort_model[[1]]$sort
    if (!is.null(sort_field) && sort_field %in% names(data)) {
      data <- data[
        order(data[[sort_field]], decreasing = !identical(sort_dir, "asc")),
      ]
    }
  }

  # Filtering
  if (
    !is.null(params$filter_model$items) &&
      length(params$filter_model$items) > 0
  ) {
    for (item in params$filter_model$items) {
      field <- item$field
      op <- item$operator
      value <- item$value
      if (!is.null(field) && !is.null(value) && field %in% names(data)) {
        col <- data[[field]]
        keep <- switch(
          op,
          "contains" = grepl(value, col, ignore.case = TRUE),
          "equals" = as.character(col) == value,
          "startsWith" = grepl(paste0("^", value), col, ignore.case = TRUE),
          "endsWith" = grepl(paste0(value, "$"), col, ignore.case = TRUE),
          "is" = as.character(col) == value,
          "not" = as.character(col) != value,
          "!=" = as.character(col) != value,
          ">" = col > as.numeric(value),
          ">=" = col >= as.numeric(value),
          "<" = col < as.numeric(value),
          "<=" = col <= as.numeric(value),
          "isEmpty" = is.na(col) | col == "",
          "isNotEmpty" = !is.na(col) & col != "",
          rep(TRUE, nrow(data))
        )
        data <- data[keep, ]
      }
    }
  }

  total_rows <- nrow(data)
  start <- page * ps + 1
  end <- min(start + ps - 1, total_rows)
  page_data <- if (start <= total_rows) data[start:end, ] else data[0, ]

  list(rows = page_data, rowCount = total_rows)
}

#' Server-Side DataGrid
#'
#' A DataGrid component with server-side pagination, sorting, and filtering.
#' The component sends pagination, sort, and filter state to R via a Shiny input.
#'
#' @param inputId Character. The Shiny input ID. When pagination, sorting, or
#'   filtering changes, the new state is available as \code{input$<inputId>}
#'   in the server. The value is a list with elements \code{pagination_model}
#'   (list with \code{page} and \code{pageSize}), \code{sort_model} (list of
#'   sort items), and \code{filter_model} (list with \code{items}).
#' @param rows A data.frame of rows for the current page.
#' @param columns Column definitions (list of lists). If NULL, auto-generated
#'   from \code{names(rows)}.
#' @param rowCount Integer. Total number of rows across all pages.
#' @param loading Logical. Whether to show the loading indicator.
#' @param initialPageSize Integer. Initial page size. Default 25.
#' @param pageSizeOptions Integer vector. Available page size options.
#'   Default \code{c(10, 25, 50, 100)}.
#' @param ... Additional props passed to the MUI DataGrid.
#'
#' @return A shiny.react element.
#'
#' @rdname DataGridServer
#' @export
DataGridServer <- function(
  inputId,
  rows = NULL,
  columns = NULL,
  rowCount = 0L,
  loading = FALSE,
  initialPageSize = 25L,
  pageSizeOptions = c(10L, 25L, 50L, 100L),
  ...
) {
  if (is.null(columns) && !is.null(rows)) {
    columns <- data.frame(field = names(rows))
  }
  if (!is.null(rows) && !"id" %in% names(rows)) {
    rows$id <- rownames(rows)
  }
  tag <- shiny.react::reactElement(
    module = "@muiDataGrid/custom",
    name = "ServerSideDataGrid",
    props = shiny.react::asProps(
      inputId = inputId,
      rows = rows,
      columns = columns,
      rowCount = as.integer(rowCount),
      loading = loading,
      initialPageSize = as.integer(initialPageSize),
      pageSizeOptions = as.list(as.integer(pageSizeOptions)),
      ...
    ),
    deps = muiDataGridDependency()
  )
  class(tag) <- c("muiDataGrid", class(tag))
  tag
}
