# Inject a stable integer id column if one is not already present.
.inject_id <- function(rows) {
  if (!is.null(rows) && !"id" %in% names(rows)) {
    rows$id <- seq_len(nrow(rows))
  }
  rows
}

#' DataGrid
#'
#' @param rows A data.frame of rows. An \code{id} column is added automatically
#'   from row names if not already present.
#' @param columns Column definitions (list of lists). If \code{NULL},
#'   auto-generated from \code{names(rows)}.
#' @param ... Additional props passed directly to the MUI DataGrid component.
#'
#' @rdname DataGrid
#' @export
DataGrid <- function(rows = NULL, columns = NULL, ...) {
  if (is.null(columns)) {
    columns <- data.frame(field = names(rows))
  }
  rows <- .inject_id(rows)
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
#' @param pageSize Page size to use when \code{params} is \code{NULL} (i.e.
#'   the first render before the grid has sent state). Should match the
#'   \code{initialPageSize} passed to \code{DataGridServer}, or MUI's default
#'   of 100 if none was specified.
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
processGridParams <- function(data, params, pageSize = 100L) {
  # MUI field names from input$<inputId>: page, pageSize, field, sort,
  # operator, value, items. Update here if MUI renames them between versions.
  page         <- if (!is.null(params)) params$pagination_model$page    else 0L
  page_size    <- if (!is.null(params)) params$pagination_model$pageSize else pageSize
  sort_items   <- if (!is.null(params)) params$sort_model                else list()
  filter_items <- if (!is.null(params)) params$filter_model$items        else list()

  # Sorting — all items applied together to support multi-column sort
  valid_sort <- Filter(function(s) !is.null(s$field) && s$field %in% names(data), sort_items)
  if (length(valid_sort) > 0) {
    order_cols <- lapply(valid_sort, function(s)
      if (identical(s$sort, "desc")) -xtfrm(data[[s$field]]) else data[[s$field]]
    )
    data <- data[do.call(order, order_cols), ]
  }

  # Filtering
  no_value_ops <- c("isEmpty", "isNotEmpty")
  for (f in filter_items) {
    if (is.null(f$field) || !f$field %in% names(data)) next
    if (is.null(f$value) && !f$operator %in% no_value_ops) next
    col <- data[[f$field]]
    keep <- switch(
      f$operator,
      "contains"   = grepl(tolower(f$value), tolower(as.character(col)), fixed = TRUE),
      "equals"     = ,
      "is"         = tolower(as.character(col)) == tolower(f$value),
      "startsWith" = startsWith(tolower(as.character(col)), tolower(f$value)),
      "endsWith"   = endsWith(tolower(as.character(col)), tolower(f$value)),
      "not"        = ,
      "!="         = tolower(as.character(col)) != tolower(f$value),
      ">"          = col > as.numeric(f$value),
      ">="         = col >= as.numeric(f$value),
      "<"          = col < as.numeric(f$value),
      "<="         = col <= as.numeric(f$value),
      "isEmpty"    = is.na(col) | as.character(col) == "",
      "isNotEmpty" = !is.na(col) & as.character(col) != "",
      rep(TRUE, nrow(data))
    )
    data <- data[keep, ]
  }

  total_rows <- nrow(data)
  start      <- page * page_size + 1
  end        <- min(start + page_size - 1, total_rows)
  list(
    rows     = if (start <= total_rows) data[start:end, ] else data[0, ],
    rowCount = total_rows
  )
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
#' @param rowCount Integer. Total number of rows across all pages. If
#'   \code{NULL}, the prop is not sent and MUI handles it as unknown.
#' @param loading Logical. Whether to show the loading indicator. If
#'   \code{NULL}, MUI defaults to \code{FALSE}.
#' @param initialPageSize Integer. Convenience for setting the initial page
#'   size. Builds MUI's \code{initialState} prop. If \code{NULL}, MUI defaults
#'   to 100. Should match the \code{pageSize} argument of
#'   \code{processGridParams}.
#' @param pageSizeOptions Integer vector. Available page size options. If
#'   \code{NULL}, MUI defaults to \code{c(25, 50, 100)}.
#' @param filterDebounce Integer. Milliseconds to debounce filter input before
#'   sending state to R. If \code{NULL}, defaults to 300 ms.
#' @param ... Additional props passed directly to the MUI DataGrid component.
#'   Note: \code{paginationMode}, \code{sortingMode}, \code{filterMode}, and
#'   \code{pagination} are set automatically and should not be overridden.
#'
#' @return A shiny.react element.
#'
#' @rdname DataGridServer
#' @export
DataGridServer <- function(
  inputId,
  rows = NULL,
  columns = NULL,
  rowCount = NULL,
  loading = NULL,
  initialPageSize = NULL,
  pageSizeOptions = NULL,
  filterDebounce = NULL,
  ...
) {
  if (is.null(columns) && !is.null(rows)) {
    columns <- data.frame(field = names(rows))
  }
  rows <- .inject_id(rows)
  if (!is.null(initialPageSize) && !is.null(pageSizeOptions) &&
      !initialPageSize %in% pageSizeOptions) {
    warning("initialPageSize (", initialPageSize, ") is not in pageSizeOptions (",
            paste(pageSizeOptions, collapse = ", "), "). MUI requires the initial page size to be included.")
  }
  initial_state <- if (!is.null(initialPageSize)) {
    list(pagination = list(paginationModel = list(page = 0L, pageSize = as.integer(initialPageSize))))
  }
  tag <- shiny.react::reactElement(
    module = "@muiDataGrid/custom",
    name = "ServerSideDataGrid",
    props = shiny.react::asProps(
      inputId = inputId,
      rows = rows,
      columns = columns,
      rowCount = if (!is.null(rowCount)) as.numeric(rowCount),
      loading = loading,
      initialState = initial_state,
      pageSizeOptions = if (!is.null(pageSizeOptions)) as.list(as.integer(pageSizeOptions)),
      filterDebounce = filterDebounce,
      paginationMode = "server",
      sortingMode = "server",
      filterMode = "server",
      pagination = TRUE,
      ...
    ),
    deps = muiDataGridDependency()
  )
  class(tag) <- c("muiDataGrid", class(tag))
  tag
}
