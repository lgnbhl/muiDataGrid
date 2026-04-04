`%||%` <- function(x, y) if (is.null(x)) y else x

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
#' df <- data.frame(name = paste("Row", 1:50), value = 1:50)
#'
#' # Initial render (no params yet)
#' processGridParams(df, params = NULL, pageSize = 10)
#'
#' # Page 2, sorted descending, filtered
#' params <- list(
#'   pagination_model = list(page = 1, pageSize = 10),
#'   sort_model = list(list(field = "value", sort = "desc")),
#'   filter_model = list(items = list(
#'     list(field = "value", operator = ">", value = "10")
#'   ))
#' )
#' processGridParams(df, params)
#'
#' @rdname processGridParams
#' @export
processGridParams <- function(data, params, pageSize = 100L) {
  stopifnot(is.data.frame(data))
  # MUI field names from input$<inputId>: page, pageSize, field, sort,
  # operator, value, items. Update here if MUI renames them between versions.
  # NULL$foo returns NULL in R, so these safely default when params is NULL.
  page         <- params$pagination_model$page     %||% 0L
  page_size    <- params$pagination_model$pageSize %||% pageSize
  sort_items   <- params$sort_model                %||% list()
  filter_items <- params$filter_model$items        %||% list()

  # Sorting — all items applied together to support multi-column sort
  valid_sort <- Filter(
    function(s) !is.null(s$field) && s$field %in% names(data),
    sort_items
  )
  if (length(valid_sort) > 0) {
    order_cols <- lapply(valid_sort, function(s) {
      if (identical(s$sort, "desc")) {
        -xtfrm(data[[s$field]])
      } else {
        data[[s$field]]
      }
    })
    order_cols$na.last <- TRUE
    data <- data[do.call(order, order_cols), ]
  }

  # Filtering
  no_value_ops <- c("isEmpty", "isNotEmpty")
  logic_op <- params$filter_model$logicOperator %||% "and"

  keep_vectors <- list()
  for (f in filter_items) {
    if (is.null(f$field) || !f$field %in% names(data)) {
      next
    }
    if (is.null(f$value) && !f$operator %in% no_value_ops) {
      next
    }
    col <- data[[f$field]]
    keep <- switch(
      f$operator,
      "contains" = grepl(
        tolower(f$value),
        tolower(as.character(col)),
        fixed = TRUE
      ),
      "equals" = tolower(as.character(col)) == tolower(f$value),
      "is" = as.character(col) == f$value,
      "startsWith" = startsWith(tolower(as.character(col)), tolower(f$value)),
      "endsWith" = endsWith(tolower(as.character(col)), tolower(f$value)),
      "not" = ,
      "!=" = tolower(as.character(col)) != tolower(f$value),
      "=" = col == suppressWarnings(as.numeric(f$value)),
      ">" = col > suppressWarnings(as.numeric(f$value)),
      ">=" = col >= suppressWarnings(as.numeric(f$value)),
      "<" = col < suppressWarnings(as.numeric(f$value)),
      "<=" = col <= suppressWarnings(as.numeric(f$value)),
      "isAnyOf" = tolower(as.character(col)) %in% tolower(as.character(f$value)),
      "after" = col > as.Date(f$value),
      "onOrAfter" = col >= as.Date(f$value),
      "before" = col < as.Date(f$value),
      "onOrBefore" = col <= as.Date(f$value),
      "isEmpty" = is.na(col) | as.character(col) == "",
      "isNotEmpty" = !is.na(col) & as.character(col) != "",
      rep(TRUE, nrow(data))
    )
    keep[is.na(keep)] <- FALSE
    keep_vectors[[length(keep_vectors) + 1L]] <- keep
  }

  if (length(keep_vectors) > 0) {
    combined <- if (identical(logic_op, "or")) {
      Reduce(`|`, keep_vectors)
    } else {
      Reduce(`&`, keep_vectors)
    }
    data <- data[combined, ]
  }

  # Inject stable IDs before slicing so they are consistent across pages
  data <- .inject_id(data)

  total_rows <- nrow(data)
  start <- page * page_size + 1
  end <- min(start + page_size - 1, total_rows)
  page_data <- if (start <= total_rows) data[start:end, ] else data[0, ]
  rownames(page_data) <- NULL
  list(rows = page_data, rowCount = total_rows)
}

#' Server-Side DataGrid
#'
#' A DataGrid component with server-side pagination, sorting, and filtering.
#' The component sends pagination, sort, and filter state to R via a Shiny input.
#'
#' Pass the full dataset via \code{rows} — just like \code{DataGrid()} —
#' and \code{DataGridServer()} handles pagination, sorting, and filtering
#' automatically. For manual control (e.g. database queries), supply
#' pre-sliced \code{rows} together with an explicit \code{rowCount}.
#'
#' @param inputId Character. The Shiny input ID. When pagination, sorting, or
#'   filtering changes, the new state is available as \code{input$<inputId>}
#'   in the server. The value is a list with elements \code{pagination_model}
#'   (list with \code{page} and \code{pageSize}), \code{sort_model} (list of
#'   sort items), and \code{filter_model} (list with \code{items}).
#' @param rows A data.frame. Pass the \strong{full} dataset (like
#'   \code{DataGrid()}) and let \code{DataGridServer()} handle pagination
#'   automatically, or pass a pre-sliced page together with an explicit
#'   \code{rowCount} for manual control.
#' @param columns Column definitions (list of lists). If NULL, auto-generated
#'   from \code{names(rows)}.
#' @param rowCount Integer. When provided, \code{rows} is assumed to be
#'   already paginated and \code{rowCount} is used as the total row count
#'   (manual mode). When \code{NULL} (default), pagination is handled
#'   automatically from the full \code{rows} dataset.
#' @param loading Logical. Whether to show the loading indicator. If
#'   \code{NULL}, MUI defaults to \code{FALSE}.
#' @param initialPageSize Integer. Convenience for setting the initial page
#'   size. Builds MUI's \code{initialState} prop. If \code{NULL}, MUI defaults
#'   to 100. Also sets the page size for the first automatic render before the
#'   grid has sent state. Must be included in \code{pageSizeOptions}.
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
#' @examples
#' \dontrun{
#' # Simple usage: pass the full dataset, pagination is handled automatically
#' output$grid <- renderReact({
#'   DataGridServer("grid_params",
#'     rows = my_data,
#'     initialPageSize = 10L,
#'     pageSizeOptions = c(10L, 25L, 50L)
#'   )
#' })
#'
#' # Manual usage: handle pagination yourself (e.g. database queries)
#' output$grid <- renderReact({
#'   result <- processGridParams(my_data, input$grid_params, pageSize = 10L)
#'   DataGridServer("grid_params",
#'     rows = result$rows,
#'     rowCount = result$rowCount,
#'     initialPageSize = 10L,
#'     pageSizeOptions = c(10L, 25L, 50L)
#'   )
#' })
#' }
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
  if (
    !is.null(initialPageSize) &&
      !is.null(pageSizeOptions) &&
      !initialPageSize %in% pageSizeOptions
  ) {
    stop(
      "initialPageSize (",
      initialPageSize,
      ") is not in pageSizeOptions (",
      paste(pageSizeOptions, collapse = ", "),
      "). MUI requires the initial page size to be included in pageSizeOptions."
    )
  }
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    registry <- session$userData$.datagrid_input_registry
    if (is.null(registry)) registry <- list()
    current_output <- shiny::getCurrentOutputInfo()$name
    # Clear any stale entry previously owned by this output (e.g. inputId changed)
    stale <- names(registry)[vapply(registry, identical, logical(1), current_output)]
    if (length(stale) > 0) {
      registry[stale] <- NULL
    }
    claimed_by <- registry[[inputId]]
    if (!is.null(claimed_by) && !identical(claimed_by, current_output)) {
      stop(
        'inputId "', inputId, '" is already used by output "', claimed_by, '". ',
        "Each DataGridServer() must have a unique inputId."
      )
    }
    registry[[inputId]] <- current_output
    session$userData$.datagrid_input_registry <- registry
  }
  # Automatic mode: rowCount not supplied, so run processGridParams internally.
  if (is.null(rowCount) && !is.null(rows)) {
    params <- if (!is.null(session)) session$input[[inputId]] else NULL
    page_size <- if (!is.null(initialPageSize)) as.integer(initialPageSize) else 100L
    result <- processGridParams(rows, params, pageSize = page_size)
    rows <- result$rows
    rowCount <- result$rowCount
  }
  if (is.null(columns) && !is.null(rows)) {
    columns <- data.frame(field = setdiff(names(rows), "id"))
  }
  rows <- .inject_id(rows)
  dots <- list(...)
  protected <- c("paginationMode", "sortingMode", "filterMode", "pagination")
  overridden <- intersect(names(dots), protected)
  if (length(overridden) > 0) {
    warning(
      "Ignoring protected props set automatically by DataGridServer(): ",
      paste(overridden, collapse = ", ")
    )
    dots[overridden] <- NULL
  }
  initial_state <- if (!is.null(initialPageSize)) {
    list(
      pagination = list(
        paginationModel = list(
          page = 0L,
          pageSize = as.integer(initialPageSize)
        )
      )
    )
  }
  props <- c(
    list(
      inputId = inputId,
      rows = rows,
      columns = columns,
      rowCount = if (!is.null(rowCount)) as.integer(rowCount),
      loading = loading,
      initialState = initial_state,
      pageSizeOptions = if (!is.null(pageSizeOptions)) {
        as.list(as.integer(pageSizeOptions))
      },
      filterDebounce = filterDebounce,
      paginationMode = "server",
      sortingMode = "server",
      filterMode = "server",
      pagination = TRUE
    ),
    dots
  )
  tag <- shiny.react::reactElement(
    module = "@muiDataGrid/custom",
    name = "ServerSideDataGrid",
    props = do.call(shiny.react::asProps, props),
    deps = muiDataGridDependency()
  )
  class(tag) <- c("muiDataGrid", class(tag))
  tag
}
