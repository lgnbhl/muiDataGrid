`%||%` <- function(x, y) if (is.null(x)) y else x

# Parse to Date, returning NA for any value that is not an unambiguous date
# instead of erroring. base as.Date() throws on the whole vector if a single
# element is unparseable (e.g. "Row 1" or ""), which would abort a render; this
# fails soft like the numeric filters. Fast path: one vectorized as.Date() call
# (the common case — a column of uniform ISO dates — must not pay a per-element
# tryCatch loop on every filter evaluation). Elements the vectorized pass could
# not parse (mixed formats, or an unparseable first element aborting the whole
# call) are retried individually so per-element semantics are preserved.
.as_date <- function(x) {
  x <- as.character(x)
  out <- tryCatch(as.Date(x), error = function(e) NULL)
  if (is.null(out)) {
    out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  }
  retry <- which(is.na(out) & !is.na(x))
  for (i in retry) {
    out[i] <- tryCatch(as.Date(x[i]), error = function(e) as.Date(NA))
  }
  out
}

# Shared validation for the `rows` argument: anything that is not a data.frame
# (matrix, list of lists, vector) fails later in .inject_id()/.auto_columns()
# with errors far from the user's mistake, so reject it up front with a clear
# message. NULL stays allowed (both components accept it).
.validate_rows <- function(rows) {
  if (!is.null(rows) && !is.data.frame(rows)) {
    stop(
      "`rows` must be a data.frame or NULL, not ", class(rows)[1], ".",
      call. = FALSE
    )
  }
  invisible(rows)
}

# Inject a stable integer id column if one is not already present. When the
# caller supplies their own id, warn on duplicates or missing values: MUI X
# Data Grid requires unique, non-missing row ids and otherwise hard-errors in
# the browser ("all rows must have a unique id"), which is far harder to
# diagnose than an R-side warning.
.inject_id <- function(rows) {
  if (!is.null(rows)) {
    if (!"id" %in% names(rows)) {
      rows$id <- seq_len(nrow(rows))
    } else if (anyNA(rows$id) || anyDuplicated(rows$id) > 0) {
      warning(
        "The 'id' column has duplicate or missing values; MUI X Data Grid ",
        "requires unique, non-missing row ids and will error in the browser. ",
        "Ensure 'id' is unique and has no NA.",
        call. = FALSE
      )
    }
  }
  rows
}

# Map an R column's class to a MUI X Data Grid column `type`. Without this the
# grid treats every auto-generated column as a string, so numeric columns get
# string filter operators (contains/startsWith) instead of >/<, lose
# right-alignment, and sort lexically.
#
# Only `number` and `boolean` are inferred: both serialize to a JSON
# value (number / true|false) that matches what the MUI type expects at
# runtime. Date/POSIXct are deliberately left as `string` — JSON has no date
# type, so they reach the browser as strings, and MUI's `date`/`dateTime`
# columns expect real JS Date objects (string input causes off-by-one display
# from UTC parsing, `Invalid Date` in some browsers, and broken date filters).
# ISO date strings already sort chronologically as strings, so this is safe.
# Users who want the date-picker UX should set `type = "date"` plus a
# `valueGetter` explicitly.
.mui_col_type <- function(x) {
  if (is.logical(x)) {
    "boolean"
  } else if (is.numeric(x)) {
    "number"
  } else {
    "string"
  }
}

# Fill in a `type` for any column left untyped, inferring it from the matching
# column's R class (see .mui_col_type). MUI does not auto-detect type from row
# values, so without this an explicit `columns` spec that omits `type` leaves
# numeric columns as strings (left-aligned, string filter operators). An
# explicit `type` is always respected. Handles both supported `columns` shapes:
# a data.frame (one row per column) and a list of per-column lists.
.fill_column_types <- function(columns, rows) {
  if (is.null(columns) || is.null(rows)) {
    return(columns)
  }
  if (is.data.frame(columns)) {
    if (!"field" %in% names(columns)) {
      return(columns)
    }
    # Infer for every column lacking an explicit type. Treat a missing `type`
    # column, NA, and "" all as "untyped" so a *partial* type column (some rows
    # set, others NA) still gets the unset rows inferred — matching the
    # per-column behavior of the list form below. Explicit types are kept.
    type <- if ("type" %in% names(columns)) as.character(columns$type) else rep(NA_character_, nrow(columns))
    needs <- is.na(type) | type == ""
    if (any(needs)) {
      fields <- as.character(columns$field)
      inferred <- vapply(
        fields,
        function(f) if (f %in% names(rows)) .mui_col_type(rows[[f]]) else "string",
        character(1)
      )
      type[needs] <- inferred[needs]
      columns$type <- type
    }
    return(columns)
  }
  if (is.list(columns)) {
    columns <- lapply(columns, function(col) {
      if (is.list(col) && is.null(col$type) &&
        !is.null(col$field) && col$field %in% names(rows)) {
        inferred <- .mui_col_type(rows[[col$field]])
        # Only add non-default types; "string" is MUI's default, so setting it
        # explicitly would just clutter the user's column definition.
        if (inferred != "string") {
          col$type <- inferred
        }
      }
      col
    })
  }
  columns
}

# Auto-generate column definitions from a data.frame, excluding the id column
# and inferring each column's MUI type from its R class.
.auto_columns <- function(rows) {
  fields <- setdiff(names(rows), "id")
  .fill_column_types(
    data.frame(field = fields, stringsAsFactors = FALSE),
    rows
  )
}

#' DataGrid
#'
#' @param rows A data.frame of rows. An \code{id} column of 1-based row numbers
#'   is added automatically if not already present.
#' @param columns Column definitions (list of lists, or a data.frame with one
#'   row per column). If \code{NULL}, auto-generated from \code{names(rows)}.
#'   Any column without an explicit \code{type} has one inferred from the
#'   matching \code{rows} column: numeric columns get MUI's \code{number} type
#'   and logical columns its \code{boolean} type; all other classes (including
#'   \code{Date}, \code{POSIXct}, and factors) default to \code{string}. A
#'   \code{type} you set yourself is always kept. To get MUI's date-picker
#'   filtering, set \code{type = "date"} together with a \code{valueGetter} that
#'   returns a JS \code{Date} explicitly.
#' @param ... Additional props passed directly to the MUI DataGrid component.
#'
#' @return A \code{shiny.react} element (also classed \code{muiDataGrid}) that
#'   renders the MUI X Data Grid. Use it directly in Shiny UI, inside
#'   \code{renderReact()}, or in a Quarto/R Markdown document.
#'
#' @examples
#' # Minimal: column definitions are auto-generated from the data frame.
#' DataGrid(rows = head(iris))
#'
#' # Custom columns plus an initial page size of 5 rows.
#' DataGrid(
#'   rows = head(mtcars),
#'   columns = list(
#'     list(field = "mpg", headerName = "MPG"),
#'     list(field = "cyl", headerName = "Cylinders")
#'   ),
#'   initialState = list(
#'     pagination = list(paginationModel = list(pageSize = 5))
#'   )
#' )
#'
#' @rdname DataGrid
#' @export
DataGrid <- function(rows = NULL, columns = NULL, ...) {
  .validate_rows(rows)
  if (is.null(columns) && !is.null(rows)) {
    columns <- .auto_columns(rows)
  } else if (!is.null(columns)) {
    columns <- .fill_column_types(columns, rows)
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
#' @section Lifecycle:
#' \strong{Experimental.} This helper is \emph{specific to this R package} and
#' has no equivalent in MUI X Data Grid, which leaves the server-side data
#' layer entirely to the developer. It reimplements MUI's server-mode
#' filtering and sorting semantics in R, so its behavior may change between
#' releases and can differ in edge cases from MUI's own client-side filtering.
#' Decisions worth knowing about:
#' \itemize{
#'   \item String filters are case-insensitive (except \code{is}, which is
#'     case-sensitive); missing values always sort last; unrecognized filter
#'     operators pass all rows through \emph{with a warning}.
#'   \item Filter items with an empty value (\code{NULL}, \code{""}, or an
#'     empty \code{isAnyOf} set) are ignored, matching MUI's client-side
#'     behavior of disabling such items.
#'   \item The toolbar quick filter is supported: each search term must appear
#'     (case-insensitive substring) in at least one column other than
#'     \code{id}; terms combine per the model's
#'     \code{quickFilterLogicOperator} (\code{"and"} by default), and the
#'     result is combined with the regular filter items using AND.
#'   \item Sorting and string comparison use R's locale collation, so the
#'     ordering of accented or mixed-case values can differ from the
#'     browser's ordering in the client-side \code{DataGrid()}.
#'   \item Date \code{is}/\code{not} compare values as strings (ISO dates
#'     work; \code{Date} columns stringify to ISO), while
#'     \code{after}/\code{before}/\code{onOrAfter}/\code{onOrBefore} parse
#'     dates — a character column holding non-ISO dates filters correctly
#'     with the latter but not with \code{is}/\code{not}.
#' }
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
  page      <- params$pagination_model$page     %||% 0L
  page_size <- params$pagination_model$pageSize %||% pageSize
  .slice_page(.apply_sort_filter(data, params), page, page_size)
}

# Sorting + filtering + id injection: the expensive part of processGridParams,
# extracted so DataGridServer()'s automatic mode can memoize it (see
# .memoized_sort_filter). A pagination-only change re-renders the grid but does
# not alter this result, so caching it avoids re-running order() over every row
# on each page click.
.apply_sort_filter <- function(data, params) {
  sort_items   <- params$sort_model         %||% list()
  filter_items <- params$filter_model$items %||% list()

  # Sorting — all items applied together to support multi-column sort.
  # Skip list-columns: xtfrm() (used below) errors on them, and they have no
  # meaningful order, so leaving them unsorted fails soft instead of erroring.
  valid_sort <- Filter(
    function(s) {
      !is.null(s$field) && s$field %in% names(data) && !is.list(data[[s$field]])
    },
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
  known_ops <- c(
    "contains", "equals", "is", "startsWith", "endsWith", "not", "!=",
    "=", ">", ">=", "<", "<=", "isAnyOf",
    "after", "onOrAfter", "before", "onOrBefore",
    no_value_ops
  )
  logic_op <- params$filter_model$logicOperator %||% "and"

  keep_vectors <- list()
  for (f in filter_items) {
    if (is.null(f$field) || !f$field %in% names(data)) {
      next
    }
    op <- f$operator %||% ""
    if (nzchar(op) && !op %in% known_ops) {
      warning(
        "Unsupported filter operator '", op, "' for field '",
        f$field, "'; this condition passes all rows through unfiltered. ",
        "Use manual mode (see processGridParams) to handle it yourself.",
        call. = FALSE
      )
    }
    # Skip items with no usable value, matching MUI's client-side semantics:
    # its getApplyFilterFn returns null (no filtering) for an empty value.
    # Covers NULL (operator just selected), "" (text cleared, or the boolean
    # filter's "any" choice), and an empty set (all isAnyOf chips removed).
    # Without this, "" coerces to NA for number columns and NA comparisons
    # drop every row — the grid goes blank while MUI would show all rows.
    has_no_value <- is.null(f$value) || length(f$value) == 0 ||
      (length(f$value) == 1 && identical(as.character(f$value), ""))
    if (has_no_value && !op %in% no_value_ops) {
      next
    }
    col <- data[[f$field]]
    keep <- switch(
      op,
      "contains" = grepl(
        tolower(f$value),
        tolower(as.character(col)),
        fixed = TRUE
      ),
      "equals" = tolower(as.character(col)) == tolower(f$value),
      # `is` serves three MUI column types. boolean: MUI sends "true"/"false"
      # while a logical column stringifies to "TRUE"/"FALSE", so a string
      # comparison would match nothing — compare as logical instead. Date
      # columns: as.character() yields ISO, matching MUI's ISO filter value.
      # singleSelect: exact, case-sensitive equality (MUI's behavior).
      "is" = if (is.logical(col)) {
        col == as.logical(f$value)
      } else {
        as.character(col) == f$value
      },
      "startsWith" = startsWith(tolower(as.character(col)), tolower(f$value)),
      "endsWith" = endsWith(tolower(as.character(col)), tolower(f$value)),
      # singleSelect "not": string inequality. Number "!=": numeric inequality
      # (string comparison would misjudge e.g. 5.10 vs "5.1").
      "not" = tolower(as.character(col)) != tolower(f$value),
      "!=" = if (is.numeric(col)) {
        col != suppressWarnings(as.numeric(f$value))
      } else {
        tolower(as.character(col)) != tolower(f$value)
      },
      # Mirror "!=": numeric equality for number columns, case-insensitive string
      # equality otherwise (a stray "=" on a non-numeric column then fails soft
      # instead of coercing the whole column to NA).
      "=" = if (is.numeric(col)) {
        col == suppressWarnings(as.numeric(f$value))
      } else {
        tolower(as.character(col)) == tolower(f$value)
      },
      # Numeric comparison for number columns; lexical character comparison
      # otherwise. MUI only emits these for `number` columns, but guarding on
      # is.numeric() (as `=`/`!=` above do) means a mistyped string column that
      # somehow receives a number-style filter compares lexically instead of
      # coercing the whole column to NA.
      ">" = if (is.numeric(col)) col > suppressWarnings(as.numeric(f$value)) else as.character(col) > f$value,
      ">=" = if (is.numeric(col)) col >= suppressWarnings(as.numeric(f$value)) else as.character(col) >= f$value,
      "<" = if (is.numeric(col)) col < suppressWarnings(as.numeric(f$value)) else as.character(col) < f$value,
      "<=" = if (is.numeric(col)) col <= suppressWarnings(as.numeric(f$value)) else as.character(col) <= f$value,
      "isAnyOf" = tolower(as.character(col)) %in% tolower(as.character(f$value)),
      # Coerce via character so date columns that reach R as strings (the default
      # when `type = "string"`) compare correctly, not just real Date columns.
      # .as_date() returns NA for unparseable values (column *or* filter value)
      # instead of erroring, so a stray non-date fails soft (dropped by the
      # keep[is.na] step) rather than aborting the whole render.
      "after" = .as_date(col) > .as_date(f$value),
      "onOrAfter" = .as_date(col) >= .as_date(f$value),
      "before" = .as_date(col) < .as_date(f$value),
      "onOrBefore" = .as_date(col) <= .as_date(f$value),
      "isEmpty" = is.na(col) | as.character(col) == "",
      "isNotEmpty" = !is.na(col) & as.character(col) != "",
      rep(TRUE, nrow(data))
    )
    keep[is.na(keep)] <- FALSE
    keep_vectors[[length(keep_vectors) + 1L]] <- keep
  }

  # Quick filter (the toolbar search box). Mirrors MUI's default client-side
  # semantics: each search term must appear — case-insensitive substring — in
  # at least one column; terms combine per quickFilterLogicOperator ("and" by
  # default); and the result is ANDed with the regular filter items. The `id`
  # column is skipped to match the auto-generated columns, which never
  # display it (an auto-injected 1..n id would otherwise match numeric terms).
  quick_terms <- as.character(unlist(
    params$filter_model$quickFilterValues %||% list()
  ))
  quick_terms <- quick_terms[!is.na(quick_terms) & nzchar(quick_terms)]
  quick_keep <- NULL
  if (length(quick_terms) > 0) {
    search_cols <- setdiff(names(data), "id")
    per_term <- lapply(quick_terms, function(term) {
      hits <- lapply(search_cols, function(nm) {
        grepl(tolower(term), tolower(as.character(data[[nm]])), fixed = TRUE)
      })
      if (length(hits) == 0) rep(FALSE, nrow(data)) else Reduce(`|`, hits)
    })
    quick_logic <- params$filter_model$quickFilterLogicOperator %||% "and"
    quick_keep <- if (identical(quick_logic, "or")) {
      Reduce(`|`, per_term)
    } else {
      Reduce(`&`, per_term)
    }
    quick_keep[is.na(quick_keep)] <- FALSE
  }

  combined <- NULL
  if (length(keep_vectors) > 0) {
    combined <- if (identical(logic_op, "or")) {
      Reduce(`|`, keep_vectors)
    } else {
      Reduce(`&`, keep_vectors)
    }
  }
  if (!is.null(quick_keep)) {
    combined <- if (is.null(combined)) quick_keep else combined & quick_keep
  }
  if (!is.null(combined)) {
    data <- data[combined, ]
  }

  # Inject stable IDs before slicing so they are consistent across pages
  .inject_id(data)
}

# Slice a single page out of an already sorted/filtered frame: the cheap part of
# processGridParams, kept separate so it can run on a memoized frame without
# repeating the sort/filter (see .apply_sort_filter, .memoized_sort_filter).
.slice_page <- function(data, page, page_size) {
  total_rows <- nrow(data)
  start <- page * page_size + 1
  end <- min(start + page_size - 1, total_rows)
  page_data <- if (start <= total_rows) data[start:end, ] else data[0, ]
  rownames(page_data) <- NULL
  list(rows = page_data, rowCount = total_rows)
}

# Memoize the sort/filter pass for DataGridServer()'s automatic mode. In auto
# mode the user's full `rows` is re-processed on every render, and a page change
# *is* a render — so without this, paging through a sorted 100k-row frame
# re-runs order() over all rows on every click. Cache key: the sort and filter
# models plus the source frame itself (compared with identical(), which is a
# single linear pass versus the O(n log n) re-sort it avoids). Storing the
# source and processed frames trades memory for speed; for very large data
# prefer manual mode (pass rowCount) so R never holds the full frame.
.memoized_sort_filter <- function(session, inputId, rows, params) {
  sort_model   <- params$sort_model                     %||% list()
  filter_items <- params$filter_model$items             %||% list()
  quick_terms  <- params$filter_model$quickFilterValues %||% list()
  # No sort and no filter: .apply_sort_filter is just .inject_id (cheap), so the
  # cache lookup/store would cost more than it saves. Compute directly.
  if (is.null(session) ||
    (length(sort_model) == 0 && length(filter_items) == 0 &&
      length(quick_terms) == 0)) {
    return(.apply_sort_filter(rows, params))
  }
  filter_model <- params$filter_model %||% list(items = list())
  cache <- session$userData$.datagrid_sortfilter_cache %||% list()
  entry <- cache[[inputId]]
  if (!is.null(entry) &&
    identical(entry$sort, sort_model) &&
    identical(entry$filter, filter_model) &&
    identical(entry$source, rows)) {
    return(entry$processed)
  }
  processed <- .apply_sort_filter(rows, params)
  cache[[inputId]] <- list(
    sort = sort_model, filter = filter_model,
    source = rows, processed = processed
  )
  session$userData$.datagrid_sortfilter_cache <- cache
  processed
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
#' @section Lifecycle:
#' \strong{Experimental.} \code{DataGridServer()} (together with
#' \code{\link{processGridParams}}) is \emph{specific to this R package} and has
#' no equivalent in MUI X Data Grid: MUI ships only the building blocks
#' (\code{paginationMode = "server"} plus callbacks) and leaves the data layer
#' to you. This wrapper supplies that layer in R, and in doing so encodes a
#' number of opinionated decisions that may change in future releases. Pin the
#' package version if you rely on the current behavior. Decisions worth
#' knowing about:
#' \itemize{
#'   \item Mode is selected by the presence of \code{rowCount}: supply it to
#'     pass a pre-sliced page (manual mode); omit it to let the full
#'     \code{rows} be paginated automatically.
#'   \item Changing the sort or any filter resets the grid to the first page.
#'   \item Unrecognized filter operators pass all rows through with a warning
#'     (see \code{\link{processGridParams}}).
#'   \item When \code{rows} has no \code{id} column, ids are generated
#'     positionally and are \emph{not} stable across sort/filter changes.
#'     Supply a stable, unique \code{id} column if you use row selection.
#'   \item \code{initialPageSize}, \code{initialState}, and any initial sort or
#'     filter seed the grid \emph{only on the first render}. The React component
#'     reads them once when it mounts, so changing them reactively afterwards
#'     (e.g. from a \code{selectInput}) has no effect on the already-mounted
#'     grid. To change page size after mount, drive it from the grid's own
#'     controls rather than re-rendering with a new \code{initialPageSize}.
#'     A \code{paginationModel} inside a user-supplied \code{initialState} is
#'     honored by the first automatic render, so the served page matches what
#'     the grid displays from the start.
#'   \item A running Shiny session is required: in a static document the grid
#'     renders its first page but pagination, sorting, and filtering have
#'     nothing to respond to them. Use \code{DataGrid()} for static documents.
#' }
#'
#' @param inputId Character. The Shiny input ID. When pagination, sorting, or
#'   filtering changes, the new state is available as \code{input$<inputId>}
#'   in the server. The value is a list with elements \code{pagination_model}
#'   (list with \code{page} and \code{pageSize}), \code{sort_model} (list of
#'   sort items), and \code{filter_model} (list with \code{items},
#'   \code{logicOperator}, and — when the toolbar quick filter is used —
#'   \code{quickFilterValues}).
#' @param rows A data.frame. Pass the \strong{full} dataset (like
#'   \code{DataGrid()}) and let \code{DataGridServer()} handle pagination
#'   automatically, or pass a pre-sliced page together with an explicit
#'   \code{rowCount} for manual control.
#' @param columns Column definitions (list of lists). If NULL, auto-generated
#'   from \code{names(rows)}, with each column's \code{type} inferred from its
#'   R class (see \code{\link{DataGrid}}).
#' @param rowCount Integer. When provided, \code{rows} is assumed to be
#'   already paginated and \code{rowCount} is used as the total row count
#'   (manual mode). When \code{NULL} (default), pagination is handled
#'   automatically from the full \code{rows} dataset.
#' @param loading Logical. Whether to show the loading indicator. If
#'   \code{NULL}, MUI defaults to \code{FALSE}.
#' @param initialPageSize Integer. Convenience for setting the initial page
#'   size. Builds MUI's \code{initialState} prop. If \code{NULL}, MUI defaults
#'   to 100. Also sets the page size for the first automatic render before the
#'   grid has sent state. Must be included in \code{pageSizeOptions}. Ignored
#'   (with a warning) when you pass your own \code{initialState} via
#'   \code{...} — the \code{paginationModel} in your \code{initialState} then
#'   governs both the grid and the first automatic render.
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
  .validate_rows(rows)
  # Validate against the *effective* options: when pageSizeOptions is not
  # supplied, MUI defaults to c(25, 50, 100), so an initialPageSize outside that
  # set would still fail silently in the browser. Check against the default too.
  if (!is.null(initialPageSize)) {
    effective_options <- pageSizeOptions %||% c(25L, 50L, 100L)
    if (!initialPageSize %in% effective_options) {
      stop(
        "initialPageSize (",
        initialPageSize,
        ") is not in pageSizeOptions (",
        paste(effective_options, collapse = ", "),
        "). MUI requires the initial page size to be included in pageSizeOptions."
      )
    }
  }
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
  # A user-supplied initialState (via ...) wins; otherwise build one from
  # initialPageSize. Including both would create a duplicate-named prop in which
  # the built one silently shadows the user's, so we keep exactly one — and the
  # first automatic render slices the page that initialState describes (see
  # below), so initialPageSize plays no role at all when initialState is given.
  user_initial_state <- "initialState" %in% names(dots)
  if (user_initial_state && !is.null(initialPageSize)) {
    warning(
      "Both 'initialState' (via ...) and 'initialPageSize' were supplied. ",
      "Using your 'initialState' (it seeds the grid and the first automatic ",
      "render); 'initialPageSize' is ignored.",
      call. = FALSE
    )
  }
  session <- shiny::getDefaultReactiveDomain()
  # Without a Shiny session (static R Markdown/Quarto, console) the grid still
  # renders its first page, but pagination/sort/filter clicks have nothing to
  # respond to them. Only signal this where a user can see it — interactively
  # or during a knitr render — so programmatic use (tests) stays quiet.
  if (is.null(session) &&
    (interactive() || isTRUE(getOption("knitr.in.progress")))) {
    message(
      "DataGridServer() needs a running Shiny session to respond to ",
      "pagination, sorting, and filtering; only the first page is shown. ",
      "For static documents use DataGrid() instead."
    )
  }
  if (!is.null(session)) {
    # Detect two *different live* outputs sharing one inputId (their Shiny
    # inputs would otherwise clobber each other). The registry is reset at the
    # end of every flush (scheduled below), so entries owned by outputs that are
    # no longer rendered — e.g. removed via removeUI() or replaced by a
    # renderUI() swap — do not linger and falsely reject a freed inputId on a
    # later flush. Genuine duplicates still collide because conflicting outputs
    # render within the same flush.
    registry <- session$userData$.datagrid_input_registry
    if (is.null(registry)) registry <- list()
    current_output <- shiny::getCurrentOutputInfo()$name
    claimed_by <- registry[[inputId]]
    if (!is.null(claimed_by) && !identical(claimed_by, current_output)) {
      stop(
        'inputId "', inputId, '" is already used by output "', claimed_by, '". ',
        "Each DataGridServer() must have a unique inputId."
      )
    }
    registry[[inputId]] <- current_output
    session$userData$.datagrid_input_registry <- registry
    # Schedule a single reset for the end of this flush (guarded so we register
    # at most one callback per flush regardless of how many grids render).
    if (is.null(session$userData$.datagrid_registry_reset_scheduled)) {
      session$userData$.datagrid_registry_reset_scheduled <- TRUE
      session$onFlushed(
        function() {
          session$userData$.datagrid_input_registry <- list()
          session$userData$.datagrid_registry_reset_scheduled <- NULL
        },
        once = TRUE
      )
    }
  }
  # Manual mode (rowCount supplied) expects a single pre-sliced page. A page
  # can never contain more rows than the total, so nrow(rows) > rowCount almost
  # always means the full, unsliced dataset was passed by mistake.
  if (!is.null(rowCount) && !is.null(rows) && nrow(rows) > rowCount) {
    warning(
      "rowCount (", rowCount, ") is smaller than nrow(rows) (", nrow(rows),
      "). In manual mode 'rows' must be a single pre-sliced page. Did you mean ",
      "to omit 'rowCount' so DataGridServer() paginates the full dataset ",
      "automatically?",
      call. = FALSE
    )
  }
  # Automatic mode: rowCount not supplied, so sort/filter/paginate internally.
  # The sort/filter pass is memoized per (session, inputId) so a pagination-only
  # change re-slices a cached frame instead of re-sorting the whole dataset.
  if (is.null(rowCount) && !is.null(rows)) {
    params <- if (!is.null(session)) session$input[[inputId]] else NULL
    # Before the grid has sent any state (params NULL), slice the exact page the
    # React component will display. It seeds its controlled paginationModel from
    # the initialState prop it receives — the user's own (via ...) or the one
    # built from initialPageSize — falling back to MUI's defaults (page 0,
    # pageSize 100); see ServerSideDataGrid.jsx. Mirroring that here keeps the
    # served rows and the pagination footer consistent on first paint.
    init_pm <- if (user_initial_state) {
      dots$initialState$pagination$paginationModel
    } else if (!is.null(initialPageSize)) {
      list(page = 0L, pageSize = as.integer(initialPageSize))
    }
    page      <- params$pagination_model$page     %||% init_pm$page     %||% 0L
    page_size <- params$pagination_model$pageSize %||% init_pm$pageSize %||% 100L
    processed <- .memoized_sort_filter(session, inputId, rows, params)
    result   <- .slice_page(processed, page, page_size)
    rows     <- result$rows
    rowCount <- result$rowCount
  }
  if (is.null(columns) && !is.null(rows)) {
    columns <- .auto_columns(rows)
  } else if (!is.null(columns)) {
    columns <- .fill_column_types(columns, rows)
  }
  rows <- .inject_id(rows)
  base <- list(
    inputId = inputId,
    rows = rows,
    columns = columns,
    rowCount = if (!is.null(rowCount)) as.integer(rowCount),
    loading = loading,
    pageSizeOptions = if (!is.null(pageSizeOptions)) {
      as.list(as.integer(pageSizeOptions))
    },
    filterDebounce = filterDebounce,
    paginationMode = "server",
    sortingMode = "server",
    filterMode = "server",
    pagination = TRUE
  )
  if (!user_initial_state && !is.null(initialPageSize)) {
    base$initialState <- list(
      pagination = list(
        paginationModel = list(
          page = 0L,
          pageSize = as.integer(initialPageSize)
        )
      )
    )
  }
  props <- c(base, dots)
  tag <- shiny.react::reactElement(
    module = "@muiDataGrid/custom",
    name = "ServerSideDataGrid",
    props = do.call(shiny.react::asProps, props),
    deps = muiDataGridDependency()
  )
  class(tag) <- c("muiDataGrid", class(tag))
  tag
}
