get_props <- function(result) {
  attr(result, "reactData")$props$value
}

test_that("DataGridServer returns a shiny.tag", {
  df <- data.frame(name = c("Luke", "Leia"), height = c(172, 150))
  result <- DataGridServer(inputId = "grid", rows = df, rowCount = 2L)
  expect_s3_class(result, "shiny.tag")
})

test_that("DataGridServer targets correct React module and component", {
  df <- data.frame(name = c("Luke", "Leia"), height = c(172, 150))
  rd <- attr(
    DataGridServer(inputId = "grid", rows = df, rowCount = 2L),
    "reactData"
  )
  expect_equal(rd$module, "@muiDataGrid/custom")
  expect_equal(rd$name, "ServerSideDataGrid")
})

test_that("DataGridServer auto-generates id column when missing", {
  df <- data.frame(name = c("Luke", "Leia"), height = c(172, 150))
  rows <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 2L
  ))$rows
  expect_true("id" %in% names(rows[[1]]))
})

test_that("DataGridServer preserves existing id column", {
  df <- data.frame(id = c(10, 20), name = c("Luke", "Leia"))
  rows <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 2L
  ))$rows
  expect_equal(rows[[1]]$id, 10)
  expect_equal(rows[[2]]$id, 20)
})

test_that("DataGridServer auto-generates columns when NULL", {
  df <- data.frame(name = c("Luke"), height = c(172))
  columns <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L
  ))$columns
  fields <- vapply(columns, function(c) c$field, character(1))
  expect_equal(fields, c("name", "height"))
})

test_that("DataGridServer uses custom columns when provided", {
  df <- data.frame(name = c("Luke"), height = c(172))
  cols <- list(list(field = "name", headerName = "Name"))
  columns <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    columns = cols,
    rowCount = 1L
  ))$columns
  expect_equal(columns[[1]]$field, "name")
  expect_equal(columns[[1]]$headerName, "Name")
})

test_that("DataGridServer sets inputId prop", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "my_grid",
    rows = df,
    rowCount = 1L
  ))
  expect_equal(props$inputId, "my_grid")
})

test_that("DataGridServer converts rowCount to integer", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 100.5
  ))
  expect_type(props$rowCount, "integer")
})

test_that("DataGridServer converts pageSizeOptions to list of integers", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L,
    pageSizeOptions = c(5, 10, 25)
  ))
  expect_type(props$pageSizeOptions, "list")
  expect_equal(props$pageSizeOptions, list(5L, 10L, 25L))
})

test_that("DataGridServer warns and strips protected props from ...", {
  df <- data.frame(id = 1, name = "Luke")
  expect_warning(
    result <- DataGridServer(
      inputId = "grid",
      rows = df,
      rowCount = 1L,
      paginationMode = "client"
    ),
    "paginationMode"
  )
  props <- get_props(result)
  expect_equal(props$paginationMode, "server")
})

test_that("DataGridServer excludes id from auto-generated columns", {
  df <- data.frame(id = 1:2, name = c("Luke", "Leia"), height = c(172, 150))
  columns <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 2L
  ))$columns
  fields <- vapply(columns, function(c) c$field, character(1))
  expect_false("id" %in% fields)
  expect_equal(fields, c("name", "height"))
})

test_that("DataGridServer errors when initialPageSize not in pageSizeOptions", {
  df <- data.frame(id = 1, name = "Luke")
  expect_error(
    DataGridServer(
      inputId = "grid",
      rows = df,
      rowCount = 1L,
      initialPageSize = 15L,
      pageSizeOptions = c(10L, 25L, 50L)
    ),
    "initialPageSize"
  )
})

test_that("DataGridServer errors when initialPageSize not in default pageSizeOptions", {
  # pageSizeOptions omitted -> MUI defaults to c(25, 50, 100); 15 is not in it.
  df <- data.frame(id = 1, name = "Luke")
  expect_error(
    DataGridServer(
      inputId = "grid",
      rows = df,
      rowCount = 1L,
      initialPageSize = 15L
    ),
    "initialPageSize"
  )
})

test_that("DataGridServer allows initialPageSize in default pageSizeOptions", {
  df <- data.frame(id = 1, name = "Luke")
  expect_s3_class(
    DataGridServer(
      inputId = "grid",
      rows = df,
      rowCount = 1L,
      initialPageSize = 50L
    ),
    "shiny.tag"
  )
})

test_that("DataGridServer builds initialState from initialPageSize", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L,
    initialPageSize = 25L,
    pageSizeOptions = c(25L, 50L)
  ))
  expect_equal(props$initialState$pagination$paginationModel$pageSize, 25L)
  expect_equal(props$initialState$pagination$paginationModel$page, 0L)
})

test_that("DataGridServer initialState is NULL when initialPageSize is NULL", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L
  ))
  expect_null(props$initialState)
})

test_that("DataGridServer sets server-mode props", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L
  ))
  expect_equal(props$paginationMode, "server")
  expect_equal(props$sortingMode, "server")
  expect_equal(props$filterMode, "server")
  expect_true(props$pagination)
})

test_that("DataGridServer passes loading prop", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L,
    loading = TRUE
  ))
  expect_true(props$loading)
})

test_that("DataGridServer passes filterDebounce prop", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L,
    filterDebounce = 500L
  ))
  expect_equal(props$filterDebounce, 500L)
})

test_that("DataGridServer passes extra props via ...", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(
    inputId = "grid",
    rows = df,
    rowCount = 1L,
    density = "compact",
    disableColumnFilter = TRUE
  ))
  expect_equal(props$density, "compact")
  expect_true(props$disableColumnFilter)
})

test_that("DataGridServer handles NULL rows", {
  result <- DataGridServer(inputId = "grid", rows = NULL, rowCount = 0L)
  expect_s3_class(result, "shiny.tag")
  props <- get_props(result)
  expect_null(props$columns)
})

test_that("DataGridServer warns when rows has more rows than rowCount", {
  df <- data.frame(id = 1:10, name = paste("Row", 1:10))
  expect_warning(
    DataGridServer(inputId = "grid", rows = df, rowCount = 5L),
    "smaller than nrow"
  )
})

test_that("DataGridServer does not warn when rows fits within rowCount", {
  df <- data.frame(id = 1:5, name = paste("Row", 1:5))
  expect_no_warning(
    DataGridServer(inputId = "grid", rows = df, rowCount = 100L)
  )
})

test_that("DataGridServer warns and keeps user initialState over initialPageSize", {
  df <- data.frame(id = 1, name = "Luke")
  custom_state <- list(pagination = list(paginationModel = list(page = 2L, pageSize = 5L)))
  expect_warning(
    result <- DataGridServer(
      inputId = "grid",
      rows = df,
      rowCount = 1L,
      initialPageSize = 25L,
      pageSizeOptions = c(25L, 50L),
      initialState = custom_state
    ),
    "Both 'initialState'"
  )
  props <- get_props(result)
  # User's initialState wins; the built one (pageSize 25) must not shadow it.
  expect_equal(props$initialState$pagination$paginationModel$pageSize, 5L)
  expect_equal(props$initialState$pagination$paginationModel$page, 2L)
})

test_that("DataGridServer rejects two live outputs sharing one inputId", {
  df <- data.frame(name = c("a", "b"), height = c(1, 2))
  session <- shiny::MockShinySession$new()
  current <- "outA"
  testthat::local_mocked_bindings(
    getCurrentOutputInfo = function(...) list(name = current),
    .package = "shiny"
  )
  shiny::withReactiveDomain(session, {
    current <- "outA"
    DataGridServer("grid", rows = df, rowCount = 2L)
    # A different output claiming the same inputId in the same flush collides.
    current <- "outB"
    expect_error(
      DataGridServer("grid", rows = df, rowCount = 2L),
      "already used by output"
    )
  })
})

test_that("DataGridServer frees an inputId once its owning output stops rendering", {
  # Regression: a removed/replaced output must not permanently reserve its
  # inputId. The end-of-flush registry reset is what releases it; simulate that
  # reset, then a new output reusing the inputId must succeed.
  df <- data.frame(name = c("a", "b"), height = c(1, 2))
  session <- shiny::MockShinySession$new()
  current <- "outA"
  testthat::local_mocked_bindings(
    getCurrentOutputInfo = function(...) list(name = current),
    .package = "shiny"
  )
  shiny::withReactiveDomain(session, {
    current <- "outA"
    DataGridServer("grid", rows = df, rowCount = 2L)
    expect_equal(session$userData$.datagrid_input_registry[["grid"]], "outA")
    # Emulate the scheduled end-of-flush reset.
    session$userData$.datagrid_input_registry <- list()
    session$userData$.datagrid_registry_reset_scheduled <- NULL
    # outA is gone; outC reuses "grid" on a later flush — must not error.
    current <- "outC"
    expect_error(DataGridServer("grid", rows = df, rowCount = 2L), NA)
    expect_equal(session$userData$.datagrid_input_registry[["grid"]], "outC")
  })
})
