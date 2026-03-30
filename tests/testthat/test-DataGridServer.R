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
  rd <- attr(DataGridServer(inputId = "grid", rows = df, rowCount = 2L), "reactData")
  expect_equal(rd$module, "@muiDataGrid/custom")
  expect_equal(rd$name, "ServerSideDataGrid")
})

test_that("DataGridServer auto-generates id column when missing", {
  df <- data.frame(name = c("Luke", "Leia"), height = c(172, 150))
  rows <- get_props(DataGridServer(inputId = "grid", rows = df, rowCount = 2L))$rows
  expect_true("id" %in% names(rows[[1]]))
})

test_that("DataGridServer preserves existing id column", {
  df <- data.frame(id = c(10, 20), name = c("Luke", "Leia"))
  rows <- get_props(DataGridServer(inputId = "grid", rows = df, rowCount = 2L))$rows
  expect_equal(rows[[1]]$id, 10)
  expect_equal(rows[[2]]$id, 20)
})

test_that("DataGridServer auto-generates columns when NULL", {
  df <- data.frame(name = c("Luke"), height = c(172))
  columns <- get_props(DataGridServer(inputId = "grid", rows = df, rowCount = 1L))$columns
  fields <- vapply(columns, function(c) c$field, character(1))
  expect_equal(fields, c("name", "height"))
})

test_that("DataGridServer uses custom columns when provided", {
  df <- data.frame(name = c("Luke"), height = c(172))
  cols <- list(list(field = "name", headerName = "Name"))
  columns <- get_props(DataGridServer(inputId = "grid", rows = df, columns = cols, rowCount = 1L))$columns
  expect_equal(columns[[1]]$field, "name")
  expect_equal(columns[[1]]$headerName, "Name")
})

test_that("DataGridServer sets inputId prop", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(inputId = "my_grid", rows = df, rowCount = 1L))
  expect_equal(props$inputId, "my_grid")
})

test_that("DataGridServer converts rowCount to integer", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(inputId = "grid", rows = df, rowCount = 100.5))
  expect_type(props$rowCount, "integer")
})

test_that("DataGridServer converts pageSizeOptions to list of integers", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGridServer(inputId = "grid", rows = df, rowCount = 1L,
    pageSizeOptions = c(5, 10, 25)))
  expect_type(props$pageSizeOptions, "list")
  expect_equal(props$pageSizeOptions, list(5L, 10L, 25L))
})
