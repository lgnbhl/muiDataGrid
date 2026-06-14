get_props <- function(result) {
  attr(result, "reactData")$props$value
}

test_that("DataGrid returns a shiny.tag", {
  df <- data.frame(name = c("Luke", "Leia"), height = c(172, 150))
  result <- DataGrid(rows = df)
  expect_s3_class(result, "shiny.tag")
})

test_that("DataGrid targets correct React module and component", {
  df <- data.frame(name = c("Luke", "Leia"), height = c(172, 150))
  rd <- attr(DataGrid(rows = df), "reactData")
  expect_equal(rd$module, "@mui/x-data-grid")
  expect_equal(rd$name, "DataGrid")
})

test_that("DataGrid auto-generates id column when missing", {
  df <- data.frame(name = c("Luke", "Leia"), height = c(172, 150))
  rows <- get_props(DataGrid(rows = df))$rows
  # asProps converts data.frame rows to list of row objects
  expect_true("id" %in% names(rows[[1]]))
})

test_that("DataGrid preserves existing id column", {
  df <- data.frame(id = c(10, 20), name = c("Luke", "Leia"))
  rows <- get_props(DataGrid(rows = df))$rows
  expect_equal(rows[[1]]$id, 10)
  expect_equal(rows[[2]]$id, 20)
})

test_that("DataGrid auto-generates columns from data frame names", {
  df <- data.frame(name = c("Luke"), height = c(172))
  columns <- get_props(DataGrid(rows = df))$columns
  fields <- vapply(columns, function(c) c$field, character(1))
  expect_equal(fields, c("name", "height"))
})

test_that("DataGrid infers number and boolean column types", {
  # Only number and boolean are inferred (their JSON serialization matches the
  # MUI type). Character, factor, Date, and POSIXct all stay "string": dates
  # reach the browser as strings, and MUI's date types expect JS Date objects.
  df <- data.frame(
    name = "Luke",
    height = 172,
    active = TRUE,
    rank = factor("jedi", levels = c("jedi", "sith")),
    born = as.Date("2024-01-01"),
    seen = as.POSIXct("2024-01-01 10:00", tz = "UTC")
  )
  columns <- get_props(DataGrid(rows = df))$columns
  types <- vapply(columns, function(c) c$type, character(1))
  expect_equal(
    types,
    c("string", "number", "boolean", "string", "string", "string")
  )
})

test_that("DataGrid warns on duplicate id values", {
  df <- data.frame(id = c(1, 1), name = c("Luke", "Leia"))
  expect_warning(DataGrid(rows = df), "unique")
})

test_that("DataGrid uses custom columns when provided", {
  df <- data.frame(name = c("Luke"), height = c(172))
  cols <- list(list(field = "name", headerName = "Name"))
  columns <- get_props(DataGrid(rows = df, columns = cols))$columns
  expect_equal(columns[[1]]$field, "name")
  expect_equal(columns[[1]]$headerName, "Name")
})

test_that("DataGrid fills missing type in explicit data.frame columns", {
  df <- data.frame(name = "Luke", height = 172, mass = 77)
  cols <- data.frame(
    field = c("name", "height", "mass"),
    headerName = c("Name", "Height", "Mass")
  )
  columns <- get_props(DataGrid(rows = df, columns = cols))$columns
  types <- vapply(columns, function(c) c$type, character(1))
  expect_equal(types, c("string", "number", "number"))
})

test_that("DataGrid fills only the unset rows of a partial data.frame type column", {
  df <- data.frame(name = "Luke", height = 172, mass = 77)
  # height carries an explicit (forced) type; name and mass are left NA and
  # should be inferred — matching the per-column behavior of the list form.
  cols <- data.frame(
    field = c("name", "height", "mass"),
    type = c(NA, "string", NA),
    stringsAsFactors = FALSE
  )
  columns <- get_props(DataGrid(rows = df, columns = cols))$columns
  types <- vapply(columns, function(c) c$type, character(1))
  expect_equal(types, c("string", "string", "number"))
})

test_that("DataGrid fills missing type in explicit list columns", {
  df <- data.frame(name = "Luke", height = 172, active = TRUE)
  cols <- list(
    list(field = "name", headerName = "Name"),
    list(field = "height"),
    list(field = "active")
  )
  columns <- get_props(DataGrid(rows = df, columns = cols))$columns
  # string is MUI's default, so it is left unset; number/boolean are filled
  expect_null(columns[[1]]$type)
  expect_equal(columns[[2]]$type, "number")
  expect_equal(columns[[3]]$type, "boolean")
})

test_that("DataGrid respects an explicit column type", {
  df <- data.frame(name = "Luke", height = 172)
  # height is numeric but the user forces string — keep their choice
  cols <- list(
    list(field = "name"),
    list(field = "height", type = "string")
  )
  columns <- get_props(DataGrid(rows = df, columns = cols))$columns
  expect_equal(columns[[2]]$type, "string")

  # same for a data.frame spec that already carries a type column
  cols_df <- data.frame(field = c("name", "height"), type = c("string", "string"))
  columns2 <- get_props(DataGrid(rows = df, columns = cols_df))$columns
  types <- vapply(columns2, function(c) c$type, character(1))
  expect_equal(types, c("string", "string"))
})

test_that("DataGrid passes extra props", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGrid(rows = df, autoHeight = TRUE))
  expect_true(props$autoHeight)
})

test_that("DataGrid excludes existing id column from auto-generated columns", {
  df <- data.frame(id = 1:2, name = c("Luke", "Leia"), height = c(172, 150))
  columns <- get_props(DataGrid(rows = df))$columns
  fields <- vapply(columns, function(c) c$field, character(1))
  expect_false("id" %in% fields)
  expect_equal(fields, c("name", "height"))
})

test_that("DataGrid handles NULL rows", {
  result <- DataGrid(rows = NULL)
  expect_s3_class(result, "shiny.tag")
  expect_null(get_props(result)$columns)
})
