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

test_that("DataGrid uses custom columns when provided", {
  df <- data.frame(name = c("Luke"), height = c(172))
  cols <- list(list(field = "name", headerName = "Name"))
  columns <- get_props(DataGrid(rows = df, columns = cols))$columns
  expect_equal(columns[[1]]$field, "name")
  expect_equal(columns[[1]]$headerName, "Name")
})

test_that("DataGrid passes extra props", {
  df <- data.frame(id = 1, name = "Luke")
  props <- get_props(DataGrid(rows = df, autoHeight = TRUE))
  expect_true(props$autoHeight)
})
