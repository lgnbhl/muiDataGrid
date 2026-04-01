df <- data.frame(
  id = 1:20,
  name = paste("Row", 1:20),
  value = 1:20,
  group = rep(c("A", "B"), each = 10)
)

# --- NULL params (initial render) ---

test_that("returns first page with defaults when params is NULL", {
  result <- processGridParams(df, params = NULL)
  expect_equal(nrow(result$rows), 20)
  expect_equal(result$rowCount, 20)
  expect_equal(result$rows$id, 1:20)
})

test_that("respects custom pageSize when params is NULL", {
  result <- processGridParams(df, params = NULL, pageSize = 5L)
  expect_equal(nrow(result$rows), 5)
  expect_equal(result$rows$id, 1:5)
  expect_equal(result$rowCount, 20)
})

# --- Pagination ---

test_that("paginates to correct page", {
  params <- list(
    pagination_model = list(page = 1, pageSize = 5),
    sort_model = list(),
    filter_model = list(items = list())
  )
  result <- processGridParams(df, params)
  expect_equal(nrow(result$rows), 5)
  expect_equal(result$rows$id, 6:10)
})

test_that("returns partial page at end of data", {
  params <- list(
    pagination_model = list(page = 3, pageSize = 7),
    sort_model = list(),
    filter_model = list(items = list())
  )
  result <- processGridParams(df, params)
  # 7*3=21 start, but only 20 rows, so page 3 has rows 22-28 -> empty
  # Actually: start = 3*7+1 = 22 > 20, so empty
  expect_equal(nrow(result$rows), 0)
})

test_that("handles page size larger than dataset", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(items = list())
  )
  result <- processGridParams(df, params)
  expect_equal(nrow(result$rows), 20)
  expect_equal(result$rowCount, 20)
})

# --- Sorting ---

test_that("sorts ascending", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 5),
    sort_model = list(list(field = "value", sort = "asc")),
    filter_model = list(items = list())
  )
  result <- processGridParams(df, params)
  expect_equal(result$rows$value, 1:5)
})

test_that("sorts descending", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 5),
    sort_model = list(list(field = "value", sort = "desc")),
    filter_model = list(items = list())
  )
  result <- processGridParams(df, params)
  expect_equal(result$rows$value, 20:16)
})

test_that("ignores sort on non-existent field", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 5),
    sort_model = list(list(field = "nonexistent", sort = "asc")),
    filter_model = list(items = list())
  )
  result <- processGridParams(df, params)
  expect_equal(result$rows$id, 1:5)
})

# --- Filtering ---

test_that("filters with contains operator", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "name", operator = "contains", value = "Row 1")
      )
    )
  )
  result <- processGridParams(df, params)
  # Matches "Row 1", "Row 10", ..., "Row 19"
  expect_equal(result$rowCount, 11)
  expect_true(all(grepl("Row 1", result$rows$name)))
})

test_that("filters with equals operator", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "group", operator = "equals", value = "A")
      )
    )
  )
  result <- processGridParams(df, params)
  expect_equal(result$rowCount, 10)
  expect_true(all(result$rows$group == "A"))
})

test_that("filters with > operator", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "value", operator = ">", value = "15")
      )
    )
  )
  result <- processGridParams(df, params)
  expect_equal(result$rowCount, 5)
  expect_true(all(result$rows$value > 15))
})

test_that("filters with < operator", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "value", operator = "<", value = "5")
      )
    )
  )
  result <- processGridParams(df, params)
  expect_equal(result$rowCount, 4)
  expect_true(all(result$rows$value < 5))
})

test_that("filters with startsWith operator", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "name", operator = "startsWith", value = "Row 2")
      )
    )
  )
  result <- processGridParams(df, params)
  expect_true(all(grepl("^Row 2", result$rows$name)))
})

test_that("filters with isEmpty operator", {
  df_na <- data.frame(id = 1:3, val = c("a", NA, ""))
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "val", operator = "isEmpty", value = "unused")
      )
    )
  )
  result <- processGridParams(df_na, params)
  expect_equal(result$rowCount, 2)
})

test_that("filters with isNotEmpty operator", {
  df_na <- data.frame(id = 1:3, val = c("a", NA, ""))
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "val", operator = "isNotEmpty", value = "unused")
      )
    )
  )
  result <- processGridParams(df_na, params)
  expect_equal(result$rowCount, 1)
  expect_equal(result$rows$val, "a")
})

test_that("ignores filter with NULL value", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "group", operator = "equals", value = NULL)
      )
    )
  )
  result <- processGridParams(df, params)
  expect_equal(result$rowCount, 20)
})

test_that("ignores filter on non-existent field", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "nonexistent", operator = "equals", value = "A")
      )
    )
  )
  result <- processGridParams(df, params)
  expect_equal(result$rowCount, 20)
})

test_that("unknown operator keeps all rows", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 100),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "group", operator = "unknownOp", value = "A")
      )
    )
  )
  result <- processGridParams(df, params)
  expect_equal(result$rowCount, 20)
})

# --- Combined sorting + filtering + pagination ---

test_that("combines sort, filter, and pagination", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 3),
    sort_model = list(list(field = "value", sort = "desc")),
    filter_model = list(
      items = list(
        list(field = "group", operator = "equals", value = "A")
      )
    )
  )
  result <- processGridParams(df, params)
  # Group A has ids 1-10, sorted desc by value -> 10,9,8,...
  # Page 0 with pageSize 3 -> values 10, 9, 8
  expect_equal(result$rowCount, 10)
  expect_equal(nrow(result$rows), 3)
  expect_equal(result$rows$value, c(10, 9, 8))
})

# --- rowCount reflects filtered total, not page size ---

test_that("rowCount is total after filter, not page count", {
  params <- list(
    pagination_model = list(page = 0, pageSize = 3),
    sort_model = list(),
    filter_model = list(
      items = list(
        list(field = "group", operator = "equals", value = "B")
      )
    )
  )
  result <- processGridParams(df, params)
  expect_equal(result$rowCount, 10)
  expect_equal(nrow(result$rows), 3)
})
