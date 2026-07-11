# --- .as_date (soft date parsing used by date filters) ---

test_that(".as_date parses valid dates and returns NA for unparseable values", {
  out <- muiDataGrid:::.as_date(c("2024-01-01", "not a date", NA, ""))
  expect_s3_class(out, "Date")
  expect_equal(out[1], as.Date("2024-01-01"))
  expect_true(all(is.na(out[2:4])))
})

test_that(".as_date returns a length-0 Date for empty input", {
  out <- muiDataGrid:::.as_date(character(0))
  expect_s3_class(out, "Date")
  expect_length(out, 0)
})

test_that(".as_date parses mixed formats element-wise", {
  # The vectorized fast path locks onto the first element's format; the
  # per-element retry must still pick up stragglers in another format.
  out <- muiDataGrid:::.as_date(c("2024-01-15", "2024/02/20"))
  expect_equal(out, as.Date(c("2024-01-15", "2024-02-20")))
})

test_that(".as_date recovers when the first element is unparseable", {
  # An unparseable first element makes the vectorized as.Date() error for the
  # whole vector; later elements must still be parsed individually.
  out <- muiDataGrid:::.as_date(c("garbage", "2024-02-20"))
  expect_true(is.na(out[1]))
  expect_equal(out[2], as.Date("2024-02-20"))
})

test_that(".as_date coerces real Date and factor input", {
  expect_equal(
    muiDataGrid:::.as_date(as.Date("2024-06-15")),
    as.Date("2024-06-15")
  )
  expect_equal(
    muiDataGrid:::.as_date(factor("2024-06-15")),
    as.Date("2024-06-15")
  )
})

# --- print.muiDataGrid ---

test_that("print.muiDataGrid prints HTML and returns x invisibly when browse=FALSE", {
  grid <- DataGrid(rows = data.frame(name = "Luke", height = 172))
  expect_output(res <- print(grid, browse = FALSE), "MuiDataGrid|div|data-")
  expect_identical(res, grid)
})

# --- .check_muimaterial_compat / startup warning ---

test_that(".check_muimaterial_compat warns when muiMaterial is too old", {
  testthat::local_mocked_bindings(
    packageVersion = function(pkg) {
      if (identical(pkg, "muiMaterial")) package_version("0.1.0") else package_version("9.9.9")
    },
    .package = "utils"
  )
  expect_message(muiDataGrid:::.check_muimaterial_compat(), "older than")
})

test_that(".check_muimaterial_compat is silent when muiMaterial is recent enough", {
  testthat::local_mocked_bindings(
    packageVersion = function(pkg) package_version("0.2.0"),
    .package = "utils"
  )
  expect_no_message(muiDataGrid:::.check_muimaterial_compat())
})

test_that(".MUIMATERIAL_MIN_VERSION stays in sync with DESCRIPTION", {
  # DESCRIPTION enforces the hard minimum at install/load time; the constant in
  # zzz.R is the soft known-good floor and may only run ahead of it. If this
  # fails, the bundled @mui/x-data-grid was bumped without updating one of the
  # two places (see R/zzz.R).
  desc_path <- system.file("DESCRIPTION", package = "muiDataGrid")
  skip_if(identical(desc_path, ""), "DESCRIPTION not found")
  imports <- read.dcf(desc_path, fields = "Imports")[1, 1]
  match <- regmatches(
    imports,
    regexec("muiMaterial \\(>=\\s*([0-9.]+)\\)", imports)
  )[[1]]
  expect_length(match, 2)
  expect_true(
    package_version(muiDataGrid:::.MUIMATERIAL_MIN_VERSION) >=
      package_version(match[2])
  )
})
