# muiDataGrid 0.1.2

* `DataGrid()` now infers MUI column types from R classes (numeric columns
  become `number`, logical columns `boolean`) and warns when a supplied `id`
  column contains duplicate values.
* Added server-side support via `DataGridServer()` and `processGridParams()`,
  which apply pagination, sorting, and filtering in R and send only the current
  page to the browser. The sort/filter pass is memoized so paginating a large
  dataset no longer re-sorts every row on each page change.
* `DataGridServer()` hardened: validates `initialPageSize` against
  `pageSizeOptions`, rejects two live outputs sharing one `inputId`, warns when
  `nrow(rows)` exceeds a manual `rowCount`, and strips protected props
  (`paginationMode`, `sortingMode`, `filterMode`, `pagination`).
* Filter input in the server grid is now debounced independently of pagination
  and sorting changes.
* Bundled MUI X Data Grid locales for multi-language support.
* Require `muiMaterial` (>= 0.2.0) and warn at attach if an incompatible
  version is installed, since the grid shares its bundled MUI runtime.
* Documented attribution and licensing for the bundled MUI X Data Grid
  JavaScript (see `LICENSE.note`).

# muiDataGrid 0.1.0

* Initial release.
