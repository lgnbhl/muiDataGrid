# muiDataGrid (development version)

* Server-side filtering fixes and additions:
  * The boolean `is` filter now matches logical columns (MUI sends
    `"true"`/`"false"`, which previously matched nothing against R's
    `TRUE`/`FALSE`).
  * Filter items with an empty value (cleared text input, all `isAnyOf` chips
    removed, boolean "any") are now ignored, matching MUI's client-side
    behavior. Previously an emptied number filter could blank the whole grid.
  * The toolbar quick filter is now applied server-side: terms match
    case-insensitively against every column except `id`, combine per
    `quickFilterLogicOperator`, and are ANDed with regular filter items.
* The first automatic render of `DataGridServer()` now slices the page
  described by a user-supplied `initialState` (its `paginationModel`), so the
  served rows match the grid's pagination footer on first paint. When both
  `initialState` and `initialPageSize` are given, `initialState` wins and
  `initialPageSize` is ignored (with a warning).
* `DataGrid()` and `DataGridServer()` now reject non-data.frame `rows` with a
  clear error, and the duplicate-id warning also covers `NA` ids.
* `DataGridServer()` messages when called without a running Shiny session
  (interactively or while knitting), pointing to `DataGrid()` for static use.
* Date filters parse whole columns vectorized (with per-element fallback),
  making date filtering much faster on large data.
* JavaScript build now uses yarn/`yarn.lock` exclusively, matching CI and
  `reinstall.sh` (removed the stale `package-lock.json`, which was never used
  for builds and had drifted; a `js/.npmrc` prevents npm from recreating it).
  Added `@mui/x-internals` to `resolutions`: version 9.8.0 (still within the
  declared `^9.1.0` range) removed platform exports that the bundled
  `@mui/x-virtualizer` needs, breaking fresh builds. The `react-is` 18 pin is
  kept and now documented in `js/webpack.config.js` — react-is 19 cannot
  detect React 18 elements, and shiny.react provides a React 18 runtime.

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
