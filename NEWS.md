# muiDataGrid 0.2.0

## Breaking changes

* `DataGridServer()` no longer accepts a pre-sliced page with `rowCount`
  (the former "manual mode", re-rendered from `input$<inputId>`); passing
  `rowCount` is now an error that explains the replacement. In that mode the
  rows came from whatever input R had last read, while the grid's footer
  described the page the user was on, so a slow or debounced reactive could
  show rows from a different page or request. Produce each page with the new
  `getRows` argument instead, e.g.
  `getRows = function(params) processGridParams(data, params)` (with
  `columns`), or pass the full data frame as `rows`. See "Migrating from
  rowCount" in `vignette("server-side-data")`.
* `input$<inputId>$filter_model$items` now holds only the filters MUI
  applies, as given by MUI's own selector (an item still waiting for a value
  is left out).
* `DataGridServer()` no longer sets `paginationMode`, `sortingMode`,
  `filterMode`, and `pagination`: MUI's data source forces them. Passing them
  still warns that they are ignored.
* `DataGridServer()` no longer errors when two grids share one `inputId`.
  As with any Shiny input, each grid needs its own `inputId`.

## New features

* `DataGridServer()` is now built on MUI X Data Grid's own server-side data
  source. MUI owns pagination, sort, and filter state and the reset to the
  first page, and shows its loading overlay automatically while it waits for
  R, so the `loading` argument is rarely needed.
* Each page is requested over HTTP, through a session-scoped endpoint
  (`session$registerDataObj()`, as DT does for server-side tables), and
  answers exactly that request; MUI discards answers to requests it has moved
  past. With `rows`, the call is unchanged, paging no longer re-runs the
  `renderReact()` that created the grid, and an error while producing a page
  is reported in the R console instead of breaking the output.
* New `getRows` argument: a function of the grid's request returning
  `list(rows, rowCount)` for that page, mirroring MUI's `dataSource.getRows`.
  Use it to query a database or apply custom logic; `processGridParams()`
  returns exactly this shape.
* The toolbar quick filter is applied server-side, following MUI's rules: it
  searches only visible columns (sent by the grid as `quick_filter_fields`),
  number columns match a term equal to their value, other columns a
  case-insensitive substring, boolean columns are not searched, and terms
  combine per `quickFilterLogicOperator` and are ANDed with regular filter
  items. An initial quick filter in `initialState` is applied on first load.
* `filterDebounceMs` passed via `...` is now honored and takes precedence over
  `filterDebounce`, which now sets it (still 300 ms by default). The toolbar
  quick filter keeps its own debounce.

## Bug fixes: wrong rows or counts shown

* Date `is` filters matched no rows ("0 of 0") and `not` matched every row:
  MUI sends the date picker's value as a timestamp
  (`"2024-01-15T00:00:00.000Z"`), which never equalled the column value.
* An `initialState` with only a `pageSize` showed "No results found." under a
  footer reading e.g. "1–25 of 500".
* `autoPageSize` was ignored: the grid showed 100 rows instead of the number
  that fits its height. The grid now requests its first page itself, so it
  opens on exactly the page its `initialState` and `autoPageSize` describe;
  the first page arrives one round-trip later.
* The boolean `is` filter matched no rows on logical columns (MUI sends
  `"true"`/`"false"`, never equal to R's `"TRUE"`/`"FALSE"`).
* Filter items with an empty value (cleared text input, all `isAnyOf` chips
  removed, boolean "any") are ignored, matching MUI. Previously an emptied
  number filter blanked the whole grid.

## Other changes

* Opening the filter panel or picking an operator before typing a value no
  longer jumps back to the first page or triggers a round-trip to R.
* `DataGrid()` and `DataGridServer()` now reject non-data.frame `rows` with a
  clear error, and the duplicate-id warning also covers `NA` ids.
* When both `initialState` and `initialPageSize` are given, `initialState`
  wins and `initialPageSize` is ignored (with a warning).
* `DataGridServer()` messages when called without a running Shiny session
  (interactively or while knitting), pointing to `DataGrid()` for static use.
* Date filters parse whole columns vectorized (with per-element fallback),
  making date filtering much faster on large data.
* `DataGridServer()` now imports jsonlite, for its HTTP responses.
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
