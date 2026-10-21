# muiDataGrid (development version)

* `DataGridServer()` is now built on MUI X Data Grid's own server-side data
  source instead of a custom state layer. The R API is unchanged. MUI now owns
  pagination, sort, and filter state and the reset to the first page; slow or
  out-of-order responses can no longer show a stale page; and the grid shows
  its loading overlay automatically while it waits for R, so the `loading`
  argument is rarely needed. `input$<inputId>` gains a `request_id`, which
  `DataGridServer()` sends back automatically.
* In a Shiny session the grid now requests its first page from R as soon as
  it mounts, instead of R guessing which page it opens on. Grids using
  `autoPageSize` or an `initialState` with only a `pageSize` now open on the
  right page; the first page arrives one round-trip later.
* `input$<inputId>$filter_model$items` now holds only the filters MUI
  applies, as given by MUI's own selector (an item still waiting for a value
  is left out).
* `filterDebounce` now sets MUI's `filterDebounceMs`: the delay after typing
  in the filter panel (still 300 ms by default). It is superseded by passing
  `filterDebounceMs` itself via `...`, which takes precedence. The toolbar
  quick filter keeps its own debounce.
* `DataGridServer()` no longer sets `paginationMode`, `sortingMode`,
  `filterMode`, and `pagination`: MUI's data source forces them. Passing them
  still warns that they are ignored.
* `DataGridServer()` no longer errors when two grids share one `inputId`.
  As with any Shiny input, each grid needs its own `inputId`.

* Server-side filtering fixes and additions:
  * The boolean `is` filter now matches logical columns (MUI sends
    `"true"`/`"false"`, which previously matched nothing against R's
    `TRUE`/`FALSE`).
  * Filter items with an empty value (cleared text input, all `isAnyOf` chips
    removed, boolean "any") are now ignored, matching MUI's client-side
    behavior. Previously an emptied number filter could blank the whole grid.
  * The toolbar quick filter is now applied server-side, following MUI's
    rules: it searches only visible columns (sent by the grid as
    `quick_filter_fields` in `input$<inputId>`), number columns match a term
    equal to their value, other columns a case-insensitive substring, boolean
    columns are not searched, and terms combine per
    `quickFilterLogicOperator` and are ANDed with regular filter items.
  * Date `is`/`not` filters now work: MUI sends the date picker's value as a
    timestamp (`"2024-01-15T00:00:00.000Z"`), which previously never matched.
  * Opening the filter panel or picking an operator before typing a value no
    longer jumps back to the first page or triggers a round-trip to R; only
    filter changes that affect the result do.
  * An initial quick filter supplied via `initialState` is now applied on
    first render.
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
