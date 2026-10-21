import React, { useEffect, useRef, useMemo } from 'react';
import {
  DataGrid,
  useGridApiRef,
  gridColumnVisibilityModelSelector,
  gridFilterActiveItemsSelector,
  gridFilterModelSelector,
} from '@mui/x-data-grid';

// Server-side grid backed by MUI's own data source: MUI owns the pagination,
// sort, and filter state, resets to the first page, forces the server modes,
// shows the loading overlay, and drops stale responses. This component only
// answers MUI's getRows(), by POSTing each request to the HTTP endpoint
// DataGridServer() registered (`dataUrl`).

// How long a request waits for a replacement before it is sent (see
// makeRowSource). Small next to a round-trip to R.
const COALESCE_MS = 50;

// The columns MUI's quick filter searches: visible ones (unless the model opts
// out via quickFilterExcludeHiddenColumns) whose type has a quick filter
// function, which booleans and action columns lack. Sent to R so
// processGridParams() searches the same columns; undefined when there is no
// quick filter to apply.
function quickFilterFields(filterModel, apiRef) {
  if (!filterModel.quickFilterValues?.some(Boolean)) return undefined;
  const excludeHidden = filterModel.quickFilterExcludeHiddenColumns ?? true;
  const visibility = gridColumnVisibilityModelSelector(apiRef);
  return apiRef.current.getAllColumns()
    .filter((col) => !excludeHidden || visibility[col.field] !== false)
    .filter((col) => col.getApplyQuickFilterFn?.('1', col, apiRef) != null)
    .map((col) => ({ field: col.field, type: col.type ?? 'string' }));
}

// MUI's request in the shape R reads (input$<inputId>, getRows, and
// processGridParams()). getRows() runs on the grid's current state, so MUI's
// own selector gives the filter items it applies: inactive ones (an operator
// picked but no value yet, the row the filter panel adds on open) are left out.
function toRequest(params, apiRef) {
  const filterModel = { ...params.filterModel, items: gridFilterActiveItemsSelector(apiRef) };
  const request = {
    pagination_model: params.paginationModel,
    sort_model: params.sortModel,
    filter_model: filterModel,
  };
  const fields = quickFilterFields(filterModel, apiRef);
  if (fields) request.quick_filter_fields = fields;
  return request;
}

// Wraps `send(request) -> Promise<{ rows, rowCount }>` for MUI's getRows():
// - A request identical to the last one answered (only inactive filter items
//   changed, e.g. the filter panel opened) is answered from that answer.
// - A sort or filter change away from page 0 makes MUI request twice: once for
//   the old page, then, a few ms later after it resets to page 0, for the new
//   one. Holding each request briefly lets the second replace the first, so R
//   answers once; the replaced request gets the same answer, which MUI
//   discards as stale.
function makeRowSource(send) {
  let timer = null;
  let waiting = [];
  let last = null;
  return (request) => {
    const key = JSON.stringify(request);
    if (last?.key === key && waiting.length === 0) {
      return Promise.resolve(last.answer);
    }
    return new Promise((resolve, reject) => {
      waiting.push({ resolve, reject });
      clearTimeout(timer);
      timer = setTimeout(() => {
        const batch = waiting;
        waiting = [];
        send(request).then(
          (answer) => {
            last = { key, answer };
            batch.forEach((w) => w.resolve(answer));
          },
          (error) => batch.forEach((w) => w.reject(error)),
        );
      }, COALESCE_MS);
    });
  };
}

// POST the request to DataGridServer()'s endpoint; it answers with the page.
// The state is also posted as input$<inputId> for observers in the app.
function postTo(dataUrl, inputId) {
  return async (request) => {
    window.Shiny.setInputValue(inputId, request);
    const response = await fetch(dataUrl, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(request),
    });
    const body = await response.json().catch(() => ({}));
    if (!response.ok) throw new Error(body.error ?? `Request failed (${response.status})`);
    return body;
  };
}

function ServerSideDataGrid({
  inputId, dataUrl, rows, rowCount, loading, filterDebounce, ...otherProps
}) {
  const apiRef = useGridApiRef();
  // The rows R rendered: the only page there is without an endpoint (static
  // documents, or a grid without data).
  const rendered = useRef({ rows, rowCount });
  rendered.current = { rows, rowCount };

  // A new dataUrl (R re-rendered the grid, e.g. because its data changed) is a
  // new data source, which makes MUI reload the current page.
  const dataSource = useMemo(() => {
    if (!dataUrl || !window.Shiny?.setInputValue) {
      return { getRows: async () => rendered.current };
    }
    const source = makeRowSource(postTo(dataUrl, inputId));
    return { getRows: (params) => source(toRequest(params, apiRef)) };
  }, [dataUrl, inputId, apiRef]);

  // Hiding or showing a column changes which columns the quick filter searches,
  // but MUI does not refetch on it.
  useEffect(() => apiRef.current?.subscribeEvent('columnVisibilityModelChange', () => {
    if (gridFilterModelSelector(apiRef).quickFilterValues?.some(Boolean)) {
      apiRef.current.dataSource.fetchRows();
    }
  }), [apiRef]);

  return (
    <DataGrid
      filterDebounceMs={filterDebounce ?? 300}
      {...otherProps}
      {...(loading != null && { loading })}
      apiRef={apiRef}
      dataSource={dataSource}
      dataSourceCache={null}
    />
  );
}

export { ServerSideDataGrid };
