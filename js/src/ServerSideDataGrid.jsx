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
// bridges MUI's getRows() promise to Shiny: each request is posted as
// input$<inputId> with a request_id, R re-renders with that page, and
// DataGridServer() echoes the id back as `requestId` to resolve it.

// How long a request waits for a replacement before it is posted to R (see
// getRows). Small next to a Shiny round-trip.
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

function ServerSideDataGrid({
  inputId, rows, rowCount, requestId, loading, filterDebounce, ...otherProps
}) {
  const apiRef = useGridApiRef();
  // Latest page R rendered, read when a request can be answered locally.
  const served = useRef({ rows, rowCount });
  // Signature of the request `served` answers; null until R answered one.
  const servedSignature = useRef(null);
  const pending = useRef(new Map()); // request id -> { resolve, sig }
  const lastId = useRef(0);
  const sendTimer = useRef(null);

  useEffect(() => () => clearTimeout(sendTimer.current), []);

  const dataSource = useMemo(() => ({
    getRows: (params) => new Promise((resolve) => {
      // Static documents have no Shiny: only the first page R rendered exists.
      if (!window.Shiny?.setInputValue) {
        resolve(served.current);
        return;
      }
      // getRows() runs on the grid's current state, so MUI's own selector
      // gives the filter items it applies: inactive ones (an operator picked
      // but no value yet, the row the filter panel adds on open) are left out.
      const filterModel = {
        ...params.filterModel,
        items: gridFilterActiveItemsSelector(apiRef),
      };
      const fields = quickFilterFields(filterModel, apiRef);
      const sig = JSON.stringify([
        params.paginationModel, params.sortModel, filterModel, fields,
      ]);
      // Nothing R's answer depends on changed: reuse it.
      if (sig === servedSignature.current && pending.current.size === 0) {
        resolve(served.current);
        return;
      }
      lastId.current += 1;
      pending.current.set(lastId.current, { resolve, sig });
      const payload = {
        pagination_model: params.paginationModel,
        sort_model: params.sortModel,
        filter_model: filterModel,
        request_id: lastId.current,
      };
      if (fields) payload.quick_filter_fields = fields;
      // A sort or filter change away from page 0 makes MUI request twice: once
      // for the old page, then (a few ms later, after it resets to page 0) for
      // the new one. Holding each request briefly lets the second replace the
      // first, so R renders once. A replaced request stays pending and is
      // resolved, as stale, by the answer to the newer one.
      clearTimeout(sendTimer.current);
      sendTimer.current = setTimeout(() => {
        window.Shiny.setInputValue(inputId, payload, { priority: 'event' });
      }, COALESCE_MS);
    }),
  }), [apiRef, inputId]);

  // Each R render answers the request it echoes (and any older one Shiny
  // coalesced into it; MUI ignores those as stale). A render that answers no
  // pending request (the data changed, or another reactive re-ran) updates the
  // current page in place, unless a newer request is still on its way.
  useEffect(() => {
    served.current = { rows, rowCount };
    const answered = pending.current.get(requestId);
    if (answered) {
      servedSignature.current = answered.sig;
      pending.current.forEach(({ resolve }, id) => {
        if (id <= requestId) {
          resolve(served.current);
          pending.current.delete(id);
        }
      });
    } else if (pending.current.size === 0 && servedSignature.current !== null) {
      apiRef.current?.setRows(rows ?? []);
      if (rowCount != null) apiRef.current?.setRowCount(rowCount);
    }
  }, [rows, rowCount, requestId, apiRef]);

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
