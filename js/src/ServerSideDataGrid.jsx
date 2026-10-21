import React, { useEffect, useRef, useMemo } from 'react';
import {
  DataGrid, useGridApiRef, gridColumnVisibilityModelSelector, gridFilterModelSelector,
} from '@mui/x-data-grid';

// Server-side grid backed by MUI's own data source: MUI owns the pagination,
// sort, and filter state, resets to the first page, coalesces changes into one
// request, shows the loading overlay, and drops stale responses. This
// component only bridges MUI's getRows() promise to Shiny: each request is
// posted as input$<inputId> with a request_id, R re-renders with that page,
// and DataGridServer() echoes the id back as `requestId` to resolve it.

// How long a request waits for a replacement before it is posted to R (see
// getRows). Small next to a Shiny round-trip.
const COALESCE_MS = 50;

// The filter items MUI actually applies: a known operator on a known column,
// with a value unless the operator takes none (isEmpty). Mirrors MUI's
// gridFilterActiveItemsSelector.
function activeFilterItems(filterModel, api) {
  return (filterModel.items ?? []).filter((item) => {
    const column = item.field ? api.getColumn(item.field) : undefined;
    const operator = column?.filterOperators?.find((op) => op.value === item.operator);
    if (!operator) return false;
    const hasValue = Array.isArray(item.value)
      ? item.value.length > 0
      : item.value != null && item.value.toString() !== '';
    return !operator.InputComponent || hasValue;
  });
}

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

// What R's result depends on. Inactive filter items (an operator picked but no
// value yet, the row the filter panel adds on open) filter nothing, so a
// request that differs only in those is answered without asking R.
function signature({ paginationModel, sortModel, filterModel }, apiRef) {
  return JSON.stringify([
    paginationModel,
    sortModel,
    activeFilterItems(filterModel, apiRef.current),
    filterModel.logicOperator ?? 'and',
    quickFilterFields(filterModel, apiRef) ?? [],
    filterModel.quickFilterValues?.filter(Boolean) ?? [],
    filterModel.quickFilterLogicOperator ?? 'and',
  ]);
}

function ServerSideDataGrid({
  inputId, rows, rowCount, requestId, loading, filterDebounce, ...otherProps
}) {
  const apiRef = useGridApiRef();
  // Latest page R rendered, read when a request can be answered locally.
  const served = useRef({ rows, rowCount });
  // Signature of the params `served` answers; null until the first request.
  const servedSignature = useRef(null);
  const firstRequest = useRef(true);
  const pending = useRef(new Map()); // request id -> { resolve, sig }
  const lastId = useRef(0);
  const sendTimer = useRef(null);

  useEffect(() => () => clearTimeout(sendTimer.current), []);

  const dataSource = useMemo(() => ({
    getRows: (params) => new Promise((resolve) => {
      const sig = signature(params, apiRef);
      const answerLocally = () => {
        servedSignature.current = sig;
        resolve(served.current);
      };
      // Static documents have no Shiny: only the first page R rendered exists.
      if (!window.Shiny?.setInputValue) {
        answerLocally();
        return;
      }
      // R's first render served the initial page, unsorted and unfiltered (see
      // DataGridServer); only an initialState sort or filter needs a request.
      if (firstRequest.current) {
        firstRequest.current = false;
        const { sortModel, filterModel } = params;
        if (sortModel.length === 0 &&
          activeFilterItems(filterModel, apiRef.current).length === 0 &&
          !filterModel.quickFilterValues?.some(Boolean)) {
          answerLocally();
          return;
        }
      } else if (sig === servedSignature.current && pending.current.size === 0) {
        answerLocally();
        return;
      }
      lastId.current += 1;
      pending.current.set(lastId.current, { resolve, sig });
      // A sort or filter change away from page 0 makes MUI request twice: once
      // for the old page, then (a few ms later, after it resets to page 0) for
      // the new one. Holding each request briefly lets the second replace the
      // first, so R renders once. A replaced request stays pending and is
      // resolved, as stale, by the answer to the newer one.
      clearTimeout(sendTimer.current);
      const payload = {
        pagination_model: params.paginationModel,
        sort_model: params.sortModel,
        filter_model: params.filterModel,
        request_id: lastId.current,
      };
      const fields = quickFilterFields(params.filterModel, apiRef);
      if (fields) payload.quick_filter_fields = fields;
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
    const filterModel = gridFilterModelSelector(apiRef);
    if (filterModel.quickFilterValues?.some(Boolean)) {
      apiRef.current.dataSource.fetchRows();
    }
  }), [apiRef]);

  return (
    <DataGrid
      {...otherProps}
      {...(loading != null && { loading })}
      apiRef={apiRef}
      dataSource={dataSource}
      dataSourceCache={null}
      filterDebounceMs={filterDebounce ?? 300}
    />
  );
}

export { ServerSideDataGrid };
