import React, { useState, useEffect, useRef, useCallback, useMemo } from 'react';
import { DataGrid, useGridApiRef, gridColumnVisibilityModelSelector } from '@mui/x-data-grid';

// The filter items MUI actually applies: a known operator on a known column,
// with a value unless the operator takes none (isEmpty). Mirrors MUI's
// gridFilterActiveItemsSelector, but runs on our controlled model so it never
// lags a render behind the grid's internal state. Without the grid API every
// item counts as active, so nothing is ever dropped.
function activeFilterItems(filterModel, api) {
  const items = filterModel.items ?? [];
  if (!api) return items;
  return items.filter((item) => {
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
  const api = apiRef.current;
  if (!api || !filterModel.quickFilterValues?.some(Boolean)) return undefined;
  const excludeHidden = filterModel.quickFilterExcludeHiddenColumns ?? true;
  const visibility = gridColumnVisibilityModelSelector(apiRef);
  return api.getAllColumns()
    .filter((col) => !excludeHidden || visibility[col.field] !== false)
    .filter((col) => col.getApplyQuickFilterFn?.('1', col, apiRef) != null)
    .map((col) => ({ field: col.field, type: col.type ?? 'string' }));
}

function ServerSideDataGrid({ inputId, initialState, filterDebounce, ...otherProps }) {
  const apiRef = useGridApiRef();
  const [gridState, setGridState] = useState({
    paginationModel: initialState?.pagination?.paginationModel ?? { page: 0, pageSize: 100 },
    sortModel: initialState?.sorting?.sortModel ?? [],
    filterModel: initialState?.filter?.filterModel ?? { items: [] },
  });

  // pagination/sorting/filter are controlled here (seeded into gridState above),
  // so strip them and forward only the rest of initialState — column visibility,
  // density, pinned columns, etc. Otherwise those settings would be silently
  // dropped in server mode, and re-passing the controlled keys would trigger
  // MUI's controlled-vs-initialState warning.
  const forwardedInitialState = useMemo(() => {
    if (!initialState) return undefined;
    const { pagination, sorting, filter, ...rest } = initialState;
    return Object.keys(rest).length > 0 ? rest : undefined;
  }, [initialState]);

  const debounceTimer = useRef(null);
  const filterDebounceRef = useRef(filterDebounce ?? 300);
  // Latest state, read by the debounced send so it never posts a stale closure.
  const stateRef = useRef(gridState);
  stateRef.current = gridState;
  // Signatures of the state R last saw (null before mount), so the send effect
  // can tell a filter change (debounced) from a pagination/sort change (sent
  // now) and ignore changes that alter neither.
  const lastSeen = useRef(null);

  // Keep debounce delay in sync if the prop ever changes
  useEffect(() => { filterDebounceRef.current = filterDebounce ?? 300; }, [filterDebounce]);

  // Cancel any pending debounce on unmount
  useEffect(() => () => clearTimeout(debounceTimer.current), []);

  // Hiding or showing a column changes which columns the quick filter searches;
  // a fresh state object makes the send effect re-evaluate.
  useEffect(() => apiRef.current?.subscribeEvent('columnVisibilityModelChange', () => {
    setGridState((prev) => ({ ...prev }));
  }), [apiRef]);

  // The part of the filter model R's result depends on. Inactive items (an
  // operator picked but no value yet, the row the filter panel adds on open)
  // filter nothing, so they neither reset the page nor trigger a round-trip.
  const effectiveFilter = useCallback((filterModel) => JSON.stringify({
    ...filterModel,
    items: activeFilterItems(filterModel, apiRef.current),
    quick_filter_fields: quickFilterFields(filterModel, apiRef),
  }), [apiRef]);

  const send = useCallback(() => {
    const { paginationModel, sortModel, filterModel } = stateRef.current;
    const payload = {
      pagination_model: paginationModel,
      sort_model: sortModel,
      filter_model: filterModel,
    };
    const fields = quickFilterFields(filterModel, apiRef);
    if (fields) payload.quick_filter_fields = fields;
    window.Shiny?.setInputValue?.(inputId, payload, { priority: 'event' });
  }, [apiRef, inputId]);

  // Send state to R whenever pagination, sort, or the effective filter changes
  // (skip initial mount). Filter changes are debounced to avoid a round-trip per
  // keystroke; pagination and sort changes are sent immediately. The filterModel
  // itself is updated synchronously in its handler so the controlled grid stays
  // in sync.
  useEffect(() => {
    const seen = {
      filter: effectiveFilter(gridState.filterModel),
      other: JSON.stringify([gridState.paginationModel, gridState.sortModel]),
    };
    const prev = lastSeen.current;
    lastSeen.current = seen;
    if (prev === null) {
      // If an initial sort or filter was supplied (via initialState), R must
      // learn about it now. Otherwise the grid header shows the sort arrow /
      // filter chip but processGridParams sees params = NULL and returns
      // unsorted, unfiltered data — a visible mismatch on first paint.
      const { sortModel, filterModel } = gridState;
      if (sortModel.length > 0 ||
        activeFilterItems(filterModel, apiRef.current).length > 0 ||
        filterModel.quickFilterValues?.some(Boolean)) {
        send();
      }
      return;
    }
    if (seen.filter !== prev.filter) {
      clearTimeout(debounceTimer.current);
      debounceTimer.current = setTimeout(send, filterDebounceRef.current);
    } else if (seen.other !== prev.other) {
      clearTimeout(debounceTimer.current);
      send();
    }
  }, [gridState, effectiveFilter, send, apiRef]);

  const handlePaginationModelChange = useCallback((newPaginationModel) => {
    setGridState((prev) => ({ ...prev, paginationModel: newPaginationModel }));
  }, []);

  // Reset to page 0 when sort changes. MUI can fire this with an unchanged
  // model (e.g. re-clicking the active column past its sort cycle); bail out
  // then so we neither yank the user back to page 0 nor send a redundant
  // round-trip to R.
  const handleSortModelChange = useCallback((newSortModel) => {
    setGridState((prev) => {
      if (JSON.stringify(prev.sortModel) === JSON.stringify(newSortModel)) {
        return prev;
      }
      return {
        ...prev,
        sortModel: newSortModel,
        paginationModel: { ...prev.paginationModel, page: 0 },
      };
    });
  }, []);

  // Update the controlled filterModel synchronously (so typing in the filter
  // panel is never dropped); the send effect debounces the R round-trip. Only a
  // change to the effective filter returns to page 0, so opening the filter
  // panel or picking an operator does not yank paging users back to the start.
  const handleFilterModelChange = useCallback((newFilterModel) => {
    setGridState((prev) => {
      if (JSON.stringify(prev.filterModel) === JSON.stringify(newFilterModel)) {
        return prev;
      }
      const reset = effectiveFilter(prev.filterModel) !== effectiveFilter(newFilterModel);
      return {
        ...prev,
        filterModel: newFilterModel,
        paginationModel: reset ? { ...prev.paginationModel, page: 0 } : prev.paginationModel,
      };
    });
  }, [effectiveFilter]);

  return (
    <DataGrid
      {...otherProps}
      apiRef={apiRef}
      initialState={forwardedInitialState}
      paginationModel={gridState.paginationModel}
      onPaginationModelChange={handlePaginationModelChange}
      sortModel={gridState.sortModel}
      onSortModelChange={handleSortModelChange}
      filterModel={gridState.filterModel}
      onFilterModelChange={handleFilterModelChange}
    />
  );
}

export { ServerSideDataGrid };
