import React, { useState, useEffect, useRef, useCallback, useMemo } from 'react';
import { DataGrid } from '@mui/x-data-grid';

function ServerSideDataGrid({ inputId, initialState, filterDebounce, ...otherProps }) {
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
  const isFirstRender = useRef(true);
  // Tracks the last filterModel we saw (serialized) so the send effect can tell
  // a filter change (needs debouncing) apart from a pagination/sort change (sent
  // now). Compared by content rather than object identity so we don't depend on
  // MUI handing back a fresh filterModel reference on every change.
  const prevFilterRef = useRef(null);

  // Keep debounce delay in sync if the prop ever changes
  useEffect(() => { filterDebounceRef.current = filterDebounce ?? 300; }, [filterDebounce]);

  // Cancel any pending debounce on unmount
  useEffect(() => () => clearTimeout(debounceTimer.current), []);

  // Send state to R whenever pagination, sort, or filter changes (skip initial
  // mount). Filter changes are debounced to avoid a round-trip per keystroke;
  // pagination and sort changes are sent immediately. The filterModel itself is
  // updated synchronously in its handler so the controlled grid stays in sync.
  useEffect(() => {
    const send = () => {
      if (window.Shiny?.setInputValue) {
        window.Shiny.setInputValue(inputId, {
          pagination_model: gridState.paginationModel,
          sort_model: gridState.sortModel,
          filter_model: gridState.filterModel,
        }, { priority: 'event' });
      }
    };
    if (isFirstRender.current) {
      isFirstRender.current = false;
      prevFilterRef.current = JSON.stringify(gridState.filterModel);
      // If an initial sort or filter was supplied (via initialState), R must
      // learn about it now. Otherwise the grid header shows the sort arrow /
      // filter chip but processGridParams sees params = NULL and returns
      // unsorted, unfiltered data — a visible mismatch on first paint.
      const hasInitialSort = gridState.sortModel.length > 0;
      const hasInitialFilter = (gridState.filterModel.items?.length ?? 0) > 0;
      if (hasInitialSort || hasInitialFilter) {
        send();
      }
      return;
    }
    const serializedFilter = JSON.stringify(gridState.filterModel);
    const filterChanged = serializedFilter !== prevFilterRef.current;
    prevFilterRef.current = serializedFilter;
    if (filterChanged) {
      clearTimeout(debounceTimer.current);
      debounceTimer.current = setTimeout(send, filterDebounceRef.current);
    } else {
      clearTimeout(debounceTimer.current);
      send();
    }
  }, [gridState, inputId]);

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
  // panel is never dropped); the send effect debounces the R round-trip. MUI
  // fires this for no-op changes too (e.g. opening the filter panel), so bail
  // out when nothing actually changed — otherwise paging users get yanked to
  // page 0 and R gets a redundant round-trip.
  const handleFilterModelChange = useCallback((newFilterModel) => {
    setGridState((prev) => {
      if (JSON.stringify(prev.filterModel) === JSON.stringify(newFilterModel)) {
        return prev;
      }
      return {
        ...prev,
        filterModel: newFilterModel,
        paginationModel: { ...prev.paginationModel, page: 0 },
      };
    });
  }, []);

  return (
    <DataGrid
      {...otherProps}
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
