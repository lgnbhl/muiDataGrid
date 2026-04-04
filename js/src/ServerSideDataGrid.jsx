import React, { useState, useEffect, useRef, useCallback, memo } from 'react';
import { DataGrid } from '@mui/x-data-grid';

function ServerSideDataGrid({ inputId, initialState, filterDebounce, ...otherProps }) {
  const [gridState, setGridState] = useState({
    paginationModel: initialState?.pagination?.paginationModel ?? { page: 0, pageSize: 100 },
    sortModel: initialState?.sorting?.sortModel ?? [],
    filterModel: initialState?.filter?.filterModel ?? { items: [] },
  });

  const debounceTimer = useRef(null);
  const filterDebounceRef = useRef(filterDebounce ?? 300);
  const isFirstRender = useRef(true);

  // Keep debounce delay in sync if the prop ever changes
  useEffect(() => { filterDebounceRef.current = filterDebounce ?? 300; }, [filterDebounce]);

  // Cancel any pending debounce on unmount
  useEffect(() => () => clearTimeout(debounceTimer.current), []);

  // Send state to R whenever pagination, sort, or filter changes (skip initial mount)
  useEffect(() => {
    if (isFirstRender.current) {
      isFirstRender.current = false;
      return;
    }
    if (window.Shiny?.setInputValue) {
      window.Shiny.setInputValue(inputId, {
        pagination_model: gridState.paginationModel,
        sort_model: gridState.sortModel,
        filter_model: gridState.filterModel,
      }, { priority: 'event' });
    }
  }, [gridState, inputId]);

  const handlePaginationModelChange = useCallback((newPaginationModel) => {
    setGridState((prev) => ({ ...prev, paginationModel: newPaginationModel }));
  }, []);

  // Reset to page 0 when sort changes
  const handleSortModelChange = useCallback((newSortModel) => {
    setGridState((prev) => ({
      ...prev,
      sortModel: newSortModel,
      paginationModel: { ...prev.paginationModel, page: 0 },
    }));
  }, []);

  // Debounce filter changes to avoid excessive R round-trips per keystroke
  const handleFilterModelChange = useCallback((newFilterModel) => {
    clearTimeout(debounceTimer.current);
    debounceTimer.current = setTimeout(() => {
      setGridState((prev) => ({
        ...prev,
        filterModel: newFilterModel,
        paginationModel: { ...prev.paginationModel, page: 0 },
      }));
    }, filterDebounceRef.current);
  }, []);

  return (
    <DataGrid
      {...otherProps}
      paginationModel={gridState.paginationModel}
      onPaginationModelChange={handlePaginationModelChange}
      sortModel={gridState.sortModel}
      onSortModelChange={handleSortModelChange}
      filterModel={gridState.filterModel}
      onFilterModelChange={handleFilterModelChange}
    />
  );
}

const MemoizedServerSideDataGrid = memo(ServerSideDataGrid);
export { MemoizedServerSideDataGrid as ServerSideDataGrid };
