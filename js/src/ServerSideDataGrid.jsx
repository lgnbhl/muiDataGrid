import React, { useState, useEffect, useRef } from 'react';
import { DataGrid } from '@mui/x-data-grid';

function ServerSideDataGrid({
  inputId,
  rows,
  columns,
  rowCount,
  loading,
  initialPageSize,
  pageSizeOptions,
  ...otherProps
}) {
  const [paginationModel, setPaginationModel] = useState({
    page: 0,
    pageSize: initialPageSize || 25,
  });
  const [sortModel, setSortModel] = useState([]);
  const [filterModel, setFilterModel] = useState({ items: [] });

  // Track whether this is the initial mount to avoid sending params before Shiny is ready
  const isInitialMount = useRef(true);

  // Send state to R whenever pagination, sort, or filter changes
  useEffect(() => {
    if (isInitialMount.current) {
      isInitialMount.current = false;
      // Send initial params so R can render the first page
      if (window.Shiny && window.Shiny.setInputValue) {
        Shiny.setInputValue(inputId, {
          pagination_model: paginationModel,
          sort_model: sortModel,
          filter_model: filterModel,
        }, { priority: 'event' });
      }
      return;
    }

    if (window.Shiny && window.Shiny.setInputValue) {
      Shiny.setInputValue(inputId, {
        pagination_model: paginationModel,
        sort_model: sortModel,
        filter_model: filterModel,
      }, { priority: 'event' });
    }
  }, [paginationModel, sortModel, filterModel, inputId]);

  // Reset to page 0 when sort or filter changes
  const handleSortModelChange = (newSortModel) => {
    setSortModel(newSortModel);
    setPaginationModel((prev) => ({ ...prev, page: 0 }));
  };

  const handleFilterModelChange = (newFilterModel) => {
    setFilterModel(newFilterModel);
    setPaginationModel((prev) => ({ ...prev, page: 0 }));
  };

  return (
    <DataGrid
      rows={rows || []}
      columns={columns || []}
      rowCount={rowCount || 0}
      loading={loading || false}
      paginationMode="server"
      sortingMode="server"
      filterMode="server"
      pagination
      paginationModel={paginationModel}
      onPaginationModelChange={setPaginationModel}
      sortModel={sortModel}
      onSortModelChange={handleSortModelChange}
      filterModel={filterModel}
      onFilterModelChange={handleFilterModelChange}
      pageSizeOptions={pageSizeOptions || [10, 25, 50, 100]}
      {...otherProps}
    />
  );
}

export { ServerSideDataGrid };
