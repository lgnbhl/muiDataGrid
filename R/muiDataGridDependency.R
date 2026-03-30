#' Mui X Data Grid UI JS dependency
#'
#' @return HTML dependency object.
#'
#' @export
muiDataGridDependency <- function() {
  list(
    muiMaterial::muiMaterialDependency(),
    htmltools::htmlDependency(
      name = "muiDataGrid",
      version = "0.1.0",
      package = "muiDataGrid",
      src = "www/muiDataGrid",
      script = "x-data-grid.js"
    )
  )
}
