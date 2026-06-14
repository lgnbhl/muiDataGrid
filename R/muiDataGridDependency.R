#' Mui X Data Grid UI JS dependency
#'
#' @return A list of HTML dependency objects (\code{htmltools::htmlDependency})
#'   that load the bundled MUI X Data Grid JavaScript and its shared MUI
#'   Material runtime. Attach it to custom HTML when building a grid by hand.
#'
#' @examples
#' # Inspect the dependencies attached to every DataGrid() element.
#' muiDataGridDependency()
#'
#' @export
muiDataGridDependency <- function() {
  list(
    muiMaterial::muiMaterialDependency(),
    htmltools::htmlDependency(
      name = "muiDataGrid",
      version = as.character(utils::packageVersion("muiDataGrid")),
      package = "muiDataGrid",
      src = "www/muiDataGrid",
      script = "x-data-grid.js"
    )
  )
}
