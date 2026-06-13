# Minimum muiMaterial version whose JavaScript bundle ships the MUI Material +
# emotion runtime that this package's bundled @mui/x-data-grid expects. The grid
# is NOT bundled with its own @mui/material: it relies on the shared instance
# provided by muiMaterial's bundle (see muiDataGridDependency() and js/src). If
# muiMaterial ships an @mui/material major that x-data-grid is not compatible
# with, the grid fails at runtime in the browser (blank output / console errors)
# with nothing on the R side to explain it. Bump this whenever the bundled
# @mui/x-data-grid major changes to require a newer muiMaterial.
.MUIMATERIAL_MIN_VERSION <- "0.2.0"

# Warn (once, at attach) if the installed muiMaterial predates the version known
# to ship a compatible MUI runtime. Kept out of muiDataGridDependency() so it
# fires once per session rather than on every grid render.
.check_muimaterial_compat <- function() {
  if (!requireNamespace("muiMaterial", quietly = TRUE)) {
    return(invisible())
  }
  installed <- utils::packageVersion("muiMaterial")
  if (installed < .MUIMATERIAL_MIN_VERSION) {
    packageStartupMessage(
      "muiDataGrid: installed 'muiMaterial' (", installed, ") is older than the ",
      "minimum compatible version (", .MUIMATERIAL_MIN_VERSION, "). The grid ",
      "shares muiMaterial's bundled MUI Material/emotion runtime, so an older ",
      "muiMaterial can break rendering in the browser. Please update muiMaterial."
    )
  }
  invisible()
}

.onAttach <- function(libname, pkgname) {
  .check_muimaterial_compat()
}
