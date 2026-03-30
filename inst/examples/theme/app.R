library(muiDataGrid)
library(muiMaterial)
library(htmltools)

dark_theme <- list(
  palette = list(
    mode = "dark",
    primary = list(main = "#90caf9"),
    background = list(default = "#121212", paper = "#1e1e1e")
  )
)

rows <- data.frame(
  id = 1:10,
  name = c(
    "Alice",
    "Bob",
    "Carol",
    "Dave",
    "Eve",
    "Frank",
    "Grace",
    "Heidi",
    "Ivan",
    "Judy"
  ),
  age = c(28, 34, 45, 23, 31, 52, 27, 39, 41, 36),
  score = c(92, 85, 78, 95, 88, 71, 96, 83, 77, 90)
)

columns <- data.frame(
  field = c("id", "name", "age", "score"),
  headerName = c("ID", "Name", "Age", "Score"),
  width = c(80, 150, 100, 100)
)

# The DataGrid uses its own bundled Emotion instance, separate from muiMaterial's
# ThemeProvider, so it does not automatically pick up the dark theme.
# Two workarounds are needed:
#
# 1. Use `style` (not `sx`) to set DataGrid CSS custom variables
#    (--DataGrid-t-*) for backgrounds, text, and borders.
#
# 2. Inject a <style> tag for the pagination, which is rendered by MUI's
#    TablePagination with Emotion-scoped styles that override CSS inheritance.
#    We use !important to force the text color.
datagrid_dark_css <- htmltools::tags$style(htmltools::HTML(
  "
  .MuiDataGrid-root .MuiTablePagination-root,
  .MuiDataGrid-root .MuiTablePagination-selectLabel,
  .MuiDataGrid-root .MuiTablePagination-displayedRows,
  .MuiDataGrid-root .MuiTablePagination-select,
  .MuiDataGrid-root .MuiTablePagination-actions button,
  .MuiDataGrid-root .MuiSelect-icon {
    color: #fff !important;
  }
"
))

ui <- muiMaterialPage(
  ThemeProvider(
    theme = dark_theme,
    CssBaseline(),
    datagrid_dark_css,
    Box(
      sx = list(p = 4, minHeight = "100vh"),
      Typography(
        "DataGrid + ThemeProvider",
        variant = "h5",
        gutterBottom = TRUE
      ),
      Typography(
        "Themed DataGrid with muiMaterial dark theme.",
        variant = "body1",
        sx = list(mb = 2)
      ),
      Box(
        sx = list(height = 450, width = "100%"),
        DataGrid(
          rows = rows,
          columns = columns,
          style = list(
            "--DataGrid-t-color-background-base" = dark_theme$palette$background$paper,
            "--DataGrid-t-header-background-base" = dark_theme$palette$background$paper,
            "--DataGrid-t-color-foreground-base" = "#fff",
            "--DataGrid-t-color-border-base" = "#333",
            "--DataGrid-t-color-foreground-muted" = "#aaa",
            color = "#fff"
          )
        )
      )
    )
  )
)

htmltools::browsable(ui)
