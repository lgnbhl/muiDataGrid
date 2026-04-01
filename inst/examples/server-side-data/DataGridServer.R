library(shiny)
library(muiDataGrid)
library(muiMaterial)
library(dplyr)

# 100000 rows with sequential IDs for easy verification
all_data <- data.frame(
  id = 1:100000,
  label = paste("Row", 1:100000),
  value = 1:100000,
  group = rep(c("A", "B", "C", "D"), each = 25000)
)

# Very slow with big data
# DataGrid(
#   rows = all_data,
#   initialState = list(
#     pagination = list(
#       paginationModel = list(pageSize = 5)
#     )
#   )
# )

ui <- muiMaterialPage(
  Typography("Server-Side DataGrid Example", variant = "h5"),
  reactOutput("grid")
)

server <- function(input, output, session) {
  output$grid <- renderReact({
    result <- processGridParams(all_data, input$grid_params, pageSize = 10L)

    DataGridServer(
      inputId = "grid_params",
      rows = result$rows,
      rowCount = result$rowCount,
      initialPageSize = 10L,
      pageSizeOptions = c(5L, 10L, 25L)
    )
  })
}

shinyApp(ui, server)
