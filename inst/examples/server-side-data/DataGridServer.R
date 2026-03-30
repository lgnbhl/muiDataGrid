library(shiny)
library(muiDataGrid)
library(muiMaterial)
library(dplyr)

all_data <- dplyr::starwars |>
  select(name, height, mass, birth_year, gender, homeworld)

ui <- muiMaterialPage(
  Typography("Server-Side DataGrid Example", variant = "h5"),
  reactOutput("grid")
)

server <- function(input, output, session) {
  output$grid <- renderReact({
    result <- processGridParams(all_data, input$grid_params)

    DataGridServer(
      inputId = "grid_params",
      rows = result$rows,
      rowCount = result$rowCount,
      initialPageSize = 10L,
      pageSizeOptions = c(5L, 10L, 25L),
      sx = list(height = 500)
    )
  })
}

shinyApp(ui, server)
