library(shiny)
library(muiDataGrid)
library(muiMaterial)

# 100 rows with sequential IDs for easy verification
all_data <- data.frame(
  id = 1:100,
  label = paste("Row", 1:100),
  value = 1:100,
  group = rep(c("A", "B", "C", "D"), each = 25)
)

columns <- list(
  list(field = "id", headerName = "ID", width = 80),
  list(field = "label", headerName = "Label", flex = 1),
  list(field = "value", headerName = "Value", type = "number", width = 100),
  list(field = "group", headerName = "Group", width = 100)
)

ui <- muiMaterialPage(
  Typography("Server-Side vs Client-Side DataGrid", variant = "h5"),
  Grid(
    container = TRUE,
    spacing = 2,
    # LEFT: Client-side (standard DataGrid)
    Grid(
      item = TRUE,
      xs = 6,
      Typography("Client-side: DataGrid()", variant = "h6"),
      Alert(
        severity = "info",
        sx = list(mb = 1),
        "All 100 rows are sent to the browser at once. Pagination and sorting are handled entirely in JavaScript."
      ),
      DataGrid(
        rows = all_data,
        columns = columns,
        initialState = list(
          pagination = list(paginationModel = list(pageSize = 5L))
        ),
        pageSizeOptions = list(5L, 10L, 25L),
        sx = list(height = 400)
      )
    ),
    # RIGHT: Server-side (DataGridServer)
    Grid(
      item = TRUE,
      xs = 6,
      Typography("Server-side: DataGridServer()", variant = "h6"),
      reactOutput("server_info"),
      reactOutput("grid")
    )
  ),
  # Show raw input$grid_params below
  Typography(
    "input$grid_params (received from React)",
    variant = "h6",
    sx = list(mt = 3)
  ),
  Alert(
    severity = "warning",
    sx = list(mb = 1),
    "This Shiny input is sent by the DataGridServer React component every time the user changes page, sort, or filter. The R server uses it to compute and return only the relevant rows."
  ),
  tags$pre(
    style = "background: #f5f5f5; padding: 12px; border-radius: 4px; overflow-x: auto;",
    textOutput("raw_params")
  ),
  Typography(
    "Rows sent to browser by R server",
    variant = "h6",
    sx = list(mt = 2)
  ),
  Alert(
    severity = "success",
    sx = list(mb = 1),
    "These are the only rows the browser receives. Compare with the client-side grid which has all 100 rows in the DOM."
  ),
  reactOutput("rows_table")
)

server <- function(input, output, session) {
  # Shared reactive for processed data
  grid_result <- reactive({
    processGridParams(all_data, input$grid_params, pageSize = 5L)
  })

  output$server_info <- renderReact({
    params <- input$grid_params
    result <- grid_result()
    Alert(
      severity = "success",
      sx = list(mb = 1),
      paste0(
        "R sent ",
        nrow(result$rows),
        " of ",
        result$rowCount,
        " rows ",
        "(IDs: ",
        paste(result$rows$id, collapse = ", "),
        ") | ",
        "Page ",
        if (!is.null(params)) params$pagination_model$page else 0,
        " | ",
        "Sort: ",
        if (!is.null(params) && length(params$sort_model) > 0) {
          paste(params$sort_model[[1]]$field, params$sort_model[[1]]$sort)
        } else {
          "(none)"
        },
        " | ",
        "Filters: ",
        if (!is.null(params)) length(params$filter_model$items) else 0
      )
    )
  })

  output$grid <- renderReact({
    result <- grid_result()
    DataGridServer(
      inputId = "grid_params",
      rows = result$rows,
      columns = columns,
      rowCount = result$rowCount,
      initialPageSize = 5L,
      pageSizeOptions = c(5L, 10L, 25L),
      sx = list(height = 400)
    )
  })

  output$raw_params <- renderText({
    params <- input$grid_params
    if (is.null(params)) {
      "NULL (waiting for first render...)"
    } else {
      paste(
        paste0("pagination_model$page:     ", params$pagination_model$page),
        paste0("pagination_model$pageSize: ", params$pagination_model$pageSize),
        paste0(
          "sort_model:                ",
          if (length(params$sort_model) > 0) {
            paste0(
              "[ { field: \"",
              params$sort_model[[1]]$field,
              "\", sort: \"",
              params$sort_model[[1]]$sort,
              "\" } ]"
            )
          } else {
            "[]"
          }
        ),
        paste0(
          "filter_model$items:        ",
          if (length(params$filter_model$items) > 0) {
            paste0(
              "[ ",
              paste(
                vapply(
                  params$filter_model$items,
                  function(item) {
                    paste0(
                      "{ field: \"",
                      item$field,
                      "\", operator: \"",
                      item$operator,
                      "\", value: \"",
                      item$value,
                      "\" }"
                    )
                  },
                  character(1)
                ),
                collapse = ", "
              ),
              " ]"
            )
          } else {
            "[]"
          }
        ),
        sep = "\n"
      )
    }
  })

  output$rows_table <- renderReact({
    result <- grid_result()
    DataGrid(
      rows = result$rows,
      columns = columns,
      hideFooter = TRUE,
      sx = list(height = 250)
    )
  })
}

shinyApp(ui, server)
