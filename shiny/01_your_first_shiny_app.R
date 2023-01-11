library(shiny)

ui <- fluidPage(
  # "Hello, world!"
  selectInput(
    "dataset",
    label = "Dataset",
    choices = ls("package:datasets")
  ),
  verbatimTextOutput("summary"),
  tableOutput("table"),
)
server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })

  output$summary <- renderPrint({
    summary(dataset())
  })

  output$table <- renderTable({
    dataset()
  })
}
shinyApp(ui, server)
