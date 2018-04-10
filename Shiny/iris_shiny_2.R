#iris_shiny_2.r

server = function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  linear_model <- reactive({
    lm(input$ycol ~ input$xcol)
  })
  
  output$text <- renderText(
    summary(linear_model())
  )
}

ui = pageWithSidebar(
  headerPanel('Iris LM summary'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(iris)),
    selectInput('ycol', 'Y Variable', names(iris),
                selected=names(iris)[[2]])
  ),
  mainPanel(
    textOutput('text')
  )
)

shinyApp(ui, server)