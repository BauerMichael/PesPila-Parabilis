library(shiny)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
#     y <- 1:100
#     z <- y*y
    myCO2(input$bins + 1)
#     bins <- seq(min(y), max(y), length.out = input$bins + 1)
#     plot(y, z, xlim = c(1, input$bins))
    
  })
})