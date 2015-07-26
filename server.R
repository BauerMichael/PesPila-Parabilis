library(shiny)

shinyServer(
    function(input, output, session) {
        
        output$stats <- renderTable({
            if (input$var == "Goals") {
                overallData()[[1]]
            } else if (input$var == "Results") {
                overallData()[[2]]
            } else if (input$var == "Difference") {
                overallData()[[3]]
            } else if (input$var == "GSPM") {
                overallData()[[4]]
            } else {
                loadLeagueContent()
            }
        })
        
        output$compare <- renderTable({
            teamComparison(input$home, input$away)[,1:10]
        })
        
        output$atall <- renderTable({
            sumUpComparison(input$home, input$away)
        })
        
    }
)