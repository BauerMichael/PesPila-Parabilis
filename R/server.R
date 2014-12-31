library(shiny)

shinyServer(
    function(input, output) {
        
        output$stats <- renderTable({
            if (input$var == "Goals") {
                overallData()[[1]]
            } else if (input$var == "Results") {
                overallData()[[2]]
            } else {
                loadLeagueContent()
            }
        })
        
    }
)