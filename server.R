library(shiny)

shinyServer(
    function(input, output, session) {

        output$teamA <- renderText({input$home})
        output$teamB <- renderText({input$away})
        
        output$stats <- renderDataTable(
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
        )
        
        output$compare <- renderDataTable(teamComparison(input$home, input$away)[,2:10],
            options = list(pageLength = 100)
        )

        output$compareAll <- renderDataTable(allGamesComparison(input$home, input$away)[,2:10],
            options = list(pageLength = 100)
        )
        
        output$overview <- renderDataTable(sumUpComparison(input$home, input$away))

        observeEvent(input$reload,

            return({
              withProgress(session, min=1, max=2, {
                setProgress(message = 'Calculation in progress',
                            detail = 'This may take a while...')
                setProgress(value = 1)
                init()
                setProgress(value = 2)
              })
            })

        )
        
    }
)