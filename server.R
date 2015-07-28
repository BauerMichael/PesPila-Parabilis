library(shiny)

shinyServer(
    function(input, output, session) {

        country <- GetCountries()
        division <- list.files(path = paste("Data", country[1], sep = "/"), pattern = ".csv")
        for (i in 1:length(division)) {
            division[i] <- strsplit(x = division[i], split = ".csv")[[1]]
        }
        team <- FolderStructure(path = paste("Data", country[1], sep = "/"))
        
        preferences <- ReadPreferences()
        output$myCountry <- renderText({preferences[1]})
        output$myDivision <- renderText({preferences[2]})
        output$myTeam <- renderText({preferences[3]})

        updateSelectInput(session, "country", choices = country, selected = preferences[1])
        updateSelectInput(session, "division", choices = division, selected = preferences[2])
        updateSelectInput(session, "theTeam", choices = team, selected = preferences[3])

        observeEvent(input$country, {

            division <- list.files(path = paste("Data", input$country, sep = "/"), pattern = ".csv")
            for (i in 1:length(division)) {
                division[i] <- strsplit(x = division[i], split = ".csv")[[1]]
            }
            team <- FolderStructure(path = paste("Data", input$country, sep = "/"))

            updateSelectInput(session, "division", choices = division, selected = division[1])
            updateSelectInput(session, "theTeam", choices = team, selected = team[1])

        })

        observeEvent(input$setPreferences, {

            SetPreferences(input$country, input$division, input$theTeam)
            preferences <- ReadPreferences()
            output$myCountry <- renderText({preferences[1]})
            output$myDivision <- renderText({preferences[2]})
            output$myTeam <- renderText({preferences[3]})

        })

        # output$wins <- renderDataTable(Wins(path = "Data", country = "Germany"))
        # output$results <- renderDataTable(overallData()[[2]])
        # output$goalDifferences <- renderDataTable(GoalDifferences(path = "Data", country = "Germany"))
        # output$gspm <- renderDataTable(overallData()[[4]])
        # output$all <- renderDataTable(loadLeagueContent()[, 1:10])

        observeEvent(input$countryOverview, {

            updateSelectInput(session, "leagueOverview",
                            choices = strsplit(x = list.files(path = paste("Data",
                                                    country = input$countryOverview,
                                                    sep = "/"),
                                                    pattern = ".csv"),
                                                split = ".csv"),
                            selected = "Synopsis")
            output$wins <- renderDataTable(Wins(path = "Data", country = input$countryOverview))
            output$results <- renderDataTable(ResultsComparison(path = "Data", country = input$countryOverview, league = input$leagueOverview))
            output$goalDifferences <- renderDataTable(GoalDifferences(path = "Data", country = input$countryOverview))
            output$gspm <- renderDataTable(GSPM(path = "Data", country = input$countryOverview))
            output$all <- renderDataTable(LoadCountryData(path = "Data", country = input$countryOverview, filename = "Synopsis.csv")[, 1:10],
                                            options = list(pageLength = 100))

        })
        
        # output$stats <- renderDataTable(
        #     if (input$var == "Wins") {
        #         return (Wins(path = "Data", country = "Germany"))
        #         # overallData()[[1]]
        #     } else if (input$var == "Results") {
        #         overallData()[[2]]
        #     } else if (input$var == "Goal Differences") {
        #         return (GoalDifferences(path = "Data", country = "Germany"))
        #         # overallData()[[3]]
        #     } else if (input$var == "GSPM") {
        #         overallData()[[4]]
        #     } else {
        #         loadLeagueContent()[, 1:10]
        #     }
        # )
        
        output$compare <- renderDataTable(teamComparison(input$home, input$away)[, 2:10],
                                          options = list(pageLength = 100)
        )

        output$compareAll <- renderDataTable(allGamesComparison(input$home, input$away)[, 2:10],
                                             options = list(pageLength = 100)
        )
        
        output$overview <- renderDataTable(sumUpComparison(input$home, input$away))

        output$overviewAll <- renderDataTable(overviewAll(input$home, input$away))

        output$teamData <- renderDataTable(extractOneTeam(input$team)[, 2:14],
                                           options = list(pageLength = 100))

        output$sumTeamData <- renderDataTable(sumUpOneTeam(input$team)[, 2:14],
                                           options = list(pageLength = 100))

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