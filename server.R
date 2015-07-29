library(shiny)

LoadPreferences <- function(output) {
    preferences <- ReadPreferences()
    output$myCountry <- renderText({preferences[1]})
    output$myDivision <- renderText({preferences[2]})
    output$myTeam <- renderText({preferences[3]})
    return (preferences)
}

LoadAndSetPreferenceChoice <- function(session, country = "Germany") {
    division <- strsplit(x = list.files(path = paste("Data", country, sep = "/"), pattern = ".csv"), split = ".csv")
    team <- FolderStructure(path = paste("Data", country, sep = "/"))

    updateSelectInput(session, "country", choices = GetCountries(), selected = country)
    updateSelectInput(session, "division", choices = division)
    updateSelectInput(session, "theTeam", choices = team)
}

LoadAndSetTeamDataChoice <- function(session, t = "Bayern Munich", div = "Bundesliga1", country = "Germany") {
    division <- strsplit(x = list.files(path = paste("Data", country, sep = "/"), pattern = ".csv"), split = ".csv")
    team <- FolderStructure(path = paste("Data", country, sep = "/"))

    updateSelectInput(session, "teamCountry", choices = GetCountries(), selected = country)
    updateSelectInput(session, "teamDivision", choices = division, selected = div)
    updateSelectInput(session, "teamChoice", choices = team, selected = t)
}

SetGeneralDataOverview <- function(session, preferences) {
    league <- strsplit(x = list.files(path = paste("Data", preferences[1], sep = "/"), pattern = ".csv"), split = ".csv")
    updateSelectInput(session, "countryOverview", choices = GetCountries(), selected = preferences[1])
    updateSelectInput(session, "leagueOverview", choices = league, selected = preferences[2])
}

shinyServer(
    function(input, output, session) {

        preferences <- LoadPreferences(output)

        observeEvent(input$country, {

            if (input$country == "") {
                LoadAndSetPreferenceChoice(session, preferences[1])
                LoadAndSetTeamDataChoice(session, preferences[3], preferences[2], preferences[1])
                SetGeneralDataOverview(session, preferences)
            } else {
                LoadAndSetPreferenceChoice(session, input$country)
                LoadAndSetTeamDataChoice(session, preferences[3], preferences[2], input$country)
                SetGeneralDataOverview(session, preferences)
            }

        })

        observeEvent(input$setPreferences, {

            SetPreferences(input$country, input$division, input$theTeam)
            preferences <- LoadPreferences(output)
            SetGeneralDataOverview(session, preferences)
            LoadAndSetTeamDataChoice(session, input$theTeam, input$division, input$country)
            SetGeneralDataOverview(session, preferences)

        })

        # output$wins <- renderDataTable(Wins(path = "Data", country = "Germany"))
        # output$results <- renderDataTable(overallData()[[2]])
        # output$goalDifferences <- renderDataTable(GoalDifferences(path = "Data", country = "Germany"))
        # output$gspm <- renderDataTable(overallData()[[4]])
        # output$all <- renderDataTable(loadLeagueContent()[, 1:10])

        observeEvent(input$countryOverview, {

            league <- strsplit(x = list.files(path = paste("Data", country = input$countryOverview,
                                                sep = "/"), pattern = ".csv"), split = ".csv")

            if (input$countryOverview == preferences[1]) {
                updateSelectInput(session, "leagueOverview",
                            choices = league,
                            selected = preferences[2])
            } else {
                updateSelectInput(session, "leagueOverview",
                            choices = league,
                            selected = league[1])
            }

            output$wins <- renderDataTable(Wins(path = "Data", country = input$countryOverview))
            output$results <- renderDataTable(ResultsComparison(path = "Data", country = input$countryOverview, league = input$leagueOverview))
            output$goalDifferences <- renderDataTable(GoalDifferences(path = "Data", country = input$countryOverview))
            output$gspm <- renderDataTable(GSPM(path = "Data", country = input$countryOverview))
            output$all <- renderDataTable(LoadCountryData(path = "Data", country = input$countryOverview, filename = "Synopsis.csv")[, 1:10],
                                            options = list(pageLength = 100))

            # withProgress(session, min = 1, max = 6, {
                # setProgress(message = "Calculation in progress",
                            # detail = "Just a few seconds please...")
                # setProgress(value = 1)
                # updateSelectInput(session, "leagueOverview",
                #             choices = strsplit(x = list.files(path = paste("Data",
                #                                     country = input$countryOverview,
                #                                     sep = "/"),
                #                                     pattern = ".csv"),
                #                                 split = ".csv"),
                #             selected = "Synopsis")
            #     results.table <- ResultsComparison(path = "Data", country = input$countryOverview, league = input$leagueOverview)
            #     # setProgress(value = 2)
            #     wins.table <- Wins(path = "Data", country = input$countryOverview)
            #     # setProgress(value = 3)
            #     differences.table <- GoalDifferences(path = "Data", country = input$countryOverview)
            #     # setProgress(value = 4)
            #     gspm.table <- GSPM(path = "Data", country = input$countryOverview)
            #     # setProgress(value = 5)
            #     all.games.table <- LoadCountryData(path = "Data", country = input$countryOverview, filename = "Synopsis.csv")
            #     # setProgress(value = 6)
            # # })
            # output$results <- renderDataTable(results.table)
            # output$wins <- renderDataTable(wins.table)
            # output$goalDifferences <- renderDataTable(differences.table)
            # output$gspm <- renderDataTable(gspm.table)
            # output$all <- renderDataTable(all.games.table[, 1:10], options = list(pageLength = 100))

        })

        # observeEvent(input$countryOverview, {

        #     updateSelectInput(session, "leagueOverview",
        #                     choices = strsplit(x = list.files(path = paste("Data",
        #                                             country = input$countryOverview,
        #                                             sep = "/"),
        #                                             pattern = ".csv"),
        #                                         split = ".csv"),
        #                     selected = "Synopsis")
        #     results.table <- ResultsComparison(path = "Data", country = input$countryOverview, league = input$leagueOverview)
        #     wins.table <- Wins(path = "Data", country = input$countryOverview)
        #     differences.table <- GoalDifferences(path = "Data", country = input$countryOverview)
        #     gspm.table <- GSPM(path = "Data", country = input$countryOverview)
        #     all.games.table <- LoadCountryData(path = "Data", country = input$countryOverview, filename = "Synopsis.csv")
        #     output$results <- renderDataTable(results.table)
        #     output$wins <- renderDataTable(wins.table)
        #     output$goalDifferences <- renderDataTable(differences.table)
        #     output$gspm <- renderDataTable(gspm.table)
        #     output$all <- renderDataTable(all.games.table[, 1:10], options = list(pageLength = 100))

        # #     updateSelectInput(session, "leagueOverview",
        # #                     choices = strsplit(x = list.files(path = paste("Data",
        # #                                             country = input$countryOverview,
        # #                                             sep = "/"),
        # #                                             pattern = ".csv"),
        # #                                         split = ".csv"),
        # #                     selected = "Synopsis")
        # #     output$wins <- renderDataTable(Wins(path = "Data", country = input$countryOverview))
        # #     output$results <- renderDataTable(ResultsComparison(path = "Data", country = input$countryOverview, league = input$leagueOverview))
        # #     output$goalDifferences <- renderDataTable(GoalDifferences(path = "Data", country = input$countryOverview))
        # #     output$gspm <- renderDataTable(GSPM(path = "Data", country = input$countryOverview))
        # #     output$all <- renderDataTable(LoadCountryData(path = "Data", country = input$countryOverview, filename = "Synopsis.csv")[, 1:10],
        # #                                     options = list(pageLength = 100))

        # })

        output$teamData <- renderDataTable(OneTeamsData(input$teamChoice, "Data", input$teamCountry)[, 2:14],
                                        options = list(pageLength = 100))
        
        output$compare <- renderDataTable(teamComparison(input$home, input$away)[, 2:10],
                                          options = list(pageLength = 100)
        )

        output$compareAll <- renderDataTable(allGamesComparison(input$home, input$away)[, 2:10],
                                             options = list(pageLength = 100)
        )
        
        output$overview <- renderDataTable(sumUpComparison(input$home, input$away))

        output$overviewAll <- renderDataTable(overviewAll(input$home, input$away))

        # output$teamData <- renderDataTable(extractOneTeam(input$teamChoice)[, 2:14],
        #                                    options = list(pageLength = 100))

        output$sumTeamData <- renderDataTable(sumUpOneTeam(input$teamChoice)[, 2:14],
                                           options = list(pageLength = 100))

        # observeEvent(input$reload,

        #     return({
        #       withProgress(session, min=1, max=2, {
        #         setProgress(message = 'Calculation in progress',
        #                     detail = 'This may take a while...')
        #         setProgress(value = 1)
        #         Initialize()
        #         setProgress(value = 2)
        #       })
        #     })

        # )
        
    }
)