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
    division <- strsplit(x = list.files(path = paste("Data", preferences[1], sep = "/"), pattern = ".csv"), split = ".csv")
    updateSelectInput(session, "countryOverview", choices = GetCountries(), selected = preferences[1])
    updateSelectInput(session, "leagueOverview", choices = division, selected = preferences[2])
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

        observeEvent(input$countryOverview, {

            division <- strsplit(x = list.files(path = paste("Data", country = input$countryOverview,
                                                sep = "/"), pattern = ".csv"), split = ".csv")

            if (input$countryOverview == preferences[1]) {
                updateSelectInput(session, "leagueOverview",
                            choices = division,
                            selected = preferences[2])
            } else {
                updateSelectInput(session, "leagueOverview",
                            choices = division,
                            selected = division[1])
            }

            output$wins <- renderDataTable(Wins(path = "Data", country = input$countryOverview))
            output$results <- renderDataTable(ResultsComparison(path = "Data", country = input$countryOverview, division = input$leagueOverview))
            output$goalDifferences <- renderDataTable(GoalDifferences(path = "Data", country = input$countryOverview))
            output$gspm <- renderDataTable(GSPM(path = "Data", country = input$countryOverview))
            output$all <- renderDataTable(LoadCountryData(path = "Data", country = input$countryOverview, filename = "Synopsis.csv")[, 1:10],
                                            options = list(pageLength = 100))

        })

        observeEvent(input$teamCountry, {

            division <- strsplit(x = list.files(path = paste("Data", country = input$teamCountry,
                                                sep = "/"), pattern = ".csv"), split = ".csv")
            team <- FolderStructure(path = paste("Data", input$teamCountry, sep = "/"))

            if (input$teamCountry == preferences[1]) {
                updateSelectInput(session, "teamDivision",
                            choices = division,
                            selected = preferences[2])
            } else {
                updateSelectInput(session, "teamDivision",
                            choices = division,
                            selected = division[1])
            }

        })

        observeEvent(input$teamDivision, {

            team <- FolderStructure(path = paste("Data", input$teamCountry, sep = "/"))

            if (input$teamCountry == preferences[1]) {
                updateSelectInput(session, "teamChoice",
                            choices = team,
                            selected = preferences[3])
            } else {
                updateSelectInput(session, "teamChoice",
                            choices = team,
                            selected = team[1])
            }

        })

        output$allMatches <- renderDataTable(OneTeamsData(input$teamChoice, "B", "Data", input$teamCountry)[, 1:10],
                                        options = list(pageLength = 100))
        output$homeMatches <- renderDataTable(OneTeamsData(input$teamChoice, "H", "Data", input$teamCountry)[, 1:10],
                                        options = list(pageLength = 100))
        output$awayMatches <- renderDataTable(OneTeamsData(input$teamChoice, "A", "Data", input$teamCountry)[, 1:10],
                                        options = list(pageLength = 100))
        output$divisionMatches <- renderDataTable(MatchesByDivision(input$teamChoice, "Data", input$teamCountry, input$teamDivision)[, 1:10],
                                        options = list(pageLength = 100))
        output$overviewMatches <- renderDataTable(TeamOverview(input$teamChoice, "Data", input$teamCountry),
                                        options = list(pageLength = 100))
        output$last2 <- renderDataTable(LastMatches(input$teamChoice, 2, "Data", input$teamCountry)[, 1:10],
                                        options = list(pageLength = 100))
        output$last5 <- renderDataTable(LastMatches(input$teamChoice, 5, "Data", input$teamCountry)[, 1:10],
                                        options = list(pageLength = 100))
        output$last10 <- renderDataTable(LastMatches(input$teamChoice, 10, "Data", input$teamCountry)[, 1:10],
                                        options = list(pageLength = 100))
        output$allMatchesTrend <- renderPlot({
            data <- TrendPlot(input$teamChoice, "B", "Data", input$teamCountry)
            plot(data[[1]], data[[2]], type = "s", xlab = "Date", ylab = "Points", axes = FALSE)
            dat1 <- data[[1]]
            dat3 <- data[[3]]
            axis(1, at = c(dat1[1], dat1[seq(1, length(dat1), 34)], dat1[length(dat1)]),
                    labels = c(dat3[1], dat3[seq(1, length(dat3), 34)], dat3[length(dat3)]))
            axis(2)
        })
        output$homeMatchesTrend <- renderPlot({
            data <- TrendPlot(input$teamChoice, "H", "Data", input$teamCountry)
            plot(data[[1]], data[[2]], type = "s", xlab = "Date", ylab = "Points", axes = FALSE)
            dat1 <- data[[1]]
            dat3 <- data[[3]]
            axis(1, at = c(dat1[1], dat1[seq(1, length(dat1), 34)], dat1[length(dat1)]),
                    labels = c(dat3[1], dat3[seq(1, length(dat3), 34)], dat3[length(dat3)]))
            axis(2)
        })
        output$awayMatchesTrend <- renderPlot({
            data <- TrendPlot(input$teamChoice, "A", "Data", input$teamCountry)
            plot(data[[1]], data[[2]], type = "s", xlab = "Date", ylab = "Points", axes = FALSE)
            dat1 <- data[[1]]
            dat3 <- data[[3]]
            axis(1, at = c(dat1[1], dat1[seq(1, length(dat1), 34)], dat1[length(dat1)]),
                    labels = c(dat3[1], dat3[seq(1, length(dat3), 34)], dat3[length(dat3)]))
            axis(2)
        })

        output$allMatchesBoxPlot <- renderPlot({
            data <- TrendPlot(input$teamChoice, "B", "Data", input$teamCountry)
            boxplot(data[[4]]/length(data[[4]]), ylim = c(0, 3))
        })
        output$homeMatchesBoxPlot <- renderPlot({
            data <- TrendPlot(input$teamChoice, "H", "Data", input$teamCountry)
            boxplot(data[[4]]/length(data[[4]]), ylim = c(0, 3))
        })
        output$awayMatchesBoxPlot <- renderPlot({
            data <- TrendPlot(input$teamChoice, "A", "Data", input$teamCountry)
            boxplot(data[[4]]/length(data[[4]]), ylim = c(0, 3))
        })
        
        output$compare <- renderDataTable(TeamComparison(input$home, input$away)[, 2:10],
                                          options = list(pageLength = 100)
        )

        output$compareAll <- renderDataTable(AllGamesComparison(input$home, input$away)[, 2:10],
                                             options = list(pageLength = 100)
        )
        
        output$overview <- renderDataTable(SumUpComparison(input$home, input$away))

        output$overviewAll <- renderDataTable(OverviewAll(input$home, input$away))

        output$hvaBoxPlot <- renderPlot({
            hData <- TrendPlot(input$home, "H", "Data", input$teamCountry)
            aData <- TrendPlot(input$away, "A", "Data", input$teamCountry)
            boxplot(hData[[4]]/length(hData[[4]]), aData[[4]]/length(aData[[4]]), ylim = c(0, 3))
            print(hData[[4]])
        })

        output$hvaAllBoxPlot <- renderPlot({
            data <- Trend(input$home, input$away)
            boxplot(data[[1]]/length(data[[1]]), data[[2]]/length(data[[2]]), ylim = c(0, 3))
        })

        output$lmTest <- renderPlot({
            data <- TeamComparison(input$home, input$away)[, 2:10]
            hData <- rep(0, nrow(data))
            aData <- rep(0, nrow(data))
            for (i in 1:nrow(data)) {
                if (data[i, 6] == "H") {
                    hData[i] <- 1
                    aData[i] <- -1
                } else if (data[i, 6] == "A") {
                    hData[i] <- -1
                    aData[i] <- 1
                }
            }
            # x <- c(hData[[4]], aData[[4]])
            # g <- factor(rep(2, length(x)),
            #             labels = c("Normal subjects",
            #                        "Subjects with asbestosis"))
            test <- cor(data.frame(hData, aData))
            print(symnum(test))
            # test <- cov(hData ~ aData)
            # print(length(x))
            # print(length(g))
            # test1 <- kruskal.test(hData ~ aData)
            # output$kruskal <- renderText({as.character(test1)})
            # test <- lm(hData[[4]] ~ aData[[4]])
            # plot(test)
            plot(test)
        })

        # output$sumTeamData <- renderDataTable(SumUpOneTeam(input$teamChoice)[, 2:14],
        #                                    options = list(pageLength = 100))

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