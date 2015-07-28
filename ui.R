library(shiny)
library(hash)

options(warn=-1)

source("global.R")

shinyUI(

    navbarPage("PesPila-Parabilis",

        tabPanel("home",

          div(class = "col-sm-10 col-sm-offset-1 jumbotron text-center",
            tabsetPanel(
              tabPanel("Home", 
                  tags$h1("PesPila-Parabilis"),
                  tags$h3("A tool to predict the outcome of football matches.")
              ),
              tabPanel("Preferences",
                tags$h2("Your preferences are"),
                fluidRow(
                  column(2, offset = 3,
                    helpText("Country:"),
                    textOutput("myCountry")
                  ),
                  column(2,
                    helpText("Division:"),
                    textOutput("myDivision")
                  ),
                  column(2,
                    helpText("Team:"),
                    textOutput("myTeam")
                  )
                ),
                tags$br(),
                tags$h2("Set new preferences"),
                fluidRow(
                  column(4,
                    selectInput("country", label = "Choose country", choices = "", selected = "")
                  ),
                  column(4,
                    selectInput("division", label = "Choose division", choices = "", selected = "")
                  ),
                  column(4,
                    selectInput("theTeam", label = "Choose favourite team", choices = "", selected = "")
                  )
                ),
                div(class = "text-center",
                  actionButton("setPreferences", "Set new Preferences")
                )
              ),
              # tabPanel("Update data sources", actionButton("reload", "Update the data sets.")),
              tabPanel("Data sources and literature",

                tags$h2("Source of the datasets"),
                div(class = "text-center", 

                  "Please visit: ", tags$a(href = "http://www.football-data.co.uk", "Football-Data.co.uk", target = "blank")

                )

              )
            )
          )

          # div(class = "col-sm-10 col-sm-offset-1 jumbotron text-center",
          #   tags$h1("PesPila-Parabilis"),
          #   tags$h3("A tool to predict the outcome of football matches in the Bundesliga"),
          #   tags$br(),
          #   tags$br(),
          #   tags$h4("These days the word Big Data is in everyones mouth. Big companies try to predict how stock markets change or advertisement
          #             drives the sales, but also the consumers behaviour is a big deal. There are other interesting things which could
          #             be done with an amount of data: bioinformatics, mathematics and of course prediction models for football bets.
          #             Well, to make this clear: I am not interested in betting or making money with this - I wouldn't even recommend my
          #             tool to make money ;) - but I am interested in programming and how I can use data in a useful sense.
          #             Maybe the colors of this website already gives you a guess that I am a FC Bayern Munich fan. So my main purpose was
          #             to look, if I can find out the outcome of their games. In the first beta version (which was a mess) the tool said:
          #             'VfL Wolfsburg will beat Bayern Munich'. My first thougth was: 'Hahaha'. But as Wolfsburg won 4-1 I knew some day
          #             I need to re-develope this tool and make a great web interface for it. So, here we are. Test it, try it, enjoy it!"),
          #   tags$br(),
          #   tags$h4("Michael", class = "text-right")
          # ),
        ),
        tabPanel("general data",

          fluidRow(
              column(3, offset = 1,
                  sidebarPanel(width = 12,
                          
                          selectInput("countryOverview", 
                                      label = "Choose a country",
                                      choices = GetCountries(),
                                      selected = "Germany"),
                          selectInput("leagueOverview", 
                                      label = "Choose a league (for Results table)",
                                      choices = strsplit(x = list.files(path = paste("Data", "Germany", sep = "/"), pattern = ".csv"), split = ".csv"),
                                      selected = "Bundesliga1")
                  )
              ),
              column(7,
                  tabsetPanel(
                      tabPanel("Wins", dataTableOutput("wins")),
                      # tabPanel("Results",
                      #   tabsetPanel(
                      #     tabPanel("1st League", dataTableOutput("results1")),
                      #     tabPanel("2nd League", dataTableOutput("results2")),
                      #     tabPanel("3rd League", dataTableOutput("results3")),
                      #     tabPanel("4th League", dataTableOutput("results4")),
                      #     tabPanel("5th League", dataTableOutput("results5"))
                      #   )
                      # ),
                      tabPanel("Results", dataTableOutput("results")),
                      tabPanel("Goal Differences", dataTableOutput("goalDifferences")),
                      tabPanel("Goal Sums Per Match (GSPM)", dataTableOutput("gspm")),
                      tabPanel("All Games", dataTableOutput("all"))
                  )
              )
          )
        ),
        tabPanel("data per team",

          fluidRow(
              column(3,
                  sidebarPanel(width = 12,

                          selectInput("team",
                                      label = "Select prefered team",
                                      choices = loadTeams(),
                                      selected = "Bayern Munich")
                  ),

                  sidebarPanel(width = 12, id = "legend",
                      div("Description of the shortcuts:", id = "header"),
                      br(),
                      div("FTHG = Full Time Home Goals"),
                      div("FTAG = Full Time Away Goals"),
                      div("FTR = Full Time Result"),
                      div("HTHG = Half Time Home Goals"),
                      div("HTAG = Half Time Away Goals"),
                      div("HTR = Half Time Result")
                  )
              ),
              column(8,
                  tabsetPanel(
                      tabPanel("Team data", dataTableOutput("teamData")),
                      tabPanel("Overview: team data", dataTableOutput("sumTeamData"))
                  )
              )
          )
        ),
        tabPanel("result prediction",

          fluidRow(
              column(3,
                  sidebarPanel(width = 12,

                          selectInput("home",
                                      label = "Choose a home team",
                                      choices = loadTeams(),
                                      selected = "Bayern Munich"),

                          selectInput("away",
                                      label = "Choose a home team",
                                      choices = loadTeams(),
                                      selected = "Wolfsburg")
                  ),
                  sidebarPanel(width = 12, id = "legend",
                      div("Description of the shortcuts:", id = "header"),
                      br(),
                      div("FTHG = Full Time Home Goals"),
                      div("FTAG = Full Time Away Goals"),
                      div("FTR = Full Time Result"),
                      div("HTHG = Half Time Home Goals"),
                      div("HTAG = Half Time Away Goals"),
                      div("HTR = Half Time Result")
                  )
              ),
              column(8,
                  tabsetPanel(
                      tabPanel("Home vs. Away", dataTableOutput("compare")),
                      tabPanel("Overview: Home vs. Away", dataTableOutput("overview")),
                      tabPanel("All outcomes of the two teams", dataTableOutput("compareAll")),
                      tabPanel("Overview: All outcomes", dataTableOutput("overviewAll"))
                  )
              )
          )
        ),
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "/css/bootstrap.min.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "/css/styles.css"),
            tags$script(src = "/js/scripts.js")
        )
    )
)