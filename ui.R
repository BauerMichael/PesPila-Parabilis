library(shiny)

source("global.R")

shinyUI(

    navbarPage("PesPila-Parabilis",

        tabPanel("home",

          div(class = "jumbotron text-center",
            tags$h1("PesPila-Parabilis"),
            tags$h3("A tool to predict the outcome of football matches in the Bundesliga"),
            tags$br(),
            tags$br(),
            tags$h4("These days the word Big Data is in everyones mouth. Big companies try to predict how stock markets change or advertisement
                      drives the sales, but also the consumers behaviour is a big deal. There are other interesting things which could
                      be done with an amount of data: bioinformatics, mathematics and of course prediction models for football bets.
                      Well, to make this clear: I am not interested in betting or making money with this - I wouldn't even recommend my
                      tool to make money ;) - but I am interested in programming and how I can use data in a useful sense.
                      Maybe the colors of this website already gives you a guess that I am a FC Bayern Munich fan. So my main purpose was
                      to look, if I can find out the outcome of their games. In the first beta version (which was a mess) the tool said:
                      'VfL Wolfsburg will beat Bayern Munich'. My first thougth was: 'Hahaha'. But as Wolfsburg won 4-1 I knew some day
                      I need to re-develope this tool and make a great web interface for it. So, here we are. Test it, try it, enjoy it!"),
            tags$br(),
            tags$h4("Michael", class = "text-right")
          ),
          div(class = "text-center",
            actionButton("reload", "Update the data sets.")
          ),
          textOutput("test")
        ),
        tabPanel("general data",

          fluidRow(class = "dataTable",
              column(3, offset = 1,
                  sidebarPanel(width = 12,
                          
                          selectInput("var", 
                                      label = "Choose a statistic to display",
                                      choices = c("Goals", "Results", "Difference", "GSPM", "All"),
                                      selected = "Goals")

                  )
              ),
              column(6,
                  tabsetPanel(
                      tabPanel("Overview", dataTableOutput("stats"))
                  )
              )
          )
        ),
        tabPanel("teams data",

          fluidRow(class = "dataTable",
              column(3, offset = 1,
                  sidebarPanel(width = 12
                      
                  )
              ),
              column(6,
                  tabsetPanel(
                  )
              )
          )
        ),
        tabPanel("result prediction",

          fluidRow(class = "dataTable",
              column(3, offset = 1,
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
                      tabPanel("All outcomes of the two teams", dataTableOutput("compareAll")),
                      tabPanel("Overview: Home vs. Away", dataTableOutput("overview"))
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