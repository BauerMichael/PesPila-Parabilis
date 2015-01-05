library(shiny)

shinyUI(fluidPage(
    titlePanel("Testpage"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Overall statistics."),
            
            selectInput("var", 
                        label = "Choose a statistic to display",
                        choices = c("Goals", "Results", "Difference", "GSPM", "All"),
                        selected = "Goals"),
            helpText("Home team selection"),
            selectInput("home",
                        label = "Choose a home team",
                        choices = loadTeams(),
                        selected = "Bayern Munich"),
            helpText("Away team selection"),
            selectInput("away",
                        label = "Choose a home team",
                        choices = loadTeams(),
                        selected = "Wolfsburg")
    ),
    
    mainPanel(
      tableOutput("stats"),
      tableOutput("compare")
    )
  )
))