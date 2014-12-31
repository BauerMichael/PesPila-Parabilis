library(shiny)

shinyUI(fluidPage(
    titlePanel("Testpage"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
                     information from the 2010 US Census."),
      
      selectInput("var", 
        label = "Choose a statistic to display",
        choices = c("Goals", "Results", "All"),
        selected = "Goals")
    ),
    
    mainPanel(
      tableOutput("stats")
    )
  )
))