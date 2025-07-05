#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NHL Exploratory Data"),

    # Sidebar with a slider input for number of bins 
    navset_pill(
      nav_panel("About",
                div("This is an R Shiny app designed to explore a 
                  subset of NHL Data in an exploratory manner!"),
                div("The data here is sourced from the official NHL API. While there is no official documentation, an unofficial reference can be found",
                         a("here", href = "https://github.com/Zmalski/NHL-API-Reference")),
                div("Thanks Zmalski!"),
                h2("Usage"),
                h3("Data Download"),
                div("In the Data Download tab, you can view the data directly after it was pulled and cleaned from the API."),
                div("You can also customize inputs, so you can view different teams, player positions, etc."),
                div("You can even save the dataset that you create!"),
                h3("Data Exploration"),
                div("In this tab, you will be able to see visualizations of the data you have selected!"),
                div("Included are scatterplots, contingency tables, etc."),
                img(src = "nhl.png", align = "center")
                ),
      nav_panel("Data Download",
                sidebarPanel(
                  selectInput("endpoint", "Choose Dataset", choices = c("Team Statistics", "Leader Statistics")),
                  textInput("season", "Enter a season", value = "20242025"),
                  div("Enter a season as the two years that the season took place in with no extra characters. For example, the 2024-2025 NHL Season is '20242025'."),
                  selectInput("position", "Choose a position", choices = c("Skaters", "Goalies")),
                  conditionalPanel(
                    condition = "input.endpoint == 'Team Statistics'",
                    selectInput("team", "Choose a team", choices = team_abbr),
                    checkboxInput("playoffs", "View Playoff Statistics", FALSE),
                    div("NOTE: Program will return an error if you try to view playoffs statistics for a team that did not make the playoffs in that season.")
                  ),
                  conditionalPanel(
                    condition = "input.endpoint == 'Leader Statistics'",
                    sliderInput("limit", "Set entry count", min = 1, max = 25, value = 5),
                    conditionalPanel(
                      condition = "input.position == 'Skaters'",
                      selectInput("stat_skater", "Choose a Statistic", choices = c("Goals", "Assists", "Points"))
                    ),
                    conditionalPanel(
                      condition = "input.position == 'Goalies'",
                      selectInput("stat_goalie", "Choose a Statistic", choices = c("Wins", "Shutouts"))
                    )
                  ),
                  downloadButton("save", "Save Data")
                ),
                mainPanel(
                  card(tableOutput("data"))
                )
                ),
      nav_panel("Data Exploration",
                #Content goes here
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    table <- reactive ({
      if(input$endpoint == "Team Statistics") {
        end <- "team"
        
        arguments <- list(
          tolower(input$position),
          input$playoffs,
          input$team,
          input$season
        )
        
      } else if (input$endpoint == "Leader Statistics" && input$position == "Skaters") {
        end <- "skaters"
        
        stat <- switch(input$stat_skater,
                       "Goals" = "goals",
                       "Assists" = "assists",
                       "Points" = "points",
        )
        
        arguments <- list(
          tolower(input$position),
          stat,
          input$limit,
          input$season
        )
        
      } else if (input$endpoint == "Leader Statistics" && input$position == "Goalies") {
        end <- "goalies" 
        
        stat <- switch(input$stat_goalie,
                       "Wins" = "wins",
                       "Shutouts" = "shutouts"
        )
        
        
        arguments <- list(
          tolower(input$position),
          stat,
          input$limit,
          input$season
        )
      }
      
      fetchInput(endpoint = end, arguments)
    })

    output$data <- renderTable({
      table()
    })
    
    output$save <- downloadHandler(
      filename = "NHL_Data.csv",
      content = function(file) {
        write.csv(table(), file, row.names = FALSE)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
