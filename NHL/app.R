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
library(ggpie)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NHL Exploratory Data"),

    
    navset_pill(
      
      #About tab: discuss the premise and purpose of this project.
      
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
      
      #Data Download tab: allow user to select applicable data and save it locally if they wish
      
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
                    div("NOTE: You will not see any data if you try to view playoffs statistics for a team that did not make the playoffs in that season.")
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
      
      #Data Exploration tab: allow user to see visualizations of the data they chose in the Data Download tab.
      # Team stats
      # 1) Scatterplot
      #    - User will pick the two variables they wish to use
      #    - User will be able to facet by player position
      # 2) Contingency
      #    - Impact Player vs. Position measured by goals assists and points
      # 3) Pie Chart?
      #    - Visualize goals scored as  % of team contributions
      # Leader stats
      # 4) bar graph
      #    - display value of each player as a bar with value being the x axis
      
      nav_panel("Data Exploration",
                sidebarPanel(
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics'",
                                   selectInput("chooseVis", "Choose an Output", choices = c("Scatterplot", "Contingency Table", "Pie Chart")),
                                   conditionalPanel(condition = "input.chooseVis == 'Scatterplot'",
                                                    selectInput("chooseX", "Choose your X-axis", choices = c()),
                                                    selectInput("chooseY", "Choose your Y-axis", choices = c()),
                                                    checkboxInput("facet", "Facet by Position", FALSE)
                                                    ),
                                   div("NOTE: You must have selected regular season statistics for skaters to view the pie chart."),
                                   div("You must have selected skaters to view the contingency table as well."),
                                   div("---")
                                   ),
                  conditionalPanel(condition = "input.endpoint == 'Leader Statistics'",
                                   ),
                  div("You are currently viewing the data you selected in the Data Download tab! If you'd like to view new data, please re-select your choices in the Data Download tab.")
                ),
                mainPanel(
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics' && input.chooseVis == 'Scatterplot'",
                                   plotOutput("scatter")
                                   ),
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics' && input.chooseVis == 'Contingency Table' && input.position == 'Skaters'",
                                   tableOutput("contingency"),
                                   div("The above table is a count of how many players per position qualify as impact players on the team you have currently selected."),
                                   div("I define an impact player as a player that has a higher average time on ice per game than what would be expected if a team used all of their players equally.
                                       For forwards, this is 15 minutes per game (60 minutes divided by 4 groups of forwards). For defensemen, this is 20 minutes per game (60 minutes divided by 3 pairs of defensemen)."),
                                   div("The logic behind this decision is that an NHL coach wants to play their best players more often, and thus they would have a greater impact on the game since they have more time on the ice."),
                                   tableOutput("summary"),
                                   div("NOTE: Any NA's displayed are for factor groupings where only 1 observation was recorded")
                                   ),
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics' && input.chooseVis == 'Pie Chart' && input.position == 'Skaters' && !(input.playoffs)",
                                   plotOutput("pie"),
                                   div("Teams that rely on their impact players disproportionately will have a few players taking up a significant amount of space on the chart.
                                       This is helpful for seeing how much teams rely on their best players to win games!"),
                                   div("NOTES: only players who have played more than 10 games during the season are shown above.")
                  ),
                  conditionalPanel(condition = "input.endpoint == 'Leader Statistics'",
                                   plotOutput("bar")
                  )
                ),
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    #Load a table that is reactive to user input in the Data Download tab
    dataset <- reactive ({
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
      dataset()
    })
    
    output$save <- downloadHandler(
      filename = "NHL_Data.csv",
      content = function(file) {
        write.csv(dataset(), file, row.names = FALSE)
      }
    )
    
    output$scatter <- renderPlot({
      if (input$position == "Skaters") {
        ggplot(dataset(), aes(x = , y = )) +
          geom_point()
      }
    })
    
    output$contingency <- renderTable({
      cont <- xtabs(~positionCode+impactPlayer,data = dataset())
    })
    
    output$summary <- renderTable({
      if (input$position == "Skaters") {
        dataset() |>
          group_by(positionCode, impactPlayer) |>
          summarize(meanGoals = mean(goals),
                    stddevGoals = sd(goals),
                    meanAssists = mean(assists),
                    stddevAssists = sd(assists),
                    meanPoints = mean(points),
                    stddevPoints = sd(points))
      }
      
    })
    
    output$pie <- renderPlot({
      
      pie_data <- dataset() |>
        filter(gamesPlayed > 10) |>
        select(lastName, points) |>
        rename(count = points)
      
      ggpie(data = pie_data, group_key = "lastName", label_pos = "out")
      
    })
    
    output$bar <- renderPlot({
      
      #Two different endpoints are possible sources for this plot, so two separate cases are needed.
      
      if (input$position == "Skaters") {
        ggplot(data = dataset(), aes(x = fullName, y = value)) +
          geom_bar(stat = "identity", color = "black", fill = "blue") +
          ggtitle(paste("NHL Leaders in", input$stat_skater, "for the", input$season, "Season"), subtitle = "Skaters") +
          xlab("Name") + ylab("Count")
      } else {
        ggplot(data = dataset(), aes(x = lastName, y = value)) +
          geom_bar(stat = "identity", color = "black", fill = "orange") +
          ggtitle(paste("NHL Leaders in", input$stat_goalie, "for the", input$season, "Season"), subtitle = "Goalies") +
          xlab("Name") + ylab("Count")
      }
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
