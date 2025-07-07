# --- Load packages ---

library(shiny)
library(bslib)
library(ggpie)

# --- Load Helper Functions ---

source("helpers.R")

# Define UI

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
                  #Choose an endpoint
                  selectInput("endpoint", "Choose Dataset", choices = c("Team Statistics", "Leader Statistics")),
                  
                  #Choose a season
                  textInput("season", "Enter a season", value = "20242025"),
                  div("Enter a season as the two years that the season took place in with no extra characters. For example, the 2024-2025 NHL Season is '20242025'."),
                  
                  #Choose skaters or goalies
                  selectInput("position", "Choose a position", choices = c("Skaters", "Goalies")),
                  
                  #Choose a team to view statistics for
                  conditionalPanel(
                    condition = "input.endpoint == 'Team Statistics'",
                    selectInput("team", "Choose a team", choices = team_abbr),
                    checkboxInput("playoffs", "View Playoff Statistics", FALSE),
                    div("NOTE: You will not see any data if you try to view playoffs statistics for a team that did not make the playoffs in that season.")
                  ),
                  
                  #Choose the relevant statistic and parameters surrounding the leader endpoint
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
                  
                  #Save data if desired
                  downloadButton("save", "Save Data")
                ),
                
                #Display data
                mainPanel(
                  card(tableOutput("data"))
                  
                )
                ),
      
      #Data Exploration tab: allow user to see visualizations of the data they chose in the Data Download tab.
      
      nav_panel("Data Exploration",
                
                
                sidebarPanel(
                  
                  #Allow parameters for different displays to be selected
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics'",
                                   
                                   #Choose visualization to view
                                   selectInput("chooseVis", "Choose an Output", choices = c("Scatterplot", "Numerical Summaries", "Pie Chart", "Bar Chart")),
                                   
                                   #Choose statistics to view in scatterplot as well as faceting options
                                   conditionalPanel(condition = "input.chooseVis == 'Scatterplot' && input.position == 'Skaters'",
                                                    selectInput("chooseX_skaters", "Choose your X-axis", choices = c("gamesPlayed", "goals", 
                                                                                                             "assists", "points", "penaltyMinutes", "shots", 
                                                                                                             "shootingPctg", "avgTimeOnIcePerGame", "avgShiftsPerGame", 
                                                                                                             "faceoffWinPctg"),
                                                                selected = "avgTimeOnIcePerGame"),
                                                    selectInput("chooseY_skaters", "Choose your Y-axis", choices = c("gamesPlayed", "goals", 
                                                                                                             "assists", "points", "penaltyMinutes", "shots", 
                                                                                                             "shootingPctg", "avgTimeOnIcePerGame", "avgShiftsPerGame", 
                                                                                                             "faceoffWinPctg"),
                                                                selected = "points"),
                                                    checkboxInput("facet", "Enable Faceting", FALSE),
                                                    conditionalPanel(condition = "input.facet",
                                                                     selectInput("facetVar", "Choose a Faceting Variable", choices = c("positionCode", "impactPlayer")))
                                   ),
                                   
                                   #Choose inputs to view in scatterplot for goalies specifically
                                   conditionalPanel(condition = "input.chooseVis == 'Scatterplot' && input.position == 'Goalies'",
                                                    selectInput("chooseX_goalies", "Choose your X-axis", choices = c("gamesPlayed", "gamesStarted", 
                                                                                                                     "wins", "losses", "overtimeLosses", "goalsAgainstAverage", 
                                                                                                                     "savePercentage", "shotsAgainst", "saves", "goalsAgainst", "shutout"),
                                                                selected = "savePercentage"),
                                                    selectInput("chooseY_goalies", "Choose your Y-axis", choices = c("gamesPlayed", "gamesStarted", 
                                                                                                                     "wins", "losses", "overtimeLosses", "goalsAgainstAverage", 
                                                                                                                     "savePercentage", "shotsAgainst", "saves", "goalsAgainst", "shutout"),
                                                                selected = "goalsAgainstAverage"),
                                   ),
                                   
                                   #Edge cases that don't display data noted here
                                   
                                   div("NOTE: You must have selected regular season statistics to view the pie chart."),
                                   div("You must have selected skaters to view the contingency table."),
                                   div("---")
                                   ),
                  
                  div("You are currently viewing the data you selected in the Data Download tab! If you'd like to view new data, please re-select your choices in the Data Download tab.")
                ),
                
                #Display each dataset individually based on user selection
                mainPanel(
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics' && input.chooseVis == 'Scatterplot'",
                                   plotOutput("scatter")
                                   ),
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics' && input.chooseVis == 'Numerical Summaries' && input.position == 'Skaters'",
                                   tableOutput("contingency"),
                                   div("The above table is a count of how many players per position qualify as impact players on the team you have currently selected."),
                                   div("I define an impact player as a player that has a higher average time on ice per game than what would be expected if a team used all of their players equally.
                                       For forwards, this is 15 minutes per game (60 minutes divided by 4 groups of forwards). For defensemen, this is 20 minutes per game (60 minutes divided by 3 pairs of defensemen)."),
                                   div("The logic behind this decision is that an NHL coach wants to play their best players more often, and thus they would have a greater impact on the game since they have more time on the ice."),
                                   tableOutput("summary"),
                                   div("NOTE: Any NA's displayed are for factor groupings where only 1 observation was recorded")
                                   ),
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics' && input.chooseVis == 'Pie Chart' && !(input.playoffs)",
                                   plotOutput("pie"),
                                   div("Teams that rely on their impact players disproportionately will have a few players taking up a significant amount of space on the chart.
                                       This is helpful for seeing how much teams rely on their best players to win games!"),
                                   div("Teams with goalies who make the majority of their saves are those that typically have a dedicated starter and backup. These teams typically fare well in the playoffs!"),
                                   div("NOTES: only players who have played more than 10 games during the season are shown above.")
                  ),
                  conditionalPanel(condition = "input.endpoint == 'Team Statistics' && input.chooseVis == 'Bar Chart'",
                                   plotOutput("bar_grouped")),
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

    # Display dataset in Data Download tab
    output$data <- renderTable({
      dataset()
    })
    
    # Allow user to save data if desired
    output$save <- downloadHandler(
      filename = "NHL_Data.csv",
      content = function(file) {
        write.csv(dataset(), file, row.names = FALSE)
      }
    )
    
    #Render contingency tables/summaries/plots
    output$scatter <- renderPlot({
      if (input$position == "Skaters") {
        
        plot <- ggplot(dataset(), aes(x = !!sym(input$chooseX_skaters), y = !!sym(input$chooseY_skaters), color = positionCode)) +
          geom_point() +
          ggtitle(paste(input$chooseX_skaters, "vs.", input$chooseY_skaters, "for", input$team, input$season))
        
        if(input$facet == TRUE) {
          plot <- plot +
            facet_grid(. ~ get(input$facetVar))
        }
        
        plot
        
      } else if (input$position == "Goalies") {
        
        ggplot(dataset(), aes(x = !!sym(input$chooseX_goalies), y = !!sym(input$chooseY_goalies))) +
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
      
    if (input$position == "Skaters") {
      pie_data <- dataset() |>
        filter(gamesPlayed > 10) |>
        select(lastName, points) |>
        rename(count = points)
      
      ggpie(data = pie_data, group_key = "lastName", label_pos = "out") +
        ggtitle("Share of Goals Scored per Player")
      
    } else if (input$position == "Goalies") {
      
      pie_data <- dataset() |>
        select(lastName, saves) |>
        rename(count = saves)
      
      ggpie(data = pie_data, group_key = "lastName", label_pos = "out") +
        ggtitle("Share of Saves Made per Player")
      
    }
      
      
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
    
    output$bar_grouped <- renderPlot({
      
      if (input$position == "Skaters") {
        
        ggplot(data = dataset(), aes(x = positionCode, y = points, fill = impactPlayer)) +
          geom_bar(position = "dodge", stat = "identity") +
          ggtitle(paste("Points breakdown across lineup for", input$team, "in the", input$season, "Season"))
        
      } else if (input$position == "Goalies") {
        
        ggplot(data = dataset(), aes(x = lastName, y = goalsAgainstAverage)) +
          geom_bar(stat = "identity", fill = "orange") +
          ggtitle(paste("Goals Against Average (GAA) for", input$team, "Goalies in the", input$season, "Season"))
        
      }
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
