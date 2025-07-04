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
                #Content goes here
                ),
      nav_panel("Data Exploration",
                #Content goes here
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
