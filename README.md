# About This Project

This is an R Shiny app designed to view data pertaining to the National Hockey League (NHL) and explore some high-level data pertaining to team-wide statistics as well as viewing the leaders in several key statistics! All data is sourced from the [NHL API](https://gitlab.com/dword4/nhlapi). Special thanks to dword4 and Zmalski for their helpful reference documentation!

## Required Packages

The following is a list of packages necessary for this app to run:

* shiny
* ggplot2
* httr
* jsonlite
* tidyverse
* bslib
* ggpie

You can install these packages by running the following code in your RStudio console:
`install.packages(c("shiny","ggplot2","httr","jsonlite","tidyverse","bslib","ggpie"))`

## Initializing the App

Run the following code in your RStudio console to get started!
`shiny::runGitHub("Project2", "maxscampbell")`

## Special Thanks

Yabing Song (2022). ggpie: Create Pie, Donut and Rose Pie Plot with
'ggplot2'. R package version 0.2.5.
https://CRAN.R-project.org/package=ggpie

Zmalski and dword4 for their documentation on the NHL API
