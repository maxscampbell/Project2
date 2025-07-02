library(httr)
library(jsonlite)
library(tidyverse)

#---------- USEFUL DATA ---------- 


#---------- HELPER FUNCTIONS ---------- 

#Base function to get queries
getQuery <- function(url) {
  returned_raw <- httr::GET(url)
  returned <- fromJSON(rawToChar(returned_raw$content))
  
  return(returned)
}

#Some ideas
# 1) shot chart
#    - user requests a game, shot statistics are displayed for one and/or both teams
# 2) team statistics
#    - display common statistics at a team scale (and maybe rank them)
# 3) stats leaders
#    - display the top 5 in goals, assists, hits, and sv%
# 4) sv% charts
#    - display a goalie's sv% over the course of the season to identify good/bad form

#Endpoint/params
# 2) Team Stats
#    - Specify skaters and goalies
#    - Specify regular season or playoffs
# 3) Leaders
#    - Specify goalie or skater
#    - specify desired stat (goals, assists, points, hits for skater, 
#        sv%, gaa, wins for goalie)
#    - Specify limit (top 1-10)
#    - Specify season

team_stats <- getQuery("https://api-web.nhle.com/v1/club-stats/TOR/20242025/2")
skaters <- getQuery("https://api-web.nhle.com/v1/skater-stats-leaders/current?categories=goals&limit=5")

#Master function to build query URLs
urlBuilder <- function(endpoint = "team", args) {
  #Sanity check: make sure arguments are in correct data structure
  if (class(args) != "list") {
    stop("Arguments are not in a list!")
  }
  
  #Base URL to send to helpers
  base <- "https://api-web.nhle.com/v1"
  
  #Determine which helper function to use to build URL
  if (endpoint == "team") {
    return(teamStatsBuilder(base, args))
  } else if (endpoint == "skaters" || endpoint == "goalies") {
    return(leaderStatsBuilder(base, args))
  } else {
    # return a string to the UI
  }
  
}

#Team stats builder.
# Required arguments (and order):
# 1) position (either skater or goalie)
# 2) game type (either R/regular season/2 or P/playoffs/3)
# 3) team (using three letter code e.g. CAR for Carolina Hurricanes)
# 4) season to display (defaults to 20242025)
teamStatsBuilder <- function(base, args) {
  #Sanity check, make sure all args are in the list
  if (length(args) != 4) {
    stop("Incorrect arguments, there need to be 4: position, game type, team, and season.")
  }
  
  #Set up base URL
  base_endpoint <- paste(base, "club-stats", sep = "/")
  
  #Apply arguments
  pos <- args[1]
  type <- args[2]
  team <- args[3]
  season <- args[4]

  
  #Concatenate string and result
  result <- paste(base_endpoint, team, season, type, sep = "/")
  return(result)
}

#Team stats data clean/manipulate


#Leader stats builder.
# Required arguments (and order):
# 1) position (either skater or goalie)
# 2) game type (either reg. season/2 or playoffs/3)
# 3) limit (how many people to display)
# 4) season to display (defaults to 20242025)
