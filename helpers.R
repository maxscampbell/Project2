library(httr)
library(jsonlite)
library(tidyverse)

#---------- USEFUL DATA ---------- 
#Valid abbreviations for API
team_abbr <- c("ANA", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", 
               "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NJD", 
               "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "SEA", "SJS", 
               "STL", "TBL", "TOR", "UTA", "VAN", "VGK", "WPG", "WSH", 
               "ARI")

#---------- HELPER FUNCTIONS ---------- 

#Master function to accept input from Shiny app
# Returns a cleaned up request from the API
fetchInput <- function(endpoint = "team", args) {
  if (length(args) != 4) {
    stop("Missing some inputs! Expected 4 arguments.")
  }
  
  if (class(args) != "list") {
    stop("Non-endpoint arguments are not stored in a list!")
  }
  
  result <- getQuery(urlBuilder(endpoint, args))
  
  if (endpoint == "team") {
    result <- teamStatsClean(result, pos = args[1])
  } else if (endpoint == "skaters" || endpoint == "goalies") {
    result <- leaderStatsClean(result)
  } else {
    stop("Could not resolve data.")
  }
  
  return(as_tibble(result))
}

#Base function to get queries
getQuery <- function(url) {
  returned_raw <- httr::GET(url)
  returned <- fromJSON(rawToChar(returned_raw$content))
  
  return(returned)
}

#Endpoint/params
# 2) Team Stats
#    - Specify skaters and goalies
#    - Specify regular season or playoffs
# 3) Leaders
#    - Specify goalie or skater
#    - specify desired stat (goals, assists, points for skater, 
#        shutouts, wins for goalie)
#    - Specify limit (top 1-25)
#    - Specify season

#Master function to build query URLs
urlBuilder <- function(endpoint, args) {
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
  if (args[2] == TRUE) { 
    type <- "3" 
  } else { 
    type <- "2" 
  }
  team <- args[3]
  season <- args[4]

  
  #Concatenate string and result
  result <- paste(base_endpoint, team, season, type, sep = "/")
  return(result)
}

#Team stats data clean/manipulate
teamStatsClean <- function(data, pos) {
  #Two different paths to take, one with skaters one with goalies
  tryCatch({
  if (pos == "goalies") {
    result <- data$goalies
    
    result <- result |>
      select(playerId, firstName, lastName, gamesPlayed, gamesStarted, 
             wins, losses, overtimeLosses, goalsAgainstAverage, 
             savePercentage, shotsAgainst, saves, goalsAgainst, shutouts)
    
    result$firstName <- result$firstName$default
    result$lastName <- result$lastName$default
    
    return(result)
  } else if (pos == "skaters") {
    result <- data$skaters
    
    result <- result |>
      select(playerId, firstName, lastName, positionCode, gamesPlayed, goals, 
             assists, points, penaltyMinutes, shots, 
             shootingPctg, avgTimeOnIcePerGame, avgShiftsPerGame, faceoffWinPctg) |>
      mutate(impactPlayer = ifelse(positionCode == "D", 
                                   ifelse(avgTimeOnIcePerGame > 1200,
                                          ifelse(gamesPlayed > 10,
                                                 "Yes",
                                                 "No"),
                                          "No"),
                                   ifelse(avgTimeOnIcePerGame > 900,
                                          ifelse(gamesPlayed > 10,
                                                 "Yes",
                                                 "No"),
                                          "No")
                                   ),
             ) |>
      mutate(positionCode = as.factor(positionCode),
             impactPlayer = as.factor(impactPlayer))
    
    result$firstName <- result$firstName$default
    result$lastName <- result$lastName$default
    
    return(result)
  } else {
    stop("Please specify the proper position. Valid arguments: skaters, goalies.")
  }
  }, error = function(msg) {
    warning("Please specify a team that qualifed for the playoffs in your chosen season!")
  })
}

#Leader stats builder.
# Required arguments (and order):
# 1) position (either skater or goalie)
# 2) stat type (for skaters: goals, assists, etc. goalies: sv%, gaa, wins, etc.)
# 3) limit (how many people to display)
# 4) season to display (defaults to 20242025)
leaderStatsBuilder <- function(base, args) {
  #Sanity check, make sure all args are in the list
  if (length(args) != 4) {
    stop("Incorrect arguments, there need to be 4: position, game type, display limit, and season.")
  }
  
  #Set up base URL
  base_endpoint <- paste(base, "club-stats", sep = "/")
    
  
  #Apply arguments
  pos <- args[1]
  type <- args[2]
  limit <- args[3]
  season <- args[4]
  
  base_endpoint <- base
  
  if (pos == "skaters") {
    base_endpoint <- paste(base, "skater-stats-leaders", sep = "/")
  } else if (pos == "goalies") {
    base_endpoint <- paste(base, "goalie-stats-leaders", sep = "/")
  }
  
  url_1 <- paste(base_endpoint, season, "2", sep = "/")
  url_2 <- paste(url_1, "?categories=", type, "&limit=", limit, sep = "")
  
  return(url_2)
    
}

#Leader stats cleanup function
leaderStatsClean <- function(data) {
  result <- data[[1]]
  
  result$firstName <- result$firstName$default
  result$lastName <- result$lastName$default
  result$teamName <- result$teamName$default
  
  result <- result |>
    mutate(fullName = paste(substr(firstName, 1, 1), ". ", lastName, sep = ""))
  
  return(result)
}


