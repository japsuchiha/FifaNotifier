install.packages("jsonlite")
install.packages("devtools")
install.packages("later")
library(later)
library(jsonlite)
library(devtools)
devtools::install_version("notifier")
liveMatchScore <- function(){
  apiResults <- fromJSON("https://api.fifa.com/api/v1/live/football/")$Results
  matchID <- which(apiResults$IdSeason == 254645 & apiResults$MatchStatus == 3)
  
  if(length(matchID) != 1){
    liveScore <- NULL
  }
  else{
    liveScore <- unlist(apiResults[matchID,c("AggregateHomeTeamScore", "AggregateAwayTeamScore", 
                                             "HomeTeamPenaltyScore", "AwayTeamPenaltyScore")])
  homeTeam <- apiResults$HomeTeam$TeamName[[matchID]]$Description
  awayTeam <- apiResults$AwayTeam$TeamName[[matchID]]$Description
  names(liveScore) <- rep(c(homeTeam,awayTeam),2)
  }
  liveScore
  }
scoreAsString <- function(matchScore,penalties =FALSE){
  out <- paste(names(matchScore)[1], " - ",names(matchScore)[2],":",matchScore[1], " - ", matchScore[2])
  if(penalties && matchScore[1] == matchScore[2]){
    out <- paste0(out, " (pen.", matchScore[3], " - ", matchScore[4], ")")
    out
  }
}

checkScore <- function(prevScore = NULL){
  newScore <- liveMatchScore()
  if(is.null(newScore) && !is.null(prevScore)){
    sendNotification(title = "Match ended", message = scoreAsString(prevScore,TRUE))
  }
  else if(is.null(prevScore) && !is.null(newScore)){
    sendNotification(title = "Match Started", message = scoreAsString(newScore))
  }
  else if(!is.null(prevScore) && !is.null(newScore) && !identical(newScore,prevScore)){
    sendNotification(title="GOAL!", message= scoreAsString(newScore))
  }
  return (newScore)
}

download.file("https://api.fifa.com/api/v1/picture/tournaments-sq-4/254645_w", 
              "logo.png")
sendNotification <- function(title="", message){
  notifier::notify(title = title, msg = message, image = normalizePath("logo.png"))
}

getUpdates <- function(){
  prevScore <- checkScore(prevScore)
  later::later(getUpdates,delay = 60)
}

prevScore <- NULL
getUpdates()