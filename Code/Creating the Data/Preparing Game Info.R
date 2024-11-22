library(rvest)
library(rlist)
library(dplyr)
library(stringr)
library(tidyverse)
library(installr)
library(fs)
library(XML)
library(xml2)

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Game Info")
gidf <- readRDS("AllGameInfo.rds")
gidf[c('FavTeam', 'PtSpread')] <- str_split_fixed(gidf$Vegas.Line, '-', 2)

gidf <- gidf |> 
  mutate(PtSpread = if_else(FavTeam=="Pick", "0", PtSpread)) |> 
  mutate(FavTeam = if_else(FavTeam=="Pick", Tm, FavTeam))

nlist <- c(1:length(gidf$Tm))
for(n in nlist){
  if(isTRUE(gidf$FavTeam[n]=="atl")){
    gidf$FavTeam[n] = "Atlanta Falcons "
  }
  if(isTRUE(gidf$FavTeam[n]=="buf")){
    gidf$FavTeam[n] = "Buffalo Bills "
  }
  if(isTRUE(gidf$FavTeam[n]=="chi")){
    gidf$FavTeam[n] = "Chicago Bears "
  }
  if(isTRUE(gidf$FavTeam[n]=="cin")){
    gidf$FavTeam[n] = "Cincinnati Bengals "
  }
  if(isTRUE(gidf$FavTeam[n]=="cle")){
    gidf$FavTeam[n] = "Cleveland Browns "
  }
  if(isTRUE(gidf$FavTeam[n]=="clt")){
    gidf$FavTeam[n] = "Indianapolis Colts "
  }
  if(isTRUE(gidf$FavTeam[n]=="crd")){
    gidf$FavTeam[n] = "Arizona Cardinals "
  }
  if(isTRUE(gidf$FavTeam[n]=="dal")){
    gidf$FavTeam[n] = "Dallas Cowboys "
  }
  if(isTRUE(gidf$FavTeam[n]=="den")){
    gidf$FavTeam[n] = "Denver Broncos "
  }
  if(isTRUE(gidf$FavTeam[n]=="det")){
    gidf$FavTeam[n] = "Detroit Lions "
  }
  if(isTRUE(gidf$FavTeam[n]=="gnb")){
    gidf$FavTeam[n] = "Green Bay Packers "
  }
  if(isTRUE(gidf$FavTeam[n]=="htx")){
    gidf$FavTeam[n] = "Houston Texans "
  }
  if(isTRUE(gidf$FavTeam[n]=="jax")){
    gidf$FavTeam[n] = "Jacksonville Jaguars "
  }
  if(isTRUE(gidf$FavTeam[n]=="kan")){
    gidf$FavTeam[n] = "Kansas City Chiefs "
  }
  if(isTRUE(gidf$FavTeam[n]=="mia")){
    gidf$FavTeam[n] = "Miami Dolphins "
  }
  if(isTRUE(gidf$FavTeam[n]=="min")){
    gidf$FavTeam[n] = "Minnesota Vikings "
  }
  if(isTRUE(gidf$FavTeam[n]=="nwe")){
    gidf$FavTeam[n] = "New England Patriots "
  }
  if(isTRUE(gidf$FavTeam[n]=="nyg")){
    gidf$FavTeam[n] = "New York Giants "
  }
  if(isTRUE(gidf$FavTeam[n]=="nyj")){
    gidf$FavTeam[n] = "New York Jets "
  }
  if(isTRUE(gidf$FavTeam[n]=="oti")){
    gidf$FavTeam[n] = "Tennessee Titans "
  }
  if(isTRUE(gidf$FavTeam[n]=="phi")){
    gidf$FavTeam[n] = "Philadelphia Eagles "
  }
  if(isTRUE(gidf$Year[n]<2021)){
    if(isTRUE(gidf$FavTeam[n]=="rai")){
      gidf$FavTeam[n] = "Oakland Raiders "
    }
  }
  else{
    if(isTRUE(gidf$FavTeam[n]=="rai")){
      gidf$FavTeam[n] = "Las Vegas Raiders "
    }
  }
  if(isTRUE(gidf$FavTeam[n]=="sdg")){
    gidf$FavTeam[n] = "San Diego Chargers "
  }
  if(isTRUE(gidf$FavTeam[n]=="sea")){
    gidf$FavTeam[n] = "Seattle Seahawks "
  }
  if(isTRUE(gidf$FavTeam[n]=="sfo")){
    gidf$FavTeam[n] = "San Francisco 49ers "
  }
  if(isTRUE(gidf$FavTeam[n]=="tam")){
    gidf$FavTeam[n] = "Tampa Bay Buccaneers "
  }
  if(isTRUE(gidf$Year[n]<2021)){
    if(isTRUE(gidf$FavTeam[n]=="was")){
      gidf$FavTeam[n] = "Washingon Redskins "
    }
  }
  else{
    if(isTRUE(gidf$FavTeam[n]=="was")){
      gidf$FavTeam[n] = "Washington Football Team "
    }
  }
}

colnames(gidf)[colnames(gidf)=="Tm"] <- "FolderTm"
gidf$FavTeam <- trimws(gidf$FavTeam)
gidf <- gidf %>% relocate(Over.Under, .after = PtSpread)
gidf$PtSpread <- as.numeric(gidf$PtSpread)
gidf[c('OvUnd', 'OvUndResult')] <- str_split_fixed(gidf$Over.Under, ' ', 2)
gidf$OvUnd <- as.numeric(gidf$OvUnd)
gidf$OvUndResult <- lapply(gidf$OvUndResult, function(x) {
  gsub("\\(", "", x)
})
gidf$OvUndResult <- lapply(gidf$OvUndResult, function(x) {
  gsub("\\)", "", x)
})
colnames(gidf)[colnames(gidf)=="game_num"] <- "gamenum"
gidf$OvUndResult <- as.character(gidf$OvUndResult)

gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Arizona Cardinals", "ARI", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Atlanta Falcons", "ATL", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Baltimore Ravens", "BAL", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Buffalo Bills", "BUF", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Carolina Panthers", "CAR", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Chicago Bears", "CHI", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Cincinnati Bengals", "CIN", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Cleveland Browns", "CLE", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Dallas Cowboys", "DAL", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Denver Broncos", "DEN", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Detroit Lions", "DET", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Green Bay Packers", "GNB", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Houston Texans", "HOU", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Indianapolis Colts", "IND", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Jacksonville Jaguars", "JAX", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Kansas City Chiefs", "KAN", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("St. Louis Rams", "STL", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Los Angeles Rams", "LAR", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("San Diego Chargers", "SDG", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Los Angeles Chargers", "LAC", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Miami Dolphins", "MIA", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Minnesota Vikings", "MIN", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("New England Patriots", "NWE", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("New Orleans Saints", "NOR", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("New York Giants", "NYG", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("New York Jets", "NYJ", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Oakland Raiders", "OAK", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Las Vegas Raiders", "LVR", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Philadelphia Eagles", "PHI", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Pittsburgh Steelers", "PIT", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Seattle Seahawks", "SEA", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("San Francisco 49ers", "SFO", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Tampa Bay Buccaneers", "TAM", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Tennessee Titans", "TEN", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Washington Redskins", "WAS", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Washingon Redskins", "WAS", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Washington Football Team", "WAS", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("Washington Commanders", "WAS", x)
}))
gidf <- data.frame(lapply(gidf, function(x) {
  gsub("\\#", "", x)
}))

saveRDS(gidf, "AllGameInfo.rds")