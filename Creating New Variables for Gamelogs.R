library(rvest)
library(rlist)
library(dplyr)
library(stringr)
library(tidyverse)
library(installr)
library(fs)
library(XML)
library(xml2)
library(togglr)

### Fixing Gamelogs 2.0
# Adrian Peterson
## 2007 Game 5(MIN)/6(CHI)
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/min/2007")
x1 <- read.csv("Game 5.csv")
x1$Detail[89] <- "CHI pass incomplete short left intended for CHI"
x1$Possession[89] <- "CHI"
x1$Detail[92] <- "CHI for 9 yards"
x1$Possession[92] <- "CHI"
x1$Detail[93] <- "CHI pass complete short right to CHI for 12 yards (tackle by MIN)"
x1$Possession[93] <- "CHI"
x1$Detail[96] <- "CHI pass complete short right to CHI for -2 yards (tackle by MIN)"
x1$Possession[96] <- "CHI"
x1$Detail[97] <- "CHI pass complete short right to CHI for 5 yards (tackle by MIN). Penalty on CHI: Offensive Holding, 10 yards"
x1$Possession[97] <- "CHI"
write.csv(x1, "Game 5.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/chi/2007")
write.csv(x1, "Game 6.csv")
##

## 2007 Game 14 MIN/CHI
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/min/2007")
x2 <- read.csv("Game 14.csv")
x2$Detail[112] <- "MIN up the middle for 1 yard, touchdown"
x2$Possession[112] <- "MIN"
x2$Detail[146] <- "MIN up the middle for 8 yards, touchdown"
x2$Possession[146] <- "MIN"
x2$Possession[147] <- "MIN"
write.csv(x2, "Game 14.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/chi/2007")
write.csv(x2, "Game 14.csv")
##

## 2008 Game 7 MIN/CHI is totally fine

## 2008 Game 12 MIN/CHI 
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/min/2008")
x3 <- read.csv("Game 12.csv")
x3$Detail[161] <- "MIN left guard for 1 yard, touchdown. Penalty on CHI: Defensive 12 On-field (Declined)"
x3$Possession[161] <- "MIN"
write.csv(x3, "Game 12.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/chi/2008")
write.csv(x3, "Game 12.csv")
##

## 2009 Game 11 MIN/CHI
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/min/2009")
x4 <- read.csv("Game 11.csv")
x4$Detail[10] <- "MIN left end for -8 yards (tackle by CHI). MIN fumbles (forced by CHI), recovered by CHI at CHI-35 (tackle by MIN)"
x4$Possession[10] <- "MIN"
x4$Detail[160] <- "MIN left tackle for 5 yards, touchdown"
x4$Possession[160] <- "MIN"
x4$Possession[161] <- "MIN"
write.csv(x4, "Game 11.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/chi/2009")
write.csv(x4, "Game 11.csv")
##

## 2009 Game 15 MIN/CHI is totally fine

# Alex Smith
## 2007 Game 15 SFO/TAM is totally fine

## 2009 Game 14 SFO/PHI is totally fine

## 2011 Game 7 SFO/CLE
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/sfo/2011")
x5 <- read.csv("Game 7.csv")
x5$Detail[8] <- "SFO pass incomplete short middle intended for SFO (defended by CLE)"
x5$Possession[8] <- "SFO"
x5$Detail[9] <- "SFO pass incomplete short right intended for SFO (defended by CLE). Penalty on CLE: Defensive Pass Interference, 6 yards (no play)"
x5$Possession[9] <- "SFO"
x5$Detail[62] <- "SFO pass incomplete deep right intended for SFO"
x5$Possession[62] <- "SFO"
x5$Detail[67] <- "SFO pass incomplete deep left intended for SFO"
x5$Possession[67] <- "SFO"
x5$Detail[75] <- "SFO pass complete short right to SFO for 2 yards, touchdown"
x5$Possession[75] <- "SFO"
x5$Detail[91] <- "SFO pass incomplete short right intended for SFO"
x5$Possession[91] <- "SFO"
x5$Detail[93] <- "SFO pass incomplete short right intended for SFO (defended by CLE)"
x5$Possession[93] <- "SFO"
x5$Detail[103] <- "SFO pass complete deep left to SFO for 41 yards"
x5$Possession[103] <- "SFO"
x5$Detail[105] <- "SFO sacked by CLE for -8 yards. SFO fumbles (forced by CLE), recovered by SFO at CLE-45"
x5$Possession[105] <- "SFO"
x5$Detail[115] <- "SFO pass incomplete short left intended for SFO"
x5$Possession[115] <- "SFO"
x5$Detail[117] <- "SFO pass incomplete short right intended for SFO"
x5$Possession[117] <- "SFO"
x5$Detail[130] <- "SFO pass incomplete deep left intended for SFO"
x5$Possession[130] <- "SFO"
x5$Detail[141] <- "SFO pass incomplete short left intended for SFO"
x5$Possession[141] <- "SFO"
x5$Detail[166] <- "SFO left tackle for 1 yard"
x5$Possession[166] <- "SFO"
write.csv(x5, "Game 7.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/cle/2011")
write.csv(x5, "Game 7.csv")
##

# Josh Allen
## 2021 Game 8 JAX/BUF
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/buf/2021")
x6 <- read.csv("Game 8.csv")
x6$Detail[20] <- "BUF pass complete short middle to BUF for 7 yards (tackle by JAX)"
x6$Possession[20] <- "BUF"
x6$Detail[21] <- "BUF scrambles right end for 15 yards (tackle by JAX)"
x6$Possession[21] <- "BUF"
x6$Detail[27] <- "BUF up the middle for -3 yards (tackle by JAX)"
x6$Possession[27] <- "BUF"
x6$Detail[50] <- "BUF scrambles left end for 22 yards (tackle by JAX)"
x6$Possession[50] <- "BUF"
x6$Detail[94] <- "BUF up the middle for 4 yards (tackle by JAX)"
x6$Possession[94] <- "BUF"
x6$Detail[121] <- "BUF pass short right intended for BUF is intercepted by JAX at BUF-41 and returned for 11 yards (tackle by BUF)"
x6$Possession[121] <- "BUF"
x6$Detail[153] <- "BUF right end for no gain. BUF fumbles (forced by JAX), recovered by JAX at JAX-37 (tackle by BUF)"
x6$Possession[153] <- "BUF"
x6$Detail[163] <- "BUF pass complete short left to BUF for 14 yards (tackle by JAX and JAX)"
x6$Possession[163] <- "BUF"
write.csv(x6, "Game 8.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/jax/2021")
write.csv(x6, "Game 8.csv")
##

## 2023 Game 5 JAX/BUF
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/buf/2023")
x7 <- read.csv("Game 5.csv")
x7$Detail[12] <- "BUF pass incomplete short right intended for BUF"
x7$Possession[12] <- "BUF"
x7$Detail[41] <- "BUF pass incomplete short right"
x7$Possession[41] <- "BUF"
x7$Detail[48] <- "BUF pass incomplete short right intended for BUF"
x7$Possession[48] <- "BUF"
x7$Detail[62] <- "BUF pass complete short right to BUF for 15 yards"
x7$Possession[62] <- "BUF"
x7$Detail[72] <- "BUF pass incomplete short right intended for BUF (defended by JAX)"
x7$Possession[72] <- "BUF"
x7$Detail[80] <- "BUF pass incomplete short right"
x7$Possession[80] <- "BUF"
x7$Detail[81] <- "BUF pass complete short left to BUF for 10 yards"
x7$Possession[81] <- "BUF"
x7$Detail[82] <- "BUF pass complete short right to BUF for 15 yards, touchdown"
x7$Possession[82] <- "BUF"
x7$Detail[100] <- "BUF pass incomplete short left intended for BUF"
x7$Possession[100] <- "BUF"
x7$Detail[101] <- "BUF pass incomplete short right"
x7$Possession[101] <- "BUF"
x7$Detail[115] <- "BUF pass incomplete short right intended for BUF"
x7$Possession[115] <- "BUF"
x7$Detail[116] <- "BUF pass incomplete deep left intended for BUF. Penalty on BUF: Offensive Holding, 10 yards (declined)"
x7$Possession[116] <- "BUF"
x7$Detail[139] <- "BUF pass incomplete short left intended for BUF. Penalty on BUF: Offensive Holding, 10 yards (accepted) (no play)"
x7$Possession[139] <- "BUF"
x7$Detail[140] <- "BUF pass incomplete deep right intended for BUF (defended by JAX)"
x7$Possession[140] <- "BUF"
x7$Detail[166] <- "BUF sacked by JAX for no gain. Penalty on JAX: Roughing the Passer, 9 yards (accepted) (no play)"
x7$Possession[166] <- "BUF"
x7$Detail[167] <- "BUF pass incomplete short right intended for BUF"
x7$Possession[167] <- "BUF"
x7$Detail[168] <- "BUF pass complete short left to BUF for 9 yards, touchdown. Penalty on BUF: Offensive Pass Interference, 10 yards (accepted) (no play)"
x7$Possession[168] <- "BUF"
x7$Detail[169] <- "BUF pass complete deep left to BUF for 19 yards, touchdown. Penalty on JAX: Defensive Pass Interference, 15 yards (declined)"
x7$Possession[169] <- "BUF"
x7$Detail[170] <- "Two Point Attempt: BUF pass incomplete intended for to BUF for no gain"
x7$Possession[170] <- "BUF"
x7$Detail[182] <- "BUF pass complete deep right to BUF for 29 yards"
x7$Possession[182] <- "BUF"
x7$Detail[183] <- "BUF pass incomplete short right"
x7$Possession[183] <- "BUF"
x7$Detail[184] <- "BUF scrambles right guard for 3 yards, touchdown"
x7$Possession[184] <- "BUF"
x7$Detail[193] <- "BUF pass complete deep right to BUF for 12 yards. BUF fumbles (forced by JAX), recovered by JAX at BUF-18"
x7$Possession[193] <- "BUF"
write.csv(x7, "Game 5.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/jax/2023")
write.csv(x7, "Game 5.csv")
##

# Chris Johnson
## 2010 Game 1 OAK/TEN
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/oti/2010")
x8 <- read.csv("Game 1.csv")
x8$Detail[81] <- "TEN left guard for 76 yards, touchdown"
x8$Possession[81] <- "TEN"
x8$Detail[125] <- "TEN left guard for 4 yards, touchdown"
x8$Possession[125] <- "TEN"
write.csv(x8, "Game 1.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/rai/2010")
write.csv(x8, "Game 1.csv")
##
###


# Gamelogs 2.0 Update: Setting up for LeftJoin with GameInfo --------------------

teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      testdf <- read.csv(filename)
      testdf$FolderTm <- team
      testdf <- testdf %>% relocate(Year, .before = gamenum)
      testdf <- testdf %>% relocate(FolderTm, .before = Year)
      write.csv(testdf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}

# :Gamelogs 2.1: Number of Timeouts ---------------------------------------

teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      testdf <- read.csv(filename)
      testdf <- testdf %>% relocate(FolderTm, .after = Possession)
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Arizona Cardinals", "ARI", x)
                     }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Atlanta Falcons", "ATL", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Baltimore Ravens", "BAL", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Buffalo Bills", "BUF", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Carolina Panthers", "CAR", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Chicago Bears", "CHI", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Cincinnati Bengals", "CIN", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Cleveland Browns", "CLE", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Dallas Cowboys", "DAL", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Detroit Lions", "DET", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Green Bay Packers", "GNB", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Houston Texans", "HOU", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Indianapolis Colts", "IND", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Jacksonville Jaguars", "JAX", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Kansas City Chiefs", "KAN", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("St. Louis Rams", "STL", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Los Angeles Rams", "LAR", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("San Diego Chargers", "SDG", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Los Angeles Chargers", "LAC", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Miami Dolphins", "MIA", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Minnesota Vikings", "MIN", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("New England Patriots", "NWE", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("New Orleans Saints", "NOR", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("New York Giants", "NYG", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("New York Jets", "NYJ", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Oakland Raiders", "OAK", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Las Vegas Raiders", "LVR", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Philadelphia Eagles", "PHI", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Pittsburgh Steelers", "PIT", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Seattle Seahawks", "SEA", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("San Francisco 49ers", "SFO", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Tampa Bay Buccaneers", "TAM", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Tennessee Titans", "TEN", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Washington Redskins", "WAS", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Washington Football Team", "WAS", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("Washington Commanders", "WAS", x)
      }))
      testdf <- data.frame(lapply(testdf, function(x) {
        gsub("\\#", "", x)
      }))
      numzlist <- c(1:100)
      for(numz in numzlist){
        badcol <- paste0("X.",numz)
        if(badcol %in% colnames(testdf)){
          testdf <- testdf %>% select(-all_of(badcol))
        }
      }
      if("X" %in% colnames(testdf)){
        testdf <- testdf %>% select(-X)
      }
      TM1 <- colnames(testdf[8])
      TM2 <- colnames(testdf[9])
      testdf$Tm1Timeouts <- ""
      testdf$Tm1Timeouts[3] <- 3
      testdf$Tm2Timeouts <- ""
      testdf$Tm2Timeouts[3] <- 3
      halfkey <- which(testdf$Quarter=="3rd Quarter")
      testdf$Tm1Timeouts[halfkey+1] <- 3
      testdf$Tm2Timeouts[halfkey+1] <- 3
      testdf <- testdf %>% relocate((Tm1Timeouts:Tm2Timeouts), .before = Detail)
      nlist <- c(3:length(testdf$Possession))
      for(n in nlist){
        mywords <- unlist(str_split(testdf$Detail[n]," "))
        if(isTRUE(mywords[1]=="Timeout")){
          tmword <- mywords[4]
          numkey <- as.numeric(mywords[2])
          if(isTRUE(numkey<4)){
            if(isTRUE(tmword==TM1)){
              testdf$Tm1Timeouts[n] = 3 - numkey
            }
            if(isTRUE(tmword==TM2)){
              testdf$Tm2Timeouts[n] = 3 - numkey
            }
          }
        }
      }
      numslist <- c(3: length(testdf$Possession))
      keylistTm1 <- which(!(testdf$Tm1Timeouts==""))
      keylistTm2 <- which(!(testdf$Tm2Timeouts==""))
      for(nums in numslist){
        chaltest <- grepl("challenged", testdf$Detail[nums])
        failtest <- grepl("upheld", testdf$Detail[nums])
        if(isTRUE(chaltest)){
          if(isTRUE(failtest)){
            mywords <- unlist(str_split(testdf$Detail[nums]," "))
            if(isTRUE(mywords[1]==TM1)){
              keylistTm1X <- keylistTm1[keylistTm1<nums]
              mymax <- max(keylistTm1X)
              testdf$Tm1Timeouts[nums] = as.numeric(testdf$Tm1Timeouts[mymax])-1
            }
            if(isTRUE(mywords[1]==TM2)){
              keylistTm2X <- keylistTm2[keylistTm2<nums]
              mymax <- max(keylistTm2X)
              testdf$Tm2Timeouts[nums] = as.numeric(testdf$Tm2Timeouts[mymax])-1
            }
          }
        }
      }
      klist <- c(3: length(testdf$Possession))
      for(k in klist){
        if(isTRUE(testdf$Tm1Timeouts[k]=="")){
          testdf$Tm1Timeouts[k] = testdf$Tm1Timeouts[k-1]
        }
        if(isTRUE(testdf$Tm2Timeouts[k]=="")){
          testdf$Tm2Timeouts[k] = testdf$Tm2Timeouts[k-1]
        }
      }
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.1/",team,"/",year)
      setwd(newwd)
      write.csv(testdf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}



teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.1/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      testdf <- read.csv(filename)
      testdf[c("LocTeam", "LocYdLine")] <- str_split_fixed(testdf$Location, " ", 2)
      testdf$YdstoEZ <- ""
      numlist <- c(3:length(testdf$Possession))
      testdf$LocYdLine <- as.numeric(testdf$LocYdLine)
      for(num in numlist){
        if(isTRUE(testdf$LocTeam[num]=="")){
          testdf$LocTeam[num] = testdf$LocTeam[num-1]
          testdf$LocYdLine[num] = testdf$LocYdLine[num-1]
        }
        if(isFALSE(is.na(testdf$LocYdLine[num]))){
          if(isFALSE(testdf$ToGo[num]=="")){
            if(isTRUE(testdf$Possession[num]==testdf$LocTeam[num])){
              testdf$YdstoEZ[num] = 100 - testdf$LocYdLine[num]
            }
            if(isFALSE(testdf$Possession[num]==testdf$LocTeam[num])){
              testdf$YdstoEZ[num] = testdf$LocYdLine[num]
            }
          }
        }
      }
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.5/",team,"/",year)
      setwd(newwd)
      write.csv(testdf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}



# NOTE: The Chargers/Giants 2017 Week 5 Gamelog is completely messed up
# NOTE: The Packers/Bears 2017 Game 9/10 Gamelog is completely messed up
team <- "crd"
year <- 2006
game <- 2
testlist <- list()
checklist <- list()
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.5/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      testdf <- read.csv(filename)
      testdf$EndDrive <- ""
      ### Getting Rid of X Columnns (Cause they suck)
      numzlist <- c(1:100)
      for(numz in numzlist){
        badcol <- paste0("X.",numz)
        if(badcol %in% colnames(testdf)){
          testdf <- testdf %>% select(-all_of(badcol))
        }
      }
      if("X" %in% colnames(testdf)){
        testdf <- testdf %>% select(-X)
      }
      testdf <- testdf %>% relocate(EndDrive, .before = Detail)
      testdf <- testdf %>% relocate((Tm1Timeouts:Tm2Timeouts), .after = YdstoEZ)
      testdf$dc <- ""
      testdf$dc1 <- ""
      testdf$Team1Name <- colnames(testdf)[8]
      testdf$Team2Name <- colnames(testdf)[9]
      testdf <- testdf %>% relocate((dc:dc1), .before = Detail)
      colnames(testdf)[8] <- "Tm1"
      colnames(testdf)[9] <- "Tm2"
      colnames(testdf)[11] <- "Tm1Score"
      colnames(testdf)[12] <- "Tm2Score"
      testdf$NewDowns <- ""
      testdf <- testdf %>% relocate(NewDowns, .before = Detail)
      testdf$DownsResult <- ""
      testdf <- testdf %>% relocate(DownsResult, .before = Detail)
      testdf$YdstoEZ[3:(length(testdf$LocYdLine)-1)][is.na(testdf$YdstoEZ[3:(length(testdf$LocYdLine)-1)])] <- 0
      testdf$YdsGained <- ""
      testdf <- testdf %>% relocate(YdsGained, .before = Detail)
      testdf <- testdf %>% filter(!grepl("Quarter", Detail))
      testdf <- testdf %>% filter(!grepl("Quarter", Tm1))
      # testdf <- testdf %>% filter(!grepl("challenged", Detail))
      testdf <- testdf %>% filter(!grepl("Timeout", Detail))
      testdf <- testdf %>% filter(Detail!="Overtime")
      testdf <- testdf %>% filter(Detail!="Detail")
      # testdf <- testdf %>% filter(Detail!="--")
      numslist <- c(1:length(testdf$Possession))
      testdf$FirstWord <- ""
      for(nums in numslist){
        teststr <- unlist(str_split(testdf$Detail[nums]," "))
        testword <- teststr[1]
        testdf$FirstWord[nums] <- testword
        if(year>=2007&year<=2008){
          testdf$Detail[nums] <- str_replace(testdf$Detail[nums],"Ray Ventrone","NWE")
        }
        if(year>=2009&year<=2012){
          testdf$Detail[nums] <- str_replace(testdf$Detail[nums],"Ray Ventrone","CLE")
        }
        if(year>=2013&year<=2014){
          testdf$Detail[nums] <- str_replace(testdf$Detail[nums],"Ray Ventrone","SFO")
        }
      }
      # testdf <- testdf %>% filter(FirstWord!="Penalty")
      testdf <- testdf %>% filter(FirstWord!="Two")
      testdf <- testdf %>% filter(FirstWord!="Replay")
      testdf <- testdf %>% filter(FirstWord!="Challenged")
      testdf <- testdf %>% filter(!((Quarter=="")&
                                      grepl("challenge", testdf$Detail)))
      if(isTRUE(testdf[8][2,]=="")){
        scoreindicatornum8 <- 3
        scoreindicatornum9 <- 3
      }
      if(isFALSE(testdf[8][2,]=="")){
        scoreindicatornum8 <- 2
        scoreindicatornum9 <- 2
      }
      numlist <- c(3:length(testdf$Possession))
      for(num in numlist){
        if(isTRUE(testdf[8][num,]=="")){
          testdf[8][num,] <- testdf[8][(num-1),]
          testdf[9][num,] <- testdf[9][(num-1),]
        }
        if(isTRUE(testdf$Possession[num]!=testdf$Possession[(num+1)])){
          teststr10 <- grepl("kicks", testdf$Detail[(num+1)])
          if(isFALSE(teststr10)){
            testdf$EndDrive[num] <- "Yes"
          }
          if(isTRUE(teststr10)){
            testdf$EndDrive[num+1] <- "Yes"
          }
          ### TEST 3/2/24 6:19 PM
          if(isTRUE(testdf$Quarter[num]==2)){
            if(isTRUE(testdf$Quarter[num+1]==3)){
              testdf$EndDrive[num] <- "Yes"
            }
          }
          if(isTRUE(testdf$Quarter[num]==4)){
            if(isTRUE(testdf$Quarter[num+2]=="OT")){
              testdf$EndDrive[num] <- "Yes"
              testdf <- testdf[-(num+2),]
              testdf <- testdf[-(num+1),]
            }
          }
          ### TEST 3/2/24 6:19 PM
        }
        if(isFALSE(testdf$Possession[num]!=testdf$Possession[(num+1)])){
          # TEST 3/4/24 8:32 PM
          teststr40 <- grepl("kicks", testdf$Detail[num])
          if(isTRUE(teststr40)){
            teststr41 <- grepl("kicks", testdf$Detail[(num+1)]) &
              grepl("touchdown", testdf$Detail[(num+1)])
            if(isTRUE(teststr41)){
              if(isTRUE(testdf$Possession[num+1]!=testdf$Possession[num+2])){
                testdf$EndDrive[num] <- "Yes"
              }
            }
          }
          # TEST 3/4/24 8:32 PM
          if(isTRUE(testdf$Detail[num]=="--")){
            testdf$EndDrive[num] <- "Yes"
          }
          teststr <- grepl("onside", testdf$Detail[num])
          if(isTRUE(teststr)){
            testdf$EndDrive[num] <- "Yes"
          }
          teststr1 <- grepl("fumbles", testdf$Possession[num]) & 
            grepl("recovered", testdf$Possession[num])
          if(isTRUE(teststr1)){
            testdf$EndDrive[num] <- "Yes"
          }
          if(isTRUE(testdf$Quarter[num]==2)){
            if(isTRUE(testdf$Quarter[num+1]==3)){
              testdf$EndDrive[num] <- "Yes"
            }
          }
          if(isTRUE(testdf$Quarter[num]==4)){
            if(isTRUE(testdf$Quarter[num+2]=="OT")){
              testdf$EndDrive[num] <- "Yes"
              testdf <- testdf[-(num+2),]
              testdf <- testdf[-(num+1),]
            }
          }
          #### TEST 3/2/24 5:39 PM
          teststr31 <- grepl("kicks off", testdf$Detail[num]) &
            grepl(paste0("recovered by ", testdf$Possession[num]), testdf$Detail[num])
          if(isTRUE(teststr31)){
            testdf$EndDrive[num] <- "Yes"
          }
          ### TEST 3/2/24 5:39 PM
          
          teststr2 <- grepl("kicks off", testdf$Detail[num+1]) & 
            grepl("fumbles", testdf$Detail[num+1])
          if(isTRUE(teststr2)){
            teststr2x <- grepl("touchdown", testdf$Detail[num+1])
            if(isTRUE(teststr2x)){
              testdf$EndDrive[(num)] <- "Yes"
            }
            if(isFALSE(teststr2x)){
              testdf$EndDrive[(num+1)] <- "Yes"
            }
          }
          teststr3 <- grepl("punts", testdf$Detail[num+1]) & 
            grepl("fumbles", testdf$Detail[num+1])
          if(isTRUE(teststr3)){
            teststr3x <- grepl("touchdown", testdf$Detail[num+1])
            if(isTRUE(teststr3x)){
              testdf$EndDrive[(num)] <- "Yes"
            }
            if(isFALSE(teststr3x)){
              testdf$EndDrive[(num+1)] <- "Yes"
            }
          }
        }
        if(isTRUE(testdf$EndDrive[num]=="Yes")){
          if(isTRUE(testdf[8][num,]!=testdf[8][scoreindicatornum8,])){
            testdf[11][num,] <- as.numeric(testdf[8][num,])-
              as.numeric(testdf[8][scoreindicatornum8,])
          } else{
            testdf[11][num,] <- 0
          }
          if(isTRUE(testdf[9][num,]!=testdf[9][scoreindicatornum9,])){
            testdf[12][num,] <- as.numeric(testdf[9][num,])-
              as.numeric(testdf[9][scoreindicatornum9,])
          } else{
            testdf[12][num,] <- 0
          }
          scoreindicatornum8 <- num
          scoreindicatornum9 <- num
        }
        if(isTRUE(num<=4)){
          if(isTRUE(testdf$EndDrive[num]=="Yes")){
            testdf[11][num,] <- testdf[8][num,]
            testdf[12][num,] <- testdf[9][num,]
          }
        }
        if(isTRUE(testdf[11][num,]>=1)){
          if(isTRUE(testdf[11][num,]<=2)){
            if(isTRUE(testdf[11][(num-1),]==6)){
              testdf[11][num,] <- as.numeric(testdf[11][num,]) + 6
              teststr <- grepl("kicks", testdf$Detail[(num)])
              if(isTRUE(teststr)){
                teststr12 <- grepl("touchdown", testdf$Detail[num])
                if(isFALSE(teststr12)){
                  
                testdf$EndDrive[(num-1)] <- ""
                testdf[11][(num-1),] <- ""
                testdf[12][(num-1),] <- ""
                }
              }
            }
            if(isTRUE(testdf[11][(num-2),]==6)){
              testdf[11][num,] <- as.numeric(testdf[11][num,]) + 6
              teststr <- grepl("kicks", testdf$Detail[(num)])
              if(isTRUE(teststr)){
                testdf[11][(num-2),] <- 0
              }
            }
          }
        }
        if(isTRUE(testdf[12][num,]>=1)){
          if(isTRUE(testdf[12][num,]<=2)){
            if(isTRUE(testdf[12][(num-1),]==6)){
              testdf[12][num,] <- as.numeric(testdf[12][num,]) + 6
              teststr <- grepl("kicks", testdf$Detail[(num)])
              if(isTRUE(teststr)){
                teststr12 <- grepl("touchdown", testdf$Detail[num])
                if(isFALSE(teststr12)){
                  
                testdf$EndDrive[(num-1)] <- ""
                testdf[11][(num-1),] <- ""
                testdf[12][(num-1),] <- ""
                }
              }
            }
            if(isTRUE(testdf[12][(num-2),]==6)){
              testdf[12][num,] <- as.numeric(testdf[12][num,]) + 6
              teststr <- grepl("kicks", testdf$Detail[(num)])
              if(isTRUE(teststr)){
                testdf[12][(num-2),] <- 0
              }
            }
          }
        }
        if(isTRUE(testdf$Down[num]==1)){
          testdf$NewDowns[num] <- "Yes"
        }
      }
      nlist <- c(1:length(testdf$Possession))
      for(n in nlist){
        if(isTRUE(testdf$Tm1Score[n]>0)){
          if(isTRUE(testdf$Tm2Score[n]>0)){
            teststrz <- grepl("kicks", testdf$Detail[(n-1)]) &
              grepl("touchdown", testdf$Detail[(n-1)])
            if(isTRUE(teststrz)){
              testdf$EndDrive[(n-1)]<-"Yes"
              if(isTRUE(testdf$Tm1[(n-2)]!=testdf$Tm1[(n-3)])){
                testdf$Tm1Score[(n-1)] <- as.numeric(testdf$Tm1[(n-1)]) -
                  as.numeric(testdf$Tm1[(n-3)])
                testdf$Tm2Score[(n-1)] <- 0
                testdf$Tm1Score[(n+1)] <- 0
                testdf$Tm2Score[(n+1)] <- as.numeric(testdf$Tm2[(n+1)]) -
                  as.numeric(testdf$Tm2[(n-2)])
                testdf$EndDrive[n] <- ""
                testdf$Tm1Score[n] <- ""
                testdf$Tm2Score[n] <- ""
              }
              if(isTRUE(testdf$Tm2[(n-2)]!=testdf$Tm2[(n-3)])){
                testdf$Tm2Score[(n-1)] <- as.numeric(testdf$Tm2[(n-1)]) -
                  as.numeric(testdf$Tm2[(n-3)])
                testdf$Tm1Score[(n-1)] <- 0
                testdf$Tm2Score[(n+1)] <- 0
                testdf$Tm1Score[(n+1)] <- as.numeric(testdf$Tm1[(n+1)]) -
                  as.numeric(testdf$Tm1[(n-2)])
                testdf$EndDrive[n] <- ""
                testdf$Tm1Score[n] <- ""
                testdf$Tm2Score[n] <- ""
              }
            }
          }
        }
      }
      mlist <- c(4: length(testdf$Possession))
      for(m in mlist){
        if(isTRUE(testdf$NewDowns[m]=="Yes")){
          if(isTRUE(testdf$Possession[m]!=testdf$Possession[(m-1)])){
            if(isTRUE(testdf$Tm1Score[(m-1)]==0)&
               isTRUE(testdf$Tm2Score[(m-1)]==0)){
              testdf$DownsResult[(m-1)] <- "No"
            }
            else{
              testdf$DownsResult[(m-1)] <- "Yes"
            }
          }
          else{
            testdf$DownsResult[(m-1)] <- "Yes"
          }
        }
        tdteststr <- grepl("touchdown", testdf$Detail[m])
        if(isTRUE(tdteststr)){
          testdf$DownsResult[m] <- "Yes"
        }
        fgteststr <- grepl("field goal", testdf$Detail[m])
        if(isTRUE(fgteststr)){
          testdf$DownsResult[m] <- "No"
        }
        opteststr <- grepl(paste0("Penalty on ", testdf$Possession[(m-1)]), testdf$Detail[(m-1)])
        decteststr1 <- grepl("Declined", testdf$Detail[(m-1)])
        decteststr2 <- grepl("declined", testdf$Detail[(m-1)])
        if(isTRUE(opteststr)){
          if(isFALSE(decteststr1)){
            if(isFALSE(decteststr2)){
              testdf$DownsResult[(m-1)] <- ""
            }
          }
        }
      }
      m2list <- c(4:(length(testdf$Possession)-1))
      for(m2 in m2list){
        if(isTRUE(testdf$FirstWord[(m2+1)]=="End")){
          if(isTRUE(testdf$DownsResult[m2]=="")){
            testdf$DownsResult[m2] <- "No"
          }
        }
      }
      klist <- c(3: (length(testdf$Possession)-1))
      for(k in klist){
        if(isTRUE(testdf$EndDrive[k]!="Yes")){
          if(isTRUE(testdf$YdstoEZ[k]!=0)){
            if(isTRUE(testdf$FirstWord[(k+1)]!="End")){
              safteststr <- grepl("safety", testdf$Detail[k])
              kickteststr <- grepl("kicks", testdf$Detail[k])
              puntteststr <- grepl("punts", testdf$Detail[k])
              fgteststr <- grepl("field goal", testdf$Detail[k])
              intteststr <- grepl("intercepted", testdf$Detail[k])
              if(isFALSE(kickteststr)){
                if(isFALSE(puntteststr)){
                  if(isFALSE(fgteststr)){
                    if(isFALSE(intteststr)){
                      if(isFALSE(safteststr)){
                        testdf$YdsGained[k] <- testdf$YdstoEZ[k]-testdf$YdstoEZ[(k+1)]
                      }
                      if(isTRUE(safteststr)){
                        teststr52 <- unlist(str_split(testdf$Detail[k],", "))
                        teststr52 <- gsub("\\.", "", teststr52)
                        teststr52 <- unlist(str_split(teststr52," "))
                        wordindex2 <- which(teststr52=="yards")
                        if(isTRUE(is.empty(wordindex2))){
                          wordindex2 <- which(teststr52=="yard")
                        }
                        xyards2 <- as.numeric(teststr52[(wordindex2[1]-1)])
                        testdf$YdsGained[k] <- xyards2
                      }
                    }
                  }
                }
              }
            }
          }
          if(isTRUE(testdf$Detail[k]=="--")){
            testdf$YdsGained[m] <- ""
          }
        }
        if(isFALSE(testdf$EndDrive[k]!="Yes")){
          if(isTRUE(testdf$Down[k]==4)){
            kickteststr <- grepl("kicks", testdf$Detail[k])
            puntteststr <- grepl("punts", testdf$Detail[k])
            fgteststr <- grepl("field goal", testdf$Detail[k])
            intteststr <- grepl("intercepted", testdf$Detail[k])
            if(isFALSE(kickteststr)){
              if(isFALSE(puntteststr)){
                if(isFALSE(fgteststr)){
                  if(isFALSE(intteststr)){
                    if(isTRUE(testdf$Possession[k]!=testdf$Possession[k+1])){
                      teststr50 <- unlist(str_split(testdf$Detail[k],", "))
                      teststr50 <- gsub("\\.", "", teststr50)
                      teststr50 <- unlist(str_split(teststr50," "))
                      wordindex <- which(teststr50== "yards")
                      if(isTRUE(is.empty(wordindex))){
                        wordindex <- which(teststr50=="yard")
                      }
                      xyards <- as.numeric(teststr50[(wordindex[1]-1)])
                      testdf$YdsGained[k] <- xyards
                    }
                  }
                }
              }
            }
          }
          if(isFALSE(testdf$FirstWord[(k+1)]!="End")){
            teststr51 <- unlist(str_split(testdf$Detail[k],", "))
            teststr51 <- gsub("\\.", "", teststr51)
            teststr51 <- unlist(str_split(teststr51," "))
            wordindex1 <- which(teststr51== "yards")
            if(isTRUE(is.empty(wordindex1))){
              wordindex1 <- which(teststr51=="yard")
            }
            xyards1 <- as.numeric(teststr51[(wordindex1[1]-1)])
            testdf$YdsGained[k] <- xyards1
          }
        }
      }
      checkdf <- testdf %>% filter(EndDrive=="Yes")
      checklist <- list.append(checklist, checkdf)
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/",team,"/",year)
      setwd(newwd)
      write.csv(testdf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
      testlist <- list.append(testlist, testdf)
    }
  }
}


basefile <- checklist[[1]]
numlist <- c(2:length(checklist))
for(num in numlist){
  basefile <- rbind(basefile, checklist[[num]])
  print(num)
}

setwd("C:/Users/Aaron Friedlander/Desktop")
write.csv(basefile, "Checkdf.csv")


# numlist <- c(1:length(testlist))
# faultylist <- list()
# for(num in numlist){
#   testdf <- testlist[[num]]
#   # colnames(testdf)[8] <- "Tm1"
#   # colnames(testdf)[9] <- "Tm2"
#   # colnames(testdf)[11] <- "Tm1Score"
#   # colnames(testdf)[12] <- "Tm2Score"
#   testdf <- testdf %>% filter(Tm1!="3rd Quarter")
#   tm1score <- sum(na.omit(as.numeric(testdf$Tm1Score)))
#   tm2score <- sum(na.omit(as.numeric(testdf$Tm2Score)))
#   testvar <- tm1score + tm2score
#   len <- length(testdf$Possession)
#   sumtest <- as.numeric(testdf$Tm1[len-1]) + as.numeric(testdf$Tm2[len-1])
#   if(isTRUE(testvar!=sumtest)){
#     faultylist <- list.append(faultylist, testdf)
#   }
# }


### Fixing Gamelogs 3.0
# DET/WAS 2019 11
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/det/2019")
x1 <- read.csv("Game 11.csv")
x1$Tm2[26:34] <- 3
x1$Tm2Score[26] <- 3
write.csv(x1, "Game 11.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/was/2019")
write.csv(x1, "Game 11.csv")

# MIN/PHI 2016 6
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/min/2016")
x2 <- read.csv("Game 6.csv")
x2$EndDrive[57] <- "Yes"
x2$Tm1Score[57] <- 3
x2$Tm2Score[57] <- 0
write.csv(x2, "Game 6.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/phi/2016")
write.csv(x2, "Game 6.csv")

# DEN/KAN 2016 11
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/den/2016")
x3 <- read.csv("Game 11.csv")
x3$EndDrive[60] <- "Yes"
x3$Tm1Score[60] <- 2
x3$Tm2Score[60] <- 0
x3$EndDrive[61] <- ""
x3$Tm1Score[61] <- ""
x3$Tm2Score[61] <- ""
x3$EndDrive[62] <- "Yes"
x3$Tm1Score[62] <- 7
x3$Tm2Score[62] <- 0
write.csv(x3, "Game 11.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/kan/2016")
write.csv(x3, "Game 11.csv")

# DET/GB 2011 16
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/det/2011")
x4 <- read.csv("Game 16.csv")
x4$EndDrive[12] <- "Yes"
x4$Tm1Score[12] <- 7
x4$Tm2Score[12] <- 0
x4$EndDrive[12] <- "Yes"
x4$Tm1Score[12] <- 2
x4$Tm2Score[12] <- 0
write.csv(x4, "Game 16.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/gnb/2011")
write.csv(x4, "Game 16.csv")

# NYJ/BUF 2016 16
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/buf/2016")
x5 <- read.csv("Game 16.csv")
x5$Tm2Score[144] <- 3
x5$Tm2Score[146] <- 7
write.csv(x5, "Game 16.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/nyj/2016")
write.csv(x5, "Game 16.csv")

# HOU/OAK 2009 4
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/htx/2009")
x6 <- read.csv("Game 4.csv")
x6$EndDrive[101] <- "Yes"
x6$Tm1Score[101] <- 0
x6$Tm2Score[101] <- 2
x6$EndDrive[102] <- ""
x6$Tm1Score[102] <- ""
x6$Tm2Score[102] <- ""
x6$EndDrive[103] <- "Yes"
x6$Tm1Score[103] <- 0
x6$Tm2Score[103] <- 7
write.csv(x6, "Game 4.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/rai/2009")
write.csv(x6, "Game 4.csv")

# CLE/OAK 2018 4
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/cle/2018")
x7 <- read.csv("Game 4.csv")
x7$Tm2[137] <- 31
x7$Tm2Score[137] <- 7
x7$Tm2Score[141] <- 0
write.csv(x7, "Game 4.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/rai/2018")
write.csv(x7, "Game 4.csv")

# CLE/HOU 2018 12
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/cle/2018")
x8 <- read.csv("Game 12.csv")
x8$Tm2[53] <- 17
x8$Tm2[54] <- 17
x8$EndDrive[53] <- ""
x8$Tm1Score[53] <- ""
x8$Tm2Score[53] <- ""
x8$EndDrive[54] <- "Yes"
x8$Tm1Score[54] <- 0
x8$Tm2Score[54] <- 7
x8$Tm2Score[57] <- 0
write.csv(x8, "Game 12.csv")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/htx/2018")
write.csv(x8, "Game 12.csv")


      #### NEXT STEP (ADDING IT TO ALL OBSERVATIONS)
mylist <- list()
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 3.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      testdf <- read.csv(filename)
      testdf[is.na(testdf)]<-""
      testdf <- testdf %>% select(-X)
      testdf1 <- testdf
      nonemptytest11 <- testdf1$Tm1Score[testdf1$Tm1Score!=""]
      nonemptytest12 <- testdf1$Tm2Score[testdf1$Tm2Score!=""]
      nonemptytest14 <- testdf1$DownsResult[testdf1$DownsResult!=""]
      numlist <- c(3:length(testdf1$Possession))
      varkey <- 1
      for(num in numlist){
        if(isTRUE(testdf1$Tm1Score[num]=="")){
          testdf1$Tm1Score[num] = nonemptytest11[varkey]
          testdf1$Tm2Score[num] = nonemptytest12[varkey]
        }
        else{
          varkey <- varkey + 1
        }
        
      }
      varkey2 <- 1
      nlist <- c(3:length(testdf1$Possession))
      for(n in nlist){
        if(isTRUE(testdf1$DownsResult[n]=="")){
          testdf1$DownsResult[n] <- nonemptytest14[varkey2]
        }
        else{
          varkey2 <- varkey2 + 1
        }
      }
      mylist <- list.append(mylist, testdf1)
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 4.0/",team,"/",year)
      setwd(newwd)
      write.csv(testdf1, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}
      

      
## Adding Interception and LostFumble
team <- "crd"
year <- 2006
game <- 1
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 4.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      newdf <- read.csv(filename)
      # newdf <- newdf %>% filter(!(grepl("coin toss", Detail)))
      # newdf <- newdf %>% filter(!(grepl("kicks", Detail)))
      # newdf <- newdf %>% filter(!(grepl("kneels", Detail)))
      # newdf <- newdf %>% filter(!(grepl("punts", Detail)))
      # newdf <- newdf %>% filter(!(grepl("field goal", Detail)))
      # newdf <- newdf %>% filter(!(grepl("End of Regulation", Detail)))
      
      newdf$LostFumble <- ""
      newdf$Intercpetion <- ""
      newdf <- newdf %>% relocate(LostFumble, .before = Detail)
      newdf <- newdf %>% relocate(Intercpetion, .before = Detail)
      numlist <- c(1:length(newdf$Possession))
      for(num in numlist){
        fumtestr <- grepl("fumbles", newdf$Detail[num])
        if(newdf$Possession[num]==newdf$Team1Name[num]){
          postestr <- grepl(paste0("recovered by ",newdf$Team2Name[num]), newdf$Detail[num])
        }
        if(newdf$Possession[num]==newdf$Team2Name[num]){
          postestr <- grepl(paste0("recovered by ",newdf$Team1Name[num]), newdf$Detail[num])
        }
        if(isTRUE(fumtestr)){
          if(isTRUE(postestr)){
            newdf$LostFumble[num] <- "Yes"
          }
        }
        inttestr <- grepl("intercepted", newdf$Detail[num])
        if(isTRUE(inttestr)){
          newdf$Intercpetion[num] <- "Yes"
        }
      }
      
      numzlist <- c(1:length(newdf$Possession))
      for(numz in numzlist){
        if(isTRUE(newdf$Intercpetion[numz]=="Yes")){
          newdf$YdsGained[numz] <- 0
        }
        if(isTRUE(newdf$LostFumble[numz]=="Yes")){
          newdf$YdsGained[numz] <- 0
        }
      }
      
      numslist <- c(1:length(newdf$Possession))
      for(nums in numslist){
        if(isTRUE(is.na(newdf$YdsGained[nums]))){
          teststr51 <- unlist(str_split(newdf$Detail[nums],", "))
          teststr51 <- gsub("\\.", "", teststr51)
          teststr51 <- unlist(str_split(teststr51," "))
          wordindex1 <- which(teststr51== "yards")
          if(isTRUE(is.empty(wordindex1))){
            wordindex1 <- which(teststr51=="yard")
          }
          xyards1 <- as.numeric(teststr51[(wordindex1[1]-1)])
          newdf$YdsGained[nums] <- xyards1
          if(isTRUE(is.empty(wordindex1))){
            if(isTRUE(newdf$FirstWord[(nums+1)]=="End")){
              newdf$YdsGained[nums] <- 0
            }
          }
        }
      }
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 5.0/",team,"/",year)
      setwd(newwd)
      filename <- paste0("Game ",game,".csv")
      write.csv(newdf,filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}

### ADDING VARIABLES: Tm1ScoreonPlay and Tm2ScoreonPlay
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 5.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      mydf <- read.csv(filename)
      mydf$Tm1ScoreonPlay <- ""
      mydf$Tm2ScoreonPlay <- ""
      mydf <- mydf %>% relocate((Tm1ScoreonPlay:Tm2ScoreonPlay), .after = Tm2Score)
      nlist <- c(3:length(mydf$Possession))
      for(n in nlist){
        if(isTRUE(mydf$Possession[n]==mydf$Team1Name[n])){
          nptest <- grepl("no play", mydf$Detail[n])
          if(isFALSE(nptest)){
            if(isTRUE(mydf$Possession[n]==mydf$Possession[n+1] | 
                      isTRUE(grepl("End of Overtime", mydf$Location[n+1])))){
              tdtest <- grepl("ouchdown", mydf$Detail[n])
              if(isTRUE(tdtest)){
                if(isTRUE(mydf$Tm1Score[n]>=6)){
                  mydf$Tm1ScoreonPlay[n] = mydf$Tm1Score[n]
                }
              }
            }
          }
        }
        if(isTRUE(mydf$Possession[n]==mydf$Team2Name[n])){
          nptest <- grepl("no play", mydf$Detail[n])
          if(isFALSE(nptest)){
            if(isTRUE(mydf$Possession[n]==mydf$Possession[n+1] | 
                      isTRUE(grepl("End of Overtime", mydf$Location[n+1])))){
              tdtest <- grepl("ouchdown", mydf$Detail[n])
              if(isTRUE(tdtest)){
                if(isTRUE(mydf$Tm2Score[n]>=6)){
                  mydf$Tm2ScoreonPlay[n] = mydf$Tm2Score[n]
                }
              }
            }
          }
        }
        if(isTRUE(mydf$Possession[n]==mydf$Team1Name[n])){
          nptest <- grepl("no play", mydf$Detail[n])
          if(isFALSE(nptest)){
            fgtest <- grepl("ield goal", mydf$Detail[n])
            if(isTRUE(fgtest)){
              if(isTRUE(mydf$Tm1Score[n]==3)){
                mydf$Tm1ScoreonPlay[n] = mydf$Tm1Score[n]
              }
            }
          }
        }
        if(isTRUE(mydf$Possession[n]==mydf$Team2Name[n])){
          nptest <- grepl("no play", mydf$Detail[n])
          if(isFALSE(nptest)){
            fgtest <- grepl("ield goal", mydf$Detail[n])
            if(isTRUE(fgtest)){
              if(isTRUE(mydf$Tm2Score[n]==3)){
                mydf$Tm2ScoreonPlay[n] = mydf$Tm2Score[n]
              }
            }
          }
        }
      }
      mydf$Tm1ScoreonPlay[mydf$Tm1ScoreonPlay==""] <- 0
      mydf$Tm2ScoreonPlay[mydf$Tm2ScoreonPlay==""] <- 0
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 5.5/",team,"/",year)
      setwd(newwd)
      filename <- paste0("Game ",game,".csv")
      write.csv(mydf,filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}


### Gamelogs 6.0: Team1Result and Team2Result
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 5.5/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      mydf <- read.csv(filename)
      mydf$Tm1ScoreResult <- ""
      mydf$Tm2ScoreResult <- ""
      mydf <- mydf %>% relocate((Tm1ScoreResult:Tm2ScoreResult),.after = Tm2Score)
      numlist <- c(1:length(mydf$gamenum))
      for(num in numlist){
        if(isTRUE(mydf$Tm1Score[num]>=3)){
          mydf$Tm1ScoreResult[num] <- "Yes"
        }
        else{
          mydf$Tm1ScoreResult[num] <- "No"
        }
        if(isTRUE(mydf$Tm2Score[num]>=3)){
          mydf$Tm2ScoreResult[num] <- "Yes"
        }
        else{
          mydf$Tm2ScoreResult[num] <- "No"
        }
      }
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 6.0/",team,"/",year)
      setwd(newwd)
      filename <- paste0("Game ",game,".csv")
      write.csv(mydf,filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}


### Gamelogs 7.0: Adding First Down on Play
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 6.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      mydf <- read.csv(filename)
      mydf$FirstDownonPlay <- ""
      mydf <- mydf %>% relocate(FirstDownonPlay, .after = DownsResult)
      numzlist <- c(1:100)
      for(numz in numzlist){
        badcol <- paste0("X.",numz)
        if(badcol %in% colnames(mydf)){
          mydf <- mydf %>% select(-all_of(badcol))
        }
      }
      if("X" %in% colnames(mydf)){
        mydf <- mydf %>% select(-X)
      }
      numslist <- c(3:length(mydf$Possession))
      for(nums in numslist){
        teststr <- unlist(str_split(mydf$Detail[nums]," "))
        teststr <- gsub("\\:", "", teststr)
        teststr <- gsub("\\.", "", teststr)
        teststr <- gsub("\\,", "", teststr)
        if(isTRUE(teststr[1]=="Penalty")){
          mydf$Possession[nums] <- teststr[3]
        }
      }
      numlist <- c(3:length(mydf$Possession))
      for(num in numlist){
        test1 <- grepl("kick", mydf$Detail[num])
        test2 <- grepl("punt", mydf$Detail[num])
        test3 <- grepl("enalty", mydf$Detail[num])
        if(isFALSE(test3)){
          if(isFALSE(test1)){
            if(isFALSE(test2)){
              if(isTRUE(mydf$Down[num+1]==1)){
                if(isTRUE(mydf$Possession[num+1]==mydf$Possession[num])){
                  if(isFALSE(mydf$Quarter[num]==2&mydf$Quarter[num+1]==3)){
                    mydf$FirstDownonPlay[num] <- "Yes"
                  }
                  else{
                    mydf$FirstDownonPlay[num] <- "No"
                  }
                }
                else{
                  mydf$FirstDownonPlay[num] <- "No"
                }
              }
              else{
                mydf$FirstDownonPlay[num] <- "No"
              }
            }
            else{
              mydf$FirstDownonPlay[num] <- "No"
            }
          }
          else{
            mydf$FirstDownonPlay[num] <- "No"
          }
        }
        else{
          mydf$FirstDownonPlay[num] <- "No"
        }
        if(isTRUE(test3&mydf$YdsGained[num]>0)){
          dectest <- grepl("declined", mydf$Detail[num])
          if(isFALSE(dectest)){
            mydf$FirstDownonPlay[num] <- "Yes"
          }
        }
        
        tdtest <- grepl("ouchdown", mydf$Detail[num])
        if(isTRUE(tdtest)){
          if(isTRUE(mydf$Possession[num]==mydf$Possession[num+1])){
            mydf$FirstDownonPlay[num] <- "Yes"
          }
        }
      }
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 7.0/",team,"/",year)
      setwd(newwd)
      write.csv(mydf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}

### Gamelogs 8.0: Adding Play Types and EPDiff
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 7.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      mydf <- read.csv(filename)
      numzlist <- c(1:100)
      for(numz in numzlist){
        badcol <- paste0("X.",numz)
        if(badcol %in% colnames(mydf)){
          mydf <- mydf %>% select(-all_of(badcol))
        }
      }
      if("X" %in% colnames(mydf)){
        mydf <- mydf %>% select(-X)
      }
      mydf$BasePlayType <- ""
      mydf$DetailPlayType <- ""
      mydf <- mydf %>% relocate((BasePlayType:DetailPlayType), .before = Detail)
      mydf <- mydf %>% 
        mutate(EPDiff = as.numeric(EPA) - as.numeric(EPB))
      mydf <- mydf %>% relocate(EPDiff, .after = EPA)
      numslist <- c(3: length(mydf$Possession))
      for(nums in numslist){
        test1 <- grepl("kick", mydf$Detail[nums])
        test4 <- grepl("field goal", mydf$Detail[nums])
        test2 <- grepl("punt", mydf$Detail[nums])
        test3 <- grepl("no play", mydf$Detail[nums])
        if(isFALSE(test3)){
          if(isFALSE(test1)){
            if(isFALSE(test2)){
              if(isFALSE(test4)){
                passtest <- grepl("pass", mydf$Detail[nums])
                sktest <- grepl("sack", mydf$Detail[nums])
                if(isTRUE(passtest|sktest)){
                  mydf$BasePlayType[nums] <- "Pass"
                }
                else{
                  mydf$BasePlayType[nums] <- "Run"
                }
              }
              else{
                mydf$BasePlayType[nums] <- "ST"
              }
            }
            else{
              mydf$BasePlayType[nums] <- "ST"
            }
          }
          else{
            mydf$BasePlayType[nums] <- "ST"
          }
        }
        if(isTRUE(test3)){
          mydf$BasePlayType[nums] <- "NO PLAY"
        }
      }
      numzlist <- c(3:length(mydf$Possession))
      for(numz in numzlist){
        if(isTRUE(mydf$BasePlayType[numz]=="Pass")){
          shortlentest <- grepl("short", mydf$Detail[numz])
          deeplentest <- grepl("deep", mydf$Detail[numz])
          lefttest <- grepl("left", mydf$Detail[numz])
          middletest <- grepl("middle", mydf$Detail[numz])
          righttest <- grepl("right", mydf$Detail[numz])
          sktest <- grepl("sack", mydf$Detail[numz])
          spiketest <- grepl("spike", mydf$Detail[numz])
          if(isTRUE(sktest)){
            mydf$DetailPlayType[numz] <- "Sack"
          }
          if(isTRUE(shortlentest&lefttest)){
            mydf$DetailPlayType[numz] <- "Pass Short Left"
          }
          if(isTRUE(shortlentest&middletest)){
            mydf$DetailPlayType[numz] <- "Pass Short Middle"
          }
          if(isTRUE(shortlentest&righttest)){
            mydf$DetailPlayType[numz] <- "Pass Short Right"
          }
          if(isTRUE(deeplentest&lefttest)){
            mydf$DetailPlayType[numz] <- "Pass Deep Left"
          }
          if(isTRUE(deeplentest&middletest)){
            mydf$DetailPlayType[numz] <- "Pass Deep Middle"
          }
          if(isTRUE(deeplentest&righttest)){
            mydf$DetailPlayType[numz] <- "Pass Deep Right"
          }
          if(isTRUE(spiketest)){
            mydf$DetailPlayType[numz] <- "Spike"
          }
          if(isTRUE(mydf$DetailPlayType[numz]=="")){
            mydf$DetailPlayType[numz] <- "Unidentified Pass"
          }
        }
        if(isTRUE(mydf$BasePlayType[numz]=="Run")){
          lefttest <- grepl("left", mydf$Detail[numz])
          middletest <- grepl("middle", mydf$Detail[numz])
          righttest <- grepl("right", mydf$Detail[numz])
          if(isTRUE(lefttest)){
            mydf$DetailPlayType[numz] <- "Run Left"
          }
          if(isTRUE(middletest)){
            mydf$DetailPlayType[numz] <- "Run Middle"
          }
          if(isTRUE(righttest)){
            mydf$DetailPlayType[numz] <- "Run Right"
          }
          if(isTRUE(mydf$DetailPlayType[numz]=="")){
            mydf$DetailPlayType[numz] <- "Unidentified Run"
          }
        }
        if(isTRUE(mydf$BasePlayType[numz]=="ST")){
          mydf$DetailPlayType[numz] <- "ST"
        }
        if(isTRUE(mydf$BasePlayType[numz]=="NO PLAY")){
          mydf$DetailPlayType[numz] <- "NO PLAY"
        }
      }
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 8.0/",team,"/",year)
      setwd(newwd)
      write.csv(mydf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}


### Gamelogs 9.0, Adding GameResult
mylist <- list()
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 8.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      mydf <- read.csv(filename)
      numzlist <- c(1:100)
      for(numz in numzlist){
        badcol <- paste0("X.",numz)
        if(badcol %in% colnames(mydf)){
          mydf <- mydf %>% select(-all_of(badcol))
        }
      }
      if("X" %in% colnames(mydf)){
        mydf <- mydf %>% select(-X)
      }
      mydf$Tm1PointDiff <- ""
      mydf$Tm1WinLoss <- ""
      mydf$Tm2PointDiff <- ""
      mydf$Tm2WinLoss <- ""
      mydf <- mydf %>% relocate((Tm1PointDiff:Tm2WinLoss), .after = Tm2)
      indicator <- length(mydf$Possession)-1
      ptdiff <- as.numeric(mydf$Tm1[indicator]) - 
        as.numeric(mydf$Tm2[indicator])
      mydf$Tm1PointDiff <- ptdiff
      mydf$Tm2PointDiff <- -1 * ptdiff
      if(isTRUE(ptdiff>0)){
        mydf$Tm1WinLoss <- "W"
        mydf$Tm2WinLoss <- "L"
      }
      if(isTRUE(ptdiff==0)){
        mydf$Tm1WinLoss <- "T"
        mydf$Tm2WinLoss <- "T"
      }
      if(isTRUE(ptdiff<0)){
        mydf$Tm1WinLoss <- "L"
        mydf$Tm2WinLoss <- "W"
      }
      mylist <- list.append(mylist, mydf)
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 9.0/",team,"/",year)
      setwd(newwd)
      write.csv(mydf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}


basefile <- mylist[[1]]
zlist <- c(2: length(mylist))
for(z in zlist){
  basefile <- rbind(basefile, mylist[[z]])
  print(z)
}

setwd("C:/Users/Aaron Friedlander/Desktop/Thesis Interpretings csvs")
write.csv(basefile, "DatawithoutWinProb.csv")
unique(basefile$Year)


## Fixing Gamelogs 9.0
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 9.0/chi/2007")
mydf <- read.csv("Game 6.csv")
mydf$gamenum <- 6
write.csv(mydf, "Game 6.csv")



## Gamelogs 10.0: Win Probability

### NOTE: Win Probability is calculated for the FAVORITE at all points in the game
## The win probability is also calculated AFTER a play happens

# NOTE: ram/nyg 2020 4 does NOT have EP
# NOTE: min/pit 2017 2 does NOT have EP

team <- "pit"
year <- 2017
gamelist <- c(3:16)
yearlist <- c(2018:2020,2022:2023)

teamlist <- list(#"crd","atl","rav","buf","car",
                 #"chi","cin","cle","dal","den","det","gnb","htx",
                 #"clt","jax","kan","ram", 
                 #"sdg","mia","min", 
                 #"nwe","nor","nyg", 
                 #"nyj","rai","phi","pit",
                 "sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2020, 2022:2023)
  for(year in yearlist){
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 9.0/",team,"/",year)
      setwd(mywd)
      filename <- paste0("Game ",game,".csv")
      mydf <- read.csv(filename)
      numzlist <- c(1:100)
      for(numz in numzlist){
        badcol <- paste0("X.",numz)
        if(badcol %in% colnames(mydf)){
          mydf <- mydf %>% select(-all_of(badcol))
        }
      }
      if("X" %in% colnames(mydf)){
        mydf <- mydf %>% select(-X)
      }
      mydf[c("Minutes","Seconds")] <- str_split_fixed(mydf$Time, ":",2)
      mydf$Minutes <- as.numeric(mydf$Minutes)
      mydf$Seconds <- as.numeric(mydf$Seconds)
      mydf <- mydf %>% 
        mutate(TotSeconds = 60*Minutes + Seconds)
      mydf$Quarter[mydf$Quarter=="OT"] <- 5
      mydf$Quarter <- as.numeric(mydf$Quarter)
      mydf <- mydf %>% 
        mutate(TimeLeft = TotSeconds + (900*(4-Quarter)))
      mydf <- mydf %>% relocate(TimeLeft, .after = Time)
      mydf$TimeLeft <- mydf$TimeLeft/60
      # I added the log part of the equation because the sqrt decreased too fast
      mydf <- mydf %>% 
        mutate(STDev =  ((2*(13.45/sqrt((60/TimeLeft)))) + (.7*(log(TimeLeft)+10)))/3)
      mydf <- mydf %>% relocate(STDev, .after = TimeLeft)
      mydf <- mydf %>% filter(!(grepl("coin toss", Detail)))
      mydf <- mydf %>% filter(!(grepl("End of Regulation", Detail)))
      mydf$Intercpetion[is.na(mydf$Intercpetion)] <- ""
      mydf$LostFumble[is.na(mydf$LostFumble)] <- ""
      mydf <- na.omit(mydf)
      setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Game Info")
      gidf <- read.csv("AllGameInfo.csv") %>% select(-X)
      mydf <- mydf %>% relocate(FolderTm, .before = Year)
      newdf <- suppressMessages(left_join(mydf, gidf))
      newdf <- newdf %>% relocate((FavTeam:PtSpread), .after = Time)
      newdf$Mean <- ""
      newdf <- newdf %>% relocate(Mean, .before = STDev)
      newdf$Mean <- as.numeric(newdf$Mean)
      favtm <- newdf$FavTeam[1]
      newdf$Tm1 <- as.numeric(newdf$Tm1)
      newdf$Tm2 <- as.numeric(newdf$Tm2)
      newdf <- newdf %>% 
        mutate(FavTeam = if_else(FavTeam=="Pick",Team1Name,FavTeam))
      if(isTRUE(newdf$FavTeam[1]==newdf$Team1Name[1])){
        newdf <- newdf %>% 
          mutate(FavPtDiff = Tm1 - Tm2)
      }
      if(isTRUE(newdf$FavTeam[1]==newdf$Team2Name[1])){
        newdf <- newdf %>% 
          mutate(FavPtDiff = Tm2 - Tm1)
      }
      newdf <- newdf %>% relocate(FavPtDiff, .after = Tm2)
      newdf <- newdf %>% relocate(Possession, .before = Location)
      newdf <- newdf %>% relocate(Detail, .after = Location)
      newdf$WinProb <- ""
      newdf$WinProb <- as.numeric(newdf$WinProb)
      newdf$PreGameProb <- 1 - pnorm(0.5, mean = newdf$PtSpread, sd = 13.45)
      newdf <- newdf %>% relocate(WinProb, .after = STDev)
      newdf <- newdf %>% relocate(PreGameProb, .before = WinProb)
      mlist <- c(2:length(newdf$Possession))
      newdf$EPB <- as.numeric(newdf$EPB)
      newdf <- newdf %>% relocate((EPB:EPDiff), .before = Mean)
      if(isTRUE(newdf$Possession[1]==favtm)){
        newdf$Mean[1] <- newdf$PtSpread[1] + newdf$EPDiff[1] + newdf$FavPtDiff[1] + newdf$EPB[1]
      }
      if(isFALSE(newdf$Possession[1]==favtm)){
        newdf$Mean[1] <- newdf$PtSpread[1] - newdf$EPDiff[1] + newdf$FavPtDiff[1] - newdf$EPB[1]
      }
      for(m in mlist){
        if(isTRUE(newdf$Tm1ScoreonPlay[m]==0 & newdf$Tm2ScoreonPlay[m]==0)){
          if(isTRUE(newdf$Possession[m]==favtm)){
            newdf$Mean[m] = (newdf$Mean[m-1] + newdf$EPDiff[m])
          }
          if(isFALSE(newdf$Possession[m]==favtm)){
            newdf$Mean[m] = (newdf$Mean[m-1] - newdf$EPDiff[m]) 
          }
        }
        # The division by 60/sqrt(TimeLeft) is to scale down the importance of point spread
        if(isFALSE(newdf$Tm1ScoreonPlay[m]==0 & newdf$Tm2ScoreonPlay[m]==0)){
          newdf$Mean[m] = (newdf$PtSpread[1]/(sqrt(60/(newdf$TimeLeft[m]))) + newdf$FavPtDiff[m+1])
        }
      }
      wlist <- c(1:length(newdf$Possession))
      for(w in wlist){
        newdf$WinProb[w] <- 1 - pnorm(0.5, mean = newdf$Mean[w], sd = newdf$STDev[w])
      }
      newdf$Mean <- round(newdf$Mean, digits = 3)
      newdf$STDev <- round(newdf$STDev, digits = 3)
      newdf$WinProb <- round(newdf$WinProb, digits = 3)
      newdf$PreGameProb <- round(newdf$PreGameProb, digits = 3)
      newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 10.0/",team,"/",year)
      setwd(newwd)
      write.csv(newdf, filename)
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}


### Compiling them all into one
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram", "sdg","mia","min", "nwe","nor","nyg", "nyj","rai","phi","pit","sea","sfo","tam","oti","was")
mylist <- list()
for(team in teamlist){
  yearlist <- c(2006:2020, 2022:2023)
  tmfilelist <- list()
  for(year in yearlist){
    newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 10.0/",team,"/",year)
    setwd(newwd)
    filelist <- list.files()
    gamelist <- c(1:length(filelist))
    newfilelist <- list()
    for(game in gamelist){
      mydf <- read.csv(filelist[game])
      numzlist <- c(1:100)
      for(numz in numzlist){
        badcol <- paste0("X.",numz)
        if(badcol %in% colnames(mydf)){
          mydf <- mydf %>% select(-all_of(badcol))
        }
      }
      if("X" %in% colnames(mydf)){
        mydf <- mydf %>% select(-X)
      }
      newfilelist <- list.append(newfilelist, mydf)
    }
    gmbasefile <- newfilelist[[1]]
    xlist <- c(2:length(newfilelist))
    for(x in xlist){
      gmbasefile <- rbind(gmbasefile, newfilelist[[x]])
    }
    tmfilelist <- list.append(tmfilelist, gmbasefile)
  }
  tmbasefile <- tmfilelist[[1]]
  ylist <- c(2:length(tmfilelist))
  for(y in ylist){
    tmbasefile <- rbind(tmbasefile, tmfilelist[[y]])
  }
  mylist <- list.append(mylist, tmbasefile)
}

basefile <- mylist[[1]]
zlist <- c(2: length(mylist))
for(z in zlist){
  basefile <- rbind(basefile, mylist[[z]])
  print(z)
}

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files")
saveRDS(basefile, "DatawithWinProb.rds")



