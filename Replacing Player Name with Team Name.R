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


# Creates a list of the keys for every NFL team used by Pro Football Reference
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det",
                 "gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor",
                 "nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project")

# The dataframe of Player Name, Team, Year, and Week
weektogndf <- read.csv("GameResults2006+.csv") %>% select(Week:Tm)
weektogndf <- weektogndf %>% select(-(Day:Date)) %>% select(-OT)
colnames(weektogndf)[colnames(weektogndf)=="game_num"] <- "gamenum"
for(team in teamlist){
  # Creates a list of all the years 2006-2023
  yearlist <- c(2006:2023)
  for(year in yearlist){
    if(year>=2021){
      # Starting in 2021, the regular season has 17 games instead of 16
      gamelist <- c(1:17)
    }
    if(year<2021){
      # Before 2021, the regular season only had 16 games
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      # Sets the working directory to the proper team and year folder
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs/",team,"/",year)
      setwd(mywd)
      # Reads the csv of the game specified in the for loop
      gameid <- paste0("Game ",game,".csv")
      dat <- read.csv(gameid) %>% select(-X)
      dat$Tm <- toupper(team)
      # Switches Pro Football Reference keys to my preferred abbreviations
      dat$Tm[dat$Tm=="CRD"]<-"ARI"
      dat$Tm[dat$Tm=="RAV"]<-"BAL"
      dat$Tm[dat$Tm=="HTX"]<-"HOU"
      dat$Tm[dat$Tm=="CLT"]<-"IND"
      dat$Tm[dat$Tm=="STL"]<-"LAR"
      dat$Tm[dat$Tm=="SDG"]<-"LAC"
      dat$Tm[dat$Tm=="OAK"]<-"LVR"
      dat$Tm <- gsub("RAM","LAR",dat$Tm)
      dat$Tm <- gsub("RAI","LVR",dat$Tm)
      dat$Tm <- gsub("OTI","TEN",dat$Tm)
      numlist <- c(1:length(dat$gamenum))
      for(num in numlist){
        # The St. Louis Rams became the Los Angeles Rams in 2016
        if(isTRUE(dat$Tm[num]=="LAR")){
          if(isTRUE(dat$Year[num]<=2015)){
            dat$Tm[num]<-"STL"
          }
        }
        # The San Diego Chargers became the Los Angeles Chargers in 2017
        if(isTRUE(dat$Tm[num]=="LAC")){
          if(isTRUE(dat$Year[num]<=2016)){
            dat$Tm[num]<-"SDG"
          }
        }
        # The Oakland Raiders became the Las Vegas Raiders in 2020
        if(isTRUE(dat$Tm[num]=="LVR")){
          if(isTRUE(dat$Year[num]<=2019)){
            dat$Tm[num]<-"OAK"
          }
        }
      }
      dat <- suppressMessages(left_join(dat, weektogndf))
      dat <- dat %>% select(-Tm)
      words2 <- unlist(str_split(dat$Detail[[2]], " "))
      x <- which(words2 == "receive")
      test <- words2[x-2]
      
      cols <- colnames(dat)
      pos1 <- cols[8]
      pos2 <- cols[9]
      
      datlen <- length(dat$gamenum)
      numslist <- c(1:datlen)
      wd1 <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Players by Team and Year/",pos1)
      setwd(wd1)
      checkdf1tag <- paste0(year,".csv")
      checkdf1 <- read.csv(checkdf1tag)
      wd2 <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Players by Team and Year/",pos2)
      setwd(wd2)
      checkdf2tag <- paste0(year,".csv")
      checkdf2 <- read.csv(checkdf2tag)
      for(j in numslist){
        # I had to do this because of Justin Tuck
        teststr <- dat[j,]$Detail
        jtteststr <- grepl("Justin Tucker", teststr)
        if(isTRUE(jtteststr)){
          teststr <- gsub("Justin Tucker", "BAL", teststr)
        }
        # I had to do this because of Darius Slay
        dsteststr <- grepl("Darius Slayton", teststr)
        if(isTRUE(dsteststr)){
          teststr <- gsub("Darius Slayton", "NYG", teststr)
        }
        # I had to do this because of Derek Carr
        dcteststr <- grepl("Derek Carrier", teststr)
        if(isTRUE(dcteststr)){
          if(year>=2013&year<=2014){
            teststr <- gsub("Derek Carrier", "SFO", teststr)
          }
          if(year>=2015&year<=2016){
            teststr <- gsub("Derek Carrier", "WAS", teststr)
          }
          if(year==2017){
            teststr <- gsub("Derek Carrier", "LAR", teststr)
          }
          if(year>=2018&year<=2019){
            teststr <- gsub("Derek Carrier", "OAK", teststr)
          }
          if(year>2019){
            teststr <- gsub("Derek Carrier", "LVR", teststr)
          }
        }
        # Switches the rest of the player names to the team they played for
        numlist1 <- c(1:length(checkdf1$Player))
        for(num1 in numlist1){
          res1 <- grepl(checkdf1$Player[num1], teststr)
          if(isTRUE(res1)){
            if(isTRUE(dat$Week[1]>=checkdf1$LowWeek[num1])){
              if(isTRUE(dat$Week[1]<=checkdf1$UpWeek[num1])){
                teststr <- gsub(checkdf1$Player[num1], checkdf1$Tm[num1], teststr)
              }
            }
          }
        }
        numlist2 <- c(1:length(checkdf2$Player))
        for(num2 in numlist2){
          res2 <- grepl(checkdf2$Player[num2], teststr)
          if(isTRUE(res2)){
            if(isTRUE(dat$Week[1]>=checkdf2$LowWeek[num2])){
              if(isTRUE(dat$Week[1]<=checkdf2$UpWeek[num2])){
                teststr <- gsub(checkdf2$Player[num2], checkdf2$Tm[num2], teststr)
              }
            }
          }
        }
        dat[j,]$Detail <- teststr
      }
      
      list1 <- list("Falcons", "Bills", "Panthers", "Bears", "Bengals", "Browns",
                    "Colts", "Cardinals", "Cowboys", "Broncos", "Lions", "Packers",
                    "Texans", "Jaguars", "Chiefs", "Dolphins", "Vikings", "Saints",
                    "Patriots", "Giants", "Jets", "Titans", "Eagles", "Steelers",
                    "Ravens", "Seahawks", "49ers", "Buccaneers", "Team", 
                    "Redskins", "Washington", "Commanders")
      list2 <- list("ATL", "BUF", "CAR", "CHI", "CIN", "CLE", "IND", "ARI", "DAL",
                    "DEN", "DET", "GNB", "HOU", "JAX", "KAN", "MIA", "MIN", "NOR",
                    "NWE", "NYG", "NYJ", "TEN", "PHI", "PIT", "BAL", "SEA", "SFO",
                    "TAM", "WAS", "WAS", "WAS", "WAS")
      
      numlist <- c(1:32)
      for(num in numlist){
        if(test==list1[num]){
          myvar = list2[[num]]
        }
      }
      if(test=="Rams"){
        if(dat$Year[1]<=2015){
          myvar = "STL"
        }
        if(dat$Year[1]>2015){
          mvar = "LAR"
        }
      }
      if(test=="Chargers"){
        if(dat$Year[1]<=2016){
          myvar = "SDG"
        }
        if(dat$Year[1]>2016){
          myvar = "LAC"
        }
      }
      if(test=="Raiders"){
        if(dat$Year[1]<=2019){
          myvar = "OAK"
        }
        if(dat$Year[1]>2019){
          myvar = "LVR"
        }
      }
      
      if(myvar==pos1){
        curpos = pos1
        store = pos2
      }
      if(myvar==pos2){
        curpos = pos2
        store = pos1
      }
      
      dat$Possession <- ""
      dat[4,]$Possession <- curpos 
      
      datlen <- length(dat$gamenum)
      datnums <- c(4:(datlen-1))
      for(dats in datnums){
        maintest <- unlist(str_split(dat$Detail[[dats]], " "))
        mainword <- maintest[1]
        if(isTRUE(mainword==pos1)){
          curpos = pos1
          dat$Possession[dats] = curpos
        }
        if(isTRUE(mainword==pos2)){
          curpos = pos2
          dat$Possession[dats] = curpos
        }
        else{
          dat$Possession[dats] = curpos
        }
      }
    mlist <- c(4: length(dat$Possession))
    # Takes out all unnecessary characters from Detail column
    for(m in mlist){
      minuspars <- gsub("\\(","",dat$Detail[m])
      minuspars <- gsub("\\)","",minuspars)
      minuspars <- gsub("\\.","",minuspars)
      minuspars <- gsub("\\,","",minuspars)
      detailwords <- unlist(str_split(minuspars, " "))
      if(isTRUE("tackle" %in% detailwords)){
        kicktest <- grepl("kicks", dat$Detail[m])
        punttest <- grepl("punts", dat$Detail[m])
        if(isFALSE(kicktest)){
          if(isFALSE(punttest)){
            firstword <- detailwords[1]
            bykey <- which(detailwords=="by")
            tackteam <- detailwords[(bykey[1]+1)]
            if(isTRUE(firstword==tackteam)){
              if(isTRUE(tackteam==colnames(dat)[8])){
                dat$Detail[m] <- str_replace(dat$Detail[m],colnames(dat)[8], colnames(dat)[9])
                dat$Possession[m] <- colnames(dat)[9]
              }
              if(isTRUE(tackteam==colnames(dat)[9])){
                dat$Detail[m] <- str_replace(dat$Detail[m],colnames(dat)[9], colnames(dat)[8])
                dat$Possession[m] <- colnames(dat)[8]
              }
            }
          }
        }
      }
    }
    # There are multiple iterations of the gamelogs and this is the second one
    newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/",team,"/",year)  
    setwd(newwd)
    filename <- paste0("Game ", game,".csv")
    write.csv(dat,filename)
    print(paste0(team,"  ",year,"  ",game,"  "))
    }
  }
}
  



### Checking If it worked

teamlist <- list(
  "crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax",
                 "kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi",
  "pit","sea","sfo","tam","oti","was")
faultylist <- list()
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 2.0/",team,"/",year)
    setwd(mywd)
    if(year>=2021){
      gamelist <- c(1:17)
    }
    if(year<2021){
      gamelist <- c(1:16)
    }
    for(game in gamelist){
      filename <- paste0("Game ", game,".csv")
      dat <- read.csv(filename)
      testdat <- tibble(dat[(4:length(dat$gamenum)),])
      numlist <- c(1:length(testdat$gamenum))
      
      toremove <- c()
      for(num in numlist){
        linewords <- unlist(str_split(testdat[num,]$Detail, " "))
        testword <- linewords[1]
        removelist <- c("Detail", "Penalty", "Timeout", "End", "--", "Replay",
                        "Two", "Overtime", "Detail", "Details", "extra", "",
                        "challenged", "(tackle", "intended", "blocked", "(no",
                        "returned", "no", "Fumble,", "Fumble", "Aborted",
                        "Quarter")
        for(remove in removelist){
          if(isTRUE(testword==remove)){
            toremove <- list.append(toremove, num)
          }
        }
        if(isTRUE(linewords[2]=="Quarter")){
          toremove <- list.append(toremove, num)
        }
        if(isTRUE(linewords[2]=="challenged")){
          toremove <- list.append(toremove, num)
        }
        if(isTRUE(linewords[2]=="won")){
          toremove <- list.append(toremove, num)
        }
      }
      testdat1 <- testdat[-(toremove),]
      testdat1$Match <- ""
      numslist <- c(1:length(testdat1$gamenum))
      for(nums in numslist){
        wordsdet <- unlist(str_split(testdat1[nums,]$Detail, " "))
        wordx <- wordsdet[1]
        if(isTRUE(wordx==testdat1$Possession[nums])){
          testdat1$Match[nums]="YES"
        }
        else{
          testdat1$Match[nums]="NO"
        }
      }
      testcol <- testdat1$Match
      mytest <- "NO" %in% testcol
      if(isTRUE(mytest)){
        faultylist <- list.append(faultylist, testdat1)
      }
      print(paste0(team,"  ",year,"  ",game," "))
    }
  }
}

x <- faultylist[[8]]
### MIA 2014 Damian Williams should be Damien Williams












### Learning
testdat <- tibble(dat[(4:length(dat$gamenum)),])
numlist <- c(1:length(testdat$gamenum))

toremove <- c()
for(num in numlist){
  linewords <- unlist(str_split(testdat[num,]$Detail, " "))
  testword <- linewords[1]
  removelist <- c("Detail", "Penalty", "Timeout", "End", "--", "Replay", "Two")
  for(remove in removelist){
    if(isTRUE(testword==remove)){
      toremove <- list.append(toremove, num)
    }
  }
  if(isTRUE(linewords[2]=="Quarter")){
    toremove <- list.append(toremove, num)
  }
  if(isTRUE(linewords[2]=="challenged")){
    toremove <- list.append(toremove, num)
  }
}
testdat1 <- testdat[-(toremove),]
testdat1$Match <- ""
numslist <- c(1:length(testdat1$gamenum))
for(nums in numslist){
  wordsdet <- unlist(str_split(testdat1[nums,]$Detail, " "))
  wordx <- wordsdet[1]
  if(isTRUE(wordx==testdat1$Possession[nums])){
    testdat1$Match[nums]="YES"
  }
  else{
    testdat1$Match[nums]="NO"
  }
}
























### Learning
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs/atl/2006")
dat <- read.csv("Game 2.csv") %>% select(-X)
words2 <- unlist(str_split(dat$Detail[[2]], " "))
x <- which(words2 == "receive")
test <- words2[x-2]

words3 <- unlist(str_split(dat$Detail[[3]], " "))




setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project")
playteam <- read.csv("Final Player Team Data.csv")


datlen <- length(dat$gamenum)
numslist <- c(1:datlen)
for(j in numslist){
  teststr <- dat[j,]$Detail
  numlist <- c(1:41785)
  for(num in numlist){
    res <- grepl(playteam$Player[num], teststr)
    if(isTRUE(res)){
      if(dat$Year[j]==playteam$Year[num]){
        teststr <- gsub(playteam$Player[num], playteam$Tm[num], teststr)
      }
    }
  }
  dat[j,]$Detail <- teststr
}


cols <- colnames(dat)
pos1 <- cols[8]
pos2 <- cols[9]

list1 <- list("Falcons", "Bills", "Panthers", "Bears", "Bengals", "Browns",
              "Colts", "Cardinals", "Cowboys", "Broncos", "Lions", "Packers",
              "Texans", "Jaguars", "Chiefs", "Dolphins", "Vikings", "Saints",
              "Patriots", "Giants", "Jets", "Titans", "Eagles", "Steelers",
              "Ravens", "Seahawks", "49ers", "Buccaneers")
list2 <- list("ATL", "BUF", "CAR", "CHI", "CIN", "CLE", "IND", "ARI", "DAL",
              "DEN", "DET", "GNB", "HOU", "JAX", "KAN", "MIA", "MIN", "NOR",
              "NWE", "NYG", "NYJ", "TEN", "PHI", "PIT", "BAL", "SEA", "SFO",
              "TAM")

numlist <- c(1:28)
for(num in numlist){
  if(test==list1[num]){
    myvar <- list2[[num]]
  }
}
if(test=="Rams"){
  if(dat$Year[1]<=2015){
    myvar <- "STL"
  }
  if(dat$Year[1]>2015){
    mvar <- "LAR"
  }
}
if(test=="Chargers"){
  if(dat$Year[1]<=2016){
    myvar <- "SDG"
  }
  if(dat$Year[1]>2016){
    myvar <- "LAC"
  }
}
if(test=="Raiders"){
  if(dat$Year[1]<=2019){
    myvar <- "OAK"
  }
  if(dat$Year[1]>2019){
    myvar <- "LVR"
  }
}

if(myvar==pos1){
  curpos = pos2
  store = pos1
}
if(myvar==pos2){
  curpos = pos1
  store = pos2
}

dat$Possession <- ""
dat[2,]$Possession <- curpos

datlen <- length(dat$gamenum)
datnums <- c(2:(datlen-1))
for(dats in datnums){
  ### Kickoff
  stringtest <- grepl("kicks off", dat$Detail[dats])
  if(isTRUE(stringtest)){
    substrtest <- grepl("no play", dat$Detail[dats])
    if(isFALSE(substrtest)){
      dummy <- curpos
      curpos <- store
      store <- dummy
      dat$Possession[dats+1]<- curpos
    }
  }
  ### INT
  stringtest <- grepl("intercepted", dat$Detail[dats])
  if(isTRUE(stringtest)){
    substrtest <- grepl("no play", dat$Detail[dats])
    if(isFALSE(substrtest)){
      dummy <- curpos
      curpos <- store
      store <- dummy
      dat$Possession[dats+1]<- curpos
    }
  }
  ### FUMBLE
  teststr1 <- paste0("recovered by ",store)
  stringtest <- grepl(teststr1, dat$Detail[dats])
  if(isTRUE(stringtest)){
    substrtest <- grepl("no play", dat$Detail[dats])
    if(isFALSE(substrtest)){
      dummy <- curpos
      curpos <- store
      store <- dummy
      dat$Possession[dats+1]<- curpos
    }
  }
  ### TOD
  stringtest1 <- grepl("punts", dat$Detail[dats])
  if(isFALSE(stringtest1)){
    if(dat$Down[dats]==4){
      stringtest <- grepl("incomplete", dat$Detail[dats])
      if(isTRUE(stringtest)){
        substrtest <- grepl("no play", dat$Detail[dats])
        if(isFALSE(substrtest)){
          dummy <- curpos
          curpos <- store
          store <- dummy
          dat$Possession[dats+1]<- curpos
        }
      }
      stringtest2 <- grepl("yards", dat$Detail[dats])
      if(isTRUE(stringtest2)){
        wordstest <- unlist(str_split(dat$Detail[[dats]], " "))
        x <- which(words2 == "yards")
        numyards <- wordstest[x-1]
        numyards <- as.numeric(numyards)
        if(numyards<dat$ToGo[dats]){
          substrtest <- grepl("no play", dat$Detail[dats])
          if(isFALSE(substrtest)){
            dummy <- curpos
            curpos <- store
            store <- dummy
            dat$Possession[dats+1]<- curpos
          }
        }
      }
    }
  }
  ### PUNT
  stringtest <- grepl("punts", dat$Detail[dats])
  if(isTRUE(stringtest)){
    substrtest <- grepl("no play", dat$Detail[dats])
    if(isFALSE(substrtest)){
      dummy <- curpos
      curpos <- store
      store <- dummy
      dat$Possession[dats+1]<- curpos
    }
  }
  ### MISSED FG
  stringtest <- grepl("field goal no good", dat$Detail[dats])
  if(isTRUE(stringtest)){
    substrtest <- grepl("no play", dat$Detail[dats])
    if(isFALSE(substrtest)){
      dummy <- curpos
      curpos <- store
      store <- dummy
      dat$Possession[dats+1]<- curpos
    }
  }
  else{
    dat$Possession[dats+1] <- curpos
  }
}

# COMMENT 1:
#     dats <- dats - 1
#       ### OT
#       if(isTRUE(dat$Detail[dats]=="Overtime")){
#         dat$Possession[dats] = ""
#         dat$Possession[dats+1] = ""
#         words3 <- unlist(str_split(dat$Detail[[dats+2]], " "))
#         x <- which(words3 == "receive")
#         test <- words3[x-2]
#         numlist <- c(1:32)
#         for(num in numlist){
#           if(test==list1[num]){
#             myvar = list2[[num]]
#           }
#         }
#         if(test=="Rams"){
#           if(dat$Year[1]<=2015){
#             myvar = "STL"
#           }
#           if(dat$Year[1]>2015){
#             mvar = "LAR"
#           }
#         }
#         if(test=="Chargers"){
#           if(dat$Year[1]<=2016){
#             myvar = "SDG"
#           }
#           if(dat$Year[1]>2016){
#             myvar = "LAC"
#           }
#         }
#         if(test=="Raiders"){
#           if(dat$Year[1]<=2019){
#             myvar = "OAK"
#           }
#           if(dat$Year[1]>2019){
#             myvar = "LVR"
#           }
#         }
#         if(isTRUE(curpos==myvar)){
#           curpos = curpos
#         }
#         if(isTRUE(store==myvar)){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#         }
#         dat$Possession[dats+2]<-curpos
#       }
#         ### Overtime coin toss
#         stringtestf <- grepl("the coin toss", dat$Detail[dats])
#         if(isTRUE(stringtestf)){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#         ### TOD
#         teststry <- paste0("recoverd by ", store)
#         stringtesty <- grepl(teststry, dat$Detail[dats])
#         if(isFALSE(stringtesty)){
#           stringtestx <- grepl("intercepted", dat$Detail[dats])
#           if(isFALSE(stringtestx)){
#             stringtest1 <- grepl("punts", dat$Detail[dats])
#             if(isFALSE(stringtest1)){
#               if(dat$Down[dats]==4){
#                 stringtest <- grepl("incomplete", dat$Detail[dats])
#                 if(isTRUE(stringtest)){
#                   substrtest <- grepl("no play", dat$Detail[dats])
#                   if(isFALSE(substrtest)){
#                     dummy <- curpos
#                     curpos <- store
#                     store <- dummy
#                     dat$Possession[dats+1]<- curpos
#                   }
#                 }
#                 stringtestb <- grepl("no gain", dat$Detail[dats])
#                 if(isTRUE(stringtestb)){
#                   substrtestb <- grepl("no play", dat$Detail[dats])
#                   if(isFALSE(substrtestb)){
#                     dummy <- curpos
#                     curpos <- store
#                     store <- dummy
#                     dat$Possession[dats+1]<- curpos
#                   }
#                 }
#                 stringtestc <- grepl(" 1 yard", dat$Detail[dats])
#                 if(isTRUE(stringtestc)){
#                   if(isTRUE(dat$ToGo[dats]>1)){
#                     substrtestd <- grepl("no play", dat$Detail[dats])
#                     if(isFALSE(substrtestd)){
#                       dummy <- curpos
#                       curpos <- store
#                       store <- dummy
#                       dat$Possession[dats+1]<- curpos
#                   }
#                   }
#                 }
#                 stringtest2 <- grepl("yards", dat$Detail[dats])
#                 if(isTRUE(stringtest2)){
#                   stringtest3 <- grepl("touchdown", dat$Detail[dats])
#                   if(isFALSE(stringtest3)){
#                     wordstest <- unlist(str_split(dat$Detail[[dats]], " "))
#                     x <- which(wordstest == "yards")
#                     numyards <- wordstest[x[1]-1]
#                     numyards <- as.numeric(numyards)
#                     testdummy <- as.numeric(dat$ToGo[dats])
#                     if(isTRUE(numyards<testdummy)){
#                       substrtest <- grepl("no play", dat$Detail[dats])
#                       if(isFALSE(substrtest)){
#                         dummy <- curpos
#                         curpos <- store
#                         store <- dummy
#                         dat$Possession[dats+1]<- curpos
#                       }
#                     }
#                     
#                   }
#                 }
#               }
#             }
#           }
#         }
#         ### Kickoff
#         stringtest <- grepl("kicks off", dat$Detail[dats])
#         if(isTRUE(stringtest)){
#           substrtest <- grepl("no play", dat$Detail[dats])
#           if(isFALSE(substrtest)){
#             dummy <- curpos
#             curpos <- store
#             store <- dummy
#             dat$Possession[dats+1]<- curpos
#           }
#         }
#         ### INT
#         stringtest <- grepl("intercepted", dat$Detail[dats])
#         if(isTRUE(stringtest)){
#           substrtest <- grepl("no play", dat$Detail[dats])
#           if(isFALSE(substrtest)){
#             dummy <- curpos
#             curpos <- store
#             store <- dummy
#             dat$Possession[dats+1]<- curpos
#           }
#         }
#         ### FUMBLE
#         teststr1 <- paste0("recovered by ",store)
#         stringtest <- grepl(teststr1, dat$Detail[dats])
#         if(isTRUE(stringtest)){
#           substrtest <- grepl("no play", dat$Detail[dats])
#           if(isFALSE(substrtest)){
#             dummy <- curpos
#             curpos <- store
#             store <- dummy
#             dat$Possession[dats+1]<- curpos
#           }
#         }
#       ### PUNT
#       stringtest <- grepl("punts", dat$Detail[dats])
#       if(isTRUE(stringtest)){
#         stringtesta <- grepl(paste0("recovered by ", curpos), dat$Detail[dats])
#         if(isTRUE(stringtesta)){
#           substrtest <- grepl("no play", dat$Detail[dats])
#           if(isFALSE(substrtest)){
#             dat$Possession[dats+1]<- curpos
#           }
#         }
#         else{
#         substrtest <- grepl("no play", dat$Detail[dats])
#           if(isFALSE(substrtest)){
#             dummy <- curpos
#             curpos <- store
#             store <- dummy
#             dat$Possession[dats+1]<- curpos
#           }
#         }
#       }
#       
#       ### RECOVERED MUFFED PUNT
#       teststr1 <- paste0("muffed catch by ",store,", recovered by ",store)
#       stringtest <- grepl(teststr1, dat$Detail[dats])
#       if(isTRUE(stringtest)){
#         substrtest <- grepl("no play", dat$Detail[dats])
#         if(isFALSE(substrtest)){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#       }
#       ### FAILED ONSIDE
#       stringtest <- grepl("onside", dat$Detail[dats])
#       if(isTRUE(stringtest)){
#         substrtest <- grepl("no play", dat$Detail[dats])
#         if(isFALSE(substrtest)){
#           wordsx <- unlist(str_split(dat$Detail[[dats+1]], " "))
#           if(wordsx[1]==store){
#             dummy <- curpos
#             curpos <- store
#             store <- dummy
#             dat$Possession[dats+1]<- curpos
#           }
#         }
#       }
#       ### Weird fumble recovery not enough yards scenario
#       stringtestb <- grepl("punts", dat$Detail[dats])
#       if(isFALSE(stringtestb)){
#         if(dat$Down[dats]==4){
#           teststrz <- paste0("fumbles, recovered by ",curpos)
#           stringtestz <- grepl(teststrz, dat$Detail[dats])
#           if(isTRUE(stringtestz)){
#             wordsx <- unlist(str_split(dat$Detail[[dats+1]], " "))
#             if(wordsx[1]==store){
#               dummy <- curpos
#               curpos <- store
#               store <- dummy
#               dat$Possession[dats+1]<- curpos
#             }
#           }
#         }
#       }
#       
#       ### MISSED FG
#       stringtest <- grepl("field goal no good", dat$Detail[dats])
#       if(isTRUE(stringtest)){
#         substrtest <- grepl("no play", dat$Detail[dats])
#         if(isFALSE(substrtest)){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#         substrtestx <- grepl("touchdown", dat$Detail[dats])
#         if(isTRUE(substrtestx)){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#         teststrg <- paste0("recovered by ",store)
#         substrtestg <- grepl(teststrg, dat$Detail[dats])
#         if(isTRUE(substrtestg)){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#       }
#       ### START OF HALF 2
#       if(dat$Detail[dats]=="3rd Quarter"){
#         if(dat$Possession[4]==curpos){
#           dat$Possession[dats+1]<-curpos
#         }
#         if(dat$Possession[4]==store){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#       }
#       ### 4TH DOWN FUMBLE
#       substrtesth <- grepl("recovered by", dat$Detail[dats])
#       if(isTRUE(substrtesth)){
#         wordsk <- unlist(str_split(dat$Detail[[dats+1]], " "))
#         mywordstest <- wordsk[1]
#         if(isTRUE(mywordstest==curpos)){
#           dat$Possession[dats+1]<- curpos
#         }
#         if(isTRUE(mywordstest==store)){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#       }
#       ### WEIRD RANDOM DASH
#       if(dat$Detail[dats]=="--"){
#         wordsx <- unlist(str_split(dat$Detail[[dats+1]], " "))
#         if(wordsx[1]==store){
#           dummy <- curpos
#           curpos <- store
#           store <- dummy
#           dat$Possession[dats+1]<- curpos
#         }
#         if(wordsx[1]==curpos){
#           dat$Possession[dats+1]<- curpos
#         }
#       }
#           dat$Possession[dats+1] <- curpos
#   }
# }