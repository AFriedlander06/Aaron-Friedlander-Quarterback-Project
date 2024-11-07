library(rvest)
library(rlist)
library(dplyr)
library(stringr)
library(tidyverse)
library(installr)
library(fs)
library(XML)
library(xml2)


#### Compiling Whole Year Game Logs
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")

for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    setuplist <- list()
    mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Game Info/",team,"/",year)
    setwd(mywd)
    filenames <- list.files()
    if(length(filenames)==16){
      newnames <- list(filenames[1],filenames[9],filenames[10],filenames[11],filenames[12],
                       filenames[13],filenames[14],filenames[15],filenames[16],filenames[2],
                       filenames[3],filenames[4],filenames[5],filenames[6],filenames[7],
                       filenames[8])
      filenames <- newnames
      
      intlist <- c(1:16)
      for(int in intlist){
        myvar <-filenames[int]
        newvar <- myvar[[1]][[1]]
        curfile <- read.csv(newvar)
        week <- curfile$Week
        curfile <- curfile[-1,]
        newfile <- curfile %>% select(c(2:3))
        newfile <- as.data.frame(t(newfile))
        colnames(newfile)<-newfile[1,]
        newfile$Tm <- team
        newfile$Year <- year
        newfile$game_num <- int
        curfile <- newfile[-1,]
        curfile <- curfile %>% relocate((Tm:game_num), .before = `Won Toss`)
        curfile$Week <- week[1]
        curfile <- curfile |> relocate(Week, .after = game_num)
        if(length(curfile)<12){
          curfile$`Won OT Toss`<-" "
        }
        setuplist <- list.append(setuplist, curfile)
      }
      basefile <- setuplist[[1]]
      numlist <- c(2:16)
      for(num in numlist){
        basefile <- bind_rows(basefile, setuplist[[num]])
      }
      
    }
    
    
    if(length(filenames)==17){
      newnames <- list(filenames[1],filenames[10],filenames[11],filenames[12],filenames[13],
                       filenames[14],filenames[15],filenames[16],filenames[17],filenames[2],
                       filenames[3],filenames[4],filenames[5],filenames[6],filenames[7],
                       filenames[8],filenames[9])
      filenames <- newnames
      
      
      intlist <- c(1:17)
      for(int in intlist){
        myvar <-filenames[int]
        newvar <- myvar[[1]][[1]]
        curfile <- read.csv(newvar)
        week <- curfile$Week
        curfile <- curfile[-1,]
        newfile <- curfile %>% select(c(2:3))
        newfile <- as.data.frame(t(newfile))
        colnames(newfile)<-newfile[1,]
        newfile$Tm <- team
        newfile$Year <- year
        newfile$game_num <- int
        curfile <- newfile[-1,]
        curfile$Week <- week[1]
        curfile <- curfile |> relocate(Week, .after = game_num)
        if(length(curfile)<12){
          curfile$`Won OT Toss`<-" "
        }
        setuplist <- list.append(setuplist, curfile)
      }
      basefile <- setuplist[[1]]
      numlist <- c(2:17)
      for(num in numlist){
        basefile <- bind_rows(basefile, setuplist[[num]])
      }
    }
    setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Game Info/",team,"/Whole Year Game Info"))
    myfilename <- paste0(team, year,"allgameinfo.csv")
    basefile$Weather[is.na(basefile$Weather)]<-"No Information"
    basefile <- basefile |> select(-`Won OT Toss`)
    write.csv(basefile, myfilename)
  }
}

#### Compiling Whole Team Game Info
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Game Info/",team,"/Whole Year Game Info")
  setwd(mywd)
  filelist <- list.files()
  myfilelist <- list()
  for(file in filelist){
    myfile <- read.csv(file)
    myfile <- myfile %>% select(-X)
    myfilelist <- list.append(myfilelist, myfile)
  }
  basefile <- myfilelist[[1]]
  numlist <- c(2:18)
  for(num in numlist){
    basefile <- rbind(basefile, myfilelist[[num]])
  }
  setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Game Info/wholeteamgameinfo")
  myfilename <- paste0(team,"allgameinfo.csv")
  write.csv(basefile, myfilename)
}

### Compiling them all into one
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Game Info/wholeteamgameinfo")
filelist <- list.files()
myfilelist <- list()
for(file in filelist){
  myfile <- read.csv(file) %>% select(-X)
  myfilelist <- list.append(myfilelist, myfile)
}

basefile <- myfilelist[[1]]
numlist <- c(2:length(myfilelist))
for(num in numlist){
  basefile <- rbind(basefile, myfilelist[[num]])
}

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Game Info")
saveRDS(basefile, "AllGameInfo.rds")
