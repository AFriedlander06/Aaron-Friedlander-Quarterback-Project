library(rvest)
library(rlist)
library(dplyr)
library(stringr)
library(tidyverse)
library(installr)
library(fs)
library(XML)
library(xml2)


### Adding Year to Individual Game Logs
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/",team,"/",year)
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
        curfile <- read.csv(myvar[[1]])
        curfile$gamenum <- int
        curfile$Year <- year
        curfile <- curfile %>% select(-X)
        curfile <- curfile %>% relocate((gamenum:Year),.before = Quarter)
        newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/",team,"/",year)
        setwd(newwd)
        filename <- paste0("Game ", int, ".csv")
        write.csv(curfile, filename)
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
        curfile <- read.csv(myvar[[1]])
        curfile$gamenum <- int
        curfile$Year <- year
        curfile <- curfile %>% select(-X)
        curfile <- curfile %>% relocate((gamenum:Year),.before = Quarter)
        newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/",team,"/",year)
        setwd(newwd)
        filename <- paste0("Game ", int,".csv")
        write.csv(curfile, filename)
      }
    }
  }
  print(team)
}





#### Compiling Whole Year Game Logs
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")

for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    setuplist <- list()
    mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/",team,"/",year)
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
        curfile <- read.csv(myvar[[1]])
        curfile$gamenum <- int
        curfile$Year <- year
        curfile <- curfile %>% select(-X)
        colnames(curfile)[6]<-"AwayTeam"
        colnames(curfile)[7]<-"HomeTeam"
        curfile <- curfile %>% relocate((gamenum:Year),.before = Quarter)
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
        curfile <- read.csv(myvar[[1]])
        curfile$gamenum <- int
        curfile$Year <- year
        curfile <- curfile %>% select(-X)
        colnames(curfile)[6]<-"AwayTeam"
        colnames(curfile)[7]<-"HomeTeam"
        curfile <- curfile %>% relocate((gamenum:Year),.before = Quarter)
        setuplist <- list.append(setuplist, curfile)
      }
      basefile <- setuplist[[1]]
      numlist <- c(2:17)
      for(num in numlist){
        basefile <- rbind(basefile, setuplist[[num]])
        }
    }
    newwd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/",team,"/Whole Year Game Logs")
    setwd(newwd)
    myfilename <- paste0(year,"allgamelogs.csv")
    write.csv(basefile, myfilename)
  }
  print(team)
}

#### Compiling Whole Team Game Logs
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/",team,"/Whole Year Game Logs")
  setwd(mywd)
  filelist <- list.files()
  myfilelist <- list()
  for(file in filelist){
    myfile <- read.csv(file)
    myfile$Tm <- team
    myfile <- myfile %>% select(-X)
    myfile <- myfile %>% relocate(Tm, .after = Year)
    myfilelist <- list.append(myfilelist, myfile)
  }
  basefile <- myfilelist[[1]]
  numlist <- c(2:18)
  for(num in numlist){
    basefile <- rbind(basefile, myfilelist[[num]])
  }
  setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/wholeteamgamelogs")
  myfilename <- paste0(team,"allgamelogs.csv")
  write.csv(basefile, myfilename)
}

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/wholeteamgamelogs")
filelist <- list.files()
myfilelist <- list ()
for(file in filelist){
  myfile <- read.csv(file)
  myfilelist <- list.append(myfilelist, myfile)
}

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs/wholeteamgamelogs")
filelist <- list.files()
myfilelist <- list()
for(file in filelist){
  myfile <- read.csv(file)
  myfilelist <- list.append(myfilelist, myfile)
}

basefile <- myfilelist[[1]]
numlist <- c(2:32)
for(num in numlist){
  basefile <- rbind(basefile, myfilelist[[num]])
  print(num)
}
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs")
saveRDS(basefile, "AllGamelogs.rds")
