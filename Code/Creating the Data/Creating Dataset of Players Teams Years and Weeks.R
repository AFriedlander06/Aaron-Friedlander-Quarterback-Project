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

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project")
normposdf <- read.csv("NFL Player Keys - 2006+.csv") |> select(playername, playerkey)
### For punters and kickers
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/Player Related Data")
pkoldf <- readRDS("KickerPunterOlinePFRLinks.rds") |> select(Player, Links)
colnames(pkoldf)[colnames(pkoldf)=="Player"] <- "playername"
colnames(pkoldf)[colnames(pkoldf)=="Links"] <- "playerkey"

mydat <- rbind(normposdf, pkoldf)

colnames(mydat)[colnames(mydat)=="playername"] <- "name"
### End of For punters and kickers
base <- "https://www.pro-football-reference.com"
numlist <- c(5721:length(mydat$playerkey))
dflist <- list()
for(num in numlist){
  link <- mydat$playerkey[num]
  myhtml <- paste0(base, link)
  mysite <- read_html(myhtml)
  mytable <- html_table(mysite)
  x <- mytable[[1]]
  if(isTRUE(colnames(x)[1]=="Season")){
    bb <- num/length(mydat$name)
    bb <- format(round(bb, 5))
    bb <- as.numeric(bb) * 100
    print(paste0(num,"     ",bb,"%"))
    Sys.sleep(3)
    next
  }
  if(isTRUE(x[1][1,]!="Year")){
    if(isTRUE(colnames(x)[1]!="Year")){
      x <- mytable[[2]]
    }
  }
  if(isTRUE(x[1][1,]!="Year")){
    if(isTRUE(colnames(x)[1]!="Year")){
      x <- mytable[[3]]
    }
  }
  if(isTRUE(x[1][1,]!="Year")){
    if(isTRUE(colnames(x)[1]!="Year")){
      x <- mytable[[4]]
    }
  }
  if(isTRUE(x[1][1,]!="Year")){
    if(isTRUE(colnames(x)[1]!="Year")){
      x <- mytable[[5]]
    }
  }
  mydf <- x
  if(isTRUE(colnames(x)[1]!="Year")){
    colnames(mydf) <- mydf[1,]
    mydf <- mydf[-1,]
  }
  numslist <- c(1:length(mydf$Year))
  for(nums in numslist){
    mydf$Year[nums] <- gsub("\\+","",mydf$Year[nums])
    mydf$Year[nums] <- gsub("\\*","",mydf$Year[nums])
  }
  mydf$Year <- as.numeric(mydf$Year)
  addon <- "/gamelog/"
  mydf$LowWeek <- ""
  mydf$UpWeek <- ""
  mydf <- mydf %>% relocate((LowWeek:UpWeek),.after = Tm)
  klist <- c(1:length(mydf$Year))
  for(k in klist){
    TMtest <- grepl("TM", mydf$Tm[k])
    if(isTRUE(TMtest)){
      addonyear <- mydf$Year[k]
      newplayeraddon <- gsub(".htm","",link)
      newlink <- paste0(base, newplayeraddon,addon,addonyear,"/")
      newhtml <- read_html(newlink)
      newtable <- html_table(newhtml)[[1]]
      colnames(newtable) <- newtable[1,]
      newtable <- newtable[-1,]
      if(isTRUE(mydf$Tm[k]=="2TM")){
        tm1test <- which(newtable$Tm==mydf$Tm[(k+1)])
        tm2test <- which(newtable$Tm==mydf$Tm[(k+2)])
        mydf$LowWeek[(k+1)] <- newtable$Week[min(tm1test)]
        mydf$UpWeek[(k+1)] <- newtable$Week[max(tm1test)]
        mydf$LowWeek[(k+2)] <- newtable$Week[min(tm2test)]
        mydf$UpWeek[(k+2)] <- newtable$Week[max(tm2test)]
      }
      if(isTRUE(mydf$Tm[k]=="3TM")){
        tm1test <- which(newtable$Tm==mydf$Tm[(k+1)])
        tm2test <- which(newtable$Tm==mydf$Tm[(k+2)])
        tm3test <- which(newtable$Tm==mydf$Tm[(k+3)])
        mydf$LowWeek[(k+1)] <- newtable$Week[min(tm1test)]
        mydf$UpWeek[(k+1)] <- newtable$Week[max(tm1test)]
        mydf$LowWeek[(k+2)] <- newtable$Week[min(tm2test)]
        mydf$UpWeek[(k+2)] <- newtable$Week[max(tm2test)]
        mydf$LowWeek[(k+3)] <- newtable$Week[min(tm3test)]
        mydf$UpWeek[(k+3)] <- newtable$Week[max(tm3test)]
      }
      if(isTRUE(mydf$Tm[k]=="4TM")){
        tm1test <- which(newtable$Tm==mydf$Tm[(k+1)])
        tm2test <- which(newtable$Tm==mydf$Tm[(k+2)])
        tm3test <- which(newtable$Tm==mydf$Tm[(k+3)])
        tm4test <- which(newtable$Tm==mydf$Tm[(k+4)])
        mydf$LowWeek[(k+1)] <- newtable$Week[min(tm1test)]
        mydf$UpWeek[(k+1)] <- newtable$Week[max(tm1test)]
        mydf$LowWeek[(k+2)] <- newtable$Week[min(tm2test)]
        mydf$UpWeek[(k+2)] <- newtable$Week[max(tm2test)]
        mydf$LowWeek[(k+3)] <- newtable$Week[min(tm3test)]
        mydf$UpWeek[(k+3)] <- newtable$Week[max(tm3test)]
        mydf$LowWeek[(k+4)] <- newtable$Week[min(tm4test)]
        mydf$UpWeek[(k+4)] <- newtable$Week[max(tm4test)]
      }
      Sys.sleep(3)
    }
  }
  mydf <- mydf %>% select(Year:UpWeek)
  endtest <- which(mydf$Age=="Career")
  if(isTRUE(is.empty(endtest))){
    mydf <- mydf %>% filter(!is.na(Year))
  }
  if(isFALSE(is.empty(endtest))){
    mydf <- mydf[-(endtest:length(mydf$Year)),]
  }
  mydf <- mydf %>% select(-Age)
  numzlist <- c(1:length(mydf$Year))
  for(numz in numzlist){
    if(isTRUE(is.na(mydf$Year[numz]))){
      mydf$Year[numz]<-mydf$Year[(numz-1)]
    }
  }
  mydf <- mydf %>% filter(!(grepl("TM",Tm)))
  newdf <- mydf %>% filter(Year>=2006)
  newdf$Player <- mydat$name[num]
  mlist <- c(1:length(newdf$Player))
  for(m in mlist){
    if(isTRUE(newdf$LowWeek[m]=="")){
      newdf$LowWeek[m] <- 1
      newdf$UpWeek[m] <- 18
    }
  }
  newdf <- newdf %>% relocate(Player, .before = Year)
  dflist <- list.append(dflist, newdf)
  bb <- num/length(mydat$name)
  bb <- format(round(bb, 5))
  bb <- as.numeric(bb) * 100
  print(paste0(num,"     ",bb,"%"))
  Sys.sleep(3)
}

basefile <- dflist[[1]]
jlist <- c(2:length(dflist))
for(j in jlist){
  basefile <- rbind(basefile, dflist[[j]])
}

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Temp Place for PlayerNameWeekYear")
saveRDS(basefile,"PTYW9.rds")






setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project")
x1 <- read.csv("NewTeamPlayerWeek.csv") %>% select(-X)
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/PunterKickerStuff")
x2 <- read.csv("PuntKickPlayerTeamWeek.csv") %>% select(-X)

combdf <- rbind(x1, x2)
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project")
write.csv(combdf, "NewTeamPlayerWeek.csv")
