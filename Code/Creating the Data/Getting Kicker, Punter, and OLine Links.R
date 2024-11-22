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


### KICKERS
mylink <- read_html("https://www.pro-football-reference.com/players/kindex.htm")
x <- mylink %>% html_nodes(".left")
y <- html_attr(html_nodes(x,"a"),"href")

mytable <- mylink %>% html_table()
mydf <- mytable[[1]]
mydf$Links <- y

kickdf <- mydf



### PUNTER
mylink <- read_html("https://www.pro-football-reference.com/players/pindex.htm")
x <- mylink %>% html_nodes(".left")
y <- html_attr(html_nodes(x,"a"),"href")

mytable <- mylink %>% html_table()
mydf <- mytable[[1]]
mydf$Links <- y

puntdf <- mydf


### OLINE
mylink <- read_html("https://www.pro-football-reference.com/players/olindex.htm")
x <- mylink %>% html_nodes(".left")
y <- html_attr(html_nodes(x,"a"),"href")

mytable <- mylink %>% html_table()
mydf <- mytable[[1]]
mydf$Links <- y

olinedf <- mydf

newdf <- rbind(kickdf, puntdf, olinedf)

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/Player Related Data")
saveRDS(newdf, "KickerPunterOlinePFRLinks.rds")


# Getting Needed Data
### KICKERS
mydf <- readRDS("KickerPunterOlinePFRLinks.rds")
mydf <- mydf %>% filter(To>=2006)
baselink <- "https://www.pro-football-reference.com/"
addlink <- mydf$Links
numlist <- c(580:length(mydf$Links))
#playerteamlist <- list()
for(num in numlist){
  mylink <- paste0(baselink, mydf$Links[num])
  myhtml <- read_html(mylink)
  mytables <- myhtml %>% html_table()
  y <- mytables[[1]]
  if((isTRUE(y[1][1,]!="Year") & isTRUE(colnames(y[1][1,]!="Year")))){
    y = mytables[[2]]
  }
  if((isTRUE(y[1][1,]!="Year") & isTRUE(colnames(y[1][1,]!="Year")))){
    y = mytables[[3]]
  }
  if(isTRUE(colnames(y[1][1,]!="Year"))){
    colnames(y) <- y[1,]
    finaldf <- y[-1,]
  }
  test <- which(finaldf$Year == "Career")
  finaldf1 <- finaldf[-c(test:length(finaldf$Year)),]
  finaldf1$Player <- mydf$Player[num]
  finalxdf <- finaldf1 %>% select(c(Player, Tm, Year))
  playerteamlist <- list.append(playerteamlist, finalxdf)
  Sys.sleep(3)
  print(num)
}

basefile <- playerteamlist[[1]]
numlist <- c(1:length(playerteamlist))
for(num in numlist){
  basefile <- rbind(basefile, playerteamlist[[num]])
  print(num)
}

basefile <- data.frame(lapply(basefile, function(x) {
  gsub("\\*", "", x)
}))
basefile <- data.frame(lapply(basefile, function(x) {
  gsub("\\+", "", x)
}))

saveRDS(basefile, "KickerPunterOLineData.rds")
