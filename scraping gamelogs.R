library(rvest)
library(rlist)
library(dplyr)
library(stringr)
library(tidyverse)
library(installr)
library(fs)
library(XML)
library(xml2)



teamlist <- list(c("atl","buf","car","chi","cin","cle","clt","crd","dal","den",
                   "det","gnb","htx","jax","kan","mia","min","nor","nwe","nyg",
                   "nyj","oti","phi","pit","rai","ram","rav","sdg","sea","sfo",
                   "tam","was"))
for(team in teamlist){
  base <- "https://www.pro-football-reference.com"
  yearlist <- c(2006:2023)
  for(year in yearlist){
    inithtml <- paste0(base,"/teams/",team,"/",year,".htm")
    mylink <- read_html(inithtml)
    mynodes <- html_nodes(mylink, ".center a")
    if(year >= 2021){
      weeklist <- c(1:17)
    }
    if(year < 2021){
      weeklist <- c(1:16)
    }
    for(week in weeklist){
      extlink <- mynodes[[week]]
      myattr <- xml_attrs(extlink)
      newhtml <- paste0(base, myattr)
      newlink <- read_html(newhtml)
      html_table(newlink)
      mylines = readLines(newhtml)
      mylines = mylines %>% str_trim()
      mylines = mylines[mylines != ""]
      key = mylines  %>%   str_detect("SOR RK") %>%  which()
      mylines = mylines[(key-1):length(mylines)]
      key2 = mylines %>%  str_detect("</table") %>%  which() %>%  min()
      mylines = mylines[1:key2]
      newpage = read_html(str_flatten(mylines))
      predf = html_table(newpage)
      pbpdf <- predf[[1]]
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs/",team,"/",year)
      setwd(mywd)
      myfilename <- paste0("Game ", week,".csv")
      test <- write.csv(pbpdf, myfilename)
      Sys.sleep(3)
    }
  }
}
                          
year <- 2023
team <- "cin"
week <- 17
