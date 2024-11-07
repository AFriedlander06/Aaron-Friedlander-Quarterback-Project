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

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Gamelogs")
gldf <- readRDS("AllGamelogs.rds") |> select(-X)
colnames(gldf)[colnames(gldf)=="Tm"] <- "FolderTm"


gldf$Year <- as.character(gldf$Year)
gldf$gamenum <- as.character(gldf$gamenum)
totdf <- left_join(gldf, gidf)
totdf <- totdf %>% relocate((Roof:OvUndResult), .after = Time)

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/Gamelogs-GameInfo")
saveRDS(totdf, "GameLogs+Info.rds")

