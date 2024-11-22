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
library(glmnet)
library(tidymodels)
library(yardstick)
library(caret)
library(rms)
library(nflverse)
library(patchwork)
library(png)

# setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
# 
# dflist <- list()
# for(year in c(2006:2023)){
#   yeardf <- nflfastR::load_pbp(year)
#   dflist <- list.append(dflist, yeardf)
# }
# 
# basefile <- dflist[[1]]
# for(n in c(2:length(dflist))){
#   basefile <- rbind(basefile, dflist[[n]])
# }


# Creating Data of all QB Names -------------------------------------------

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project")

allplayers <- read_csv("NewTeamPlayerWeek.csv")
qbs <- read_csv("QBs 2006+.csv")
colnames(qbs)[colnames(qbs)=="playername"] <- "Player"

combdf <- left_join(allplayers, qbs)
combdf <- na.omit(combdf)
combdf <- combdf |> filter(!grepl("Missed", Tm))
combdf$lowerTm <- combdf$Tm

combdf$lowerTm <- tolower(combdf$lowerTm)
combdf <- combdf |> 
  mutate(lowerTm = case_when(
    lowerTm=="oak" ~ "rai",
    lowerTm=="bal" ~ "rav",
    lowerTm=="lvr" ~ "rai",
    lowerTm=="lar" ~ "ram",
    lowerTm=="stl" ~ "ram",
    lowerTm=="lac" ~ "sdg",
    lowerTm=="ind" ~ "clt",
    lowerTm=="ari" ~ "crd",
    lowerTm=="ten" ~ "oti",
    TRUE ~ lowerTm
  ))

combdf$Player <- gsub("\\+", "", combdf$Player)
combdf$Player <- gsub("\\*", "", combdf$Player)
combdf$Player[combdf$Player=="Derek Carr"] <- "Derek Carr "

teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2023)
  for(year in yearlist){
    dummydf <- combdf |> filter(lowerTm==team, Year==year)
    filename <- paste0(year,".csv")
    setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/QBs by Team/", team))
    write.csv(dummydf, filename)
  }
  print(team)
}
saveRDS(combdf, "QBTeamYearWeek.rds")

# Merging Gamelogs 1 with WPDiff Data -------------------------------------
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
wpdf <- readRDS("NewFinalModResults.rds")
filtdf <- wpdf |> select(FavTeam, Possession, Year, gamenum, Quarter, 
                         Time, Down, ToGo, Team1Name,Team2Name, YdstoEZBef,
                         YdstoEZAft, YdsGained, FDProb, FavExpPts,
                         NonFavExpPts, FavWPB, FavWPA, FavWPDiff, ExpWPDiff, VarWPDiff,
                         PossWPDiff, WPDiffZScore)
filtdf <- filtdf |> filter(WPDiffZScore!=Inf, WPDiffZScore!=-Inf)
filtdf <- filtdf |> 
  mutate(scaledZScores = if_else(WPDiffZScore>=0, sqrt(WPDiffZScore), -sqrt(abs(WPDiffZScore))))
filtdf <- filtdf |> filter(abs(scaledZScores)<20)

filtdf$FolderTm1 <- tolower(filtdf$Team1Name)
filtdf <- filtdf |> 
  mutate(FolderTm1 = case_when(
    FolderTm1=="oak" ~ "rai",
    FolderTm1=="bal" ~ "rav",
    FolderTm1=="lvr" ~ "rai",
    FolderTm1=="lar" ~ "ram",
    FolderTm1=="stl" ~ "ram",
    FolderTm1=="lac" ~ "sdg",
    FolderTm1=="ind" ~ "clt",
    FolderTm1=="ari" ~ "crd",
    FolderTm1=="ten" ~ "oti",
    FolderTm1=="hou" ~ "htx",
    TRUE ~ FolderTm1
  ))
filtdf$FolderTm2 <- tolower(filtdf$Team2Name)
filtdf <- filtdf |> 
  mutate(FolderTm2 = case_when(
    FolderTm2=="oak" ~ "rai",
    FolderTm2=="bal" ~ "rav",
    FolderTm2=="lvr" ~ "rai",
    FolderTm2=="lar" ~ "ram",
    FolderTm2=="stl" ~ "ram",
    FolderTm2=="lac" ~ "sdg",
    FolderTm2=="ind" ~ "clt",
    FolderTm2=="ari" ~ "crd",
    FolderTm2=="ten" ~ "oti",
    FolderTm2=="hou" ~ "htx",
    TRUE ~ FolderTm2
  ))

filtdf$Quarter <- as.character(filtdf$Quarter)
filtdf$Down <- as.character(filtdf$Down)
filtdf$ToGo <- as.character(filtdf$ToGo)


teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2022)
  for(year in yearlist){
    setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs/", team, "/", year))
    filelist <- list.files()
    for(file in filelist){
      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs/", team, "/", year))
      filename <- file
      gamedf <- suppressMessages(read_csv(file, show_col_types = FALSE) |> select(-1))
      gamedf$Team1Name <- colnames(gamedf)[8]
      gamedf$Team2Name <- colnames(gamedf)[9]
      gamedf <- na.omit(gamedf)
      newdf <- suppressMessages(left_join(gamedf, filtdf))
      newdf <- na.omit(newdf)
      if(isTRUE(nrow(newdf)==0)){
        next
      }
      tm1 <- newdf$FolderTm1[1]
      tm2 <- newdf$FolderTm2[1]
      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/QBs by Team/", tm1))
      checkdf1 <- suppressMessages(read_csv(paste0(year,".csv"), show_col_types = FALSE) |> select(-1))
      playerlist1 <- checkdf1$Player
      newdf$QBsingame <- ""
      for(player1 in playerlist1){
        newdf <- newdf |> 
          mutate(QBsingame = if_else(grepl(player1, Detail), player1, QBsingame))
      }
      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/QBs by Team/", tm2))
      checkdf2 <- suppressMessages(read_csv(paste0(year,".csv"), show_col_types = FALSE) |> select(-1))
      playerlist2 <- checkdf2$Player
      for(player2 in playerlist2){
        newdf <- newdf |> 
          mutate(QBsingame = if_else(grepl(player2, Detail), player2, QBsingame))
      }
      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 11.0/", team, "/", year))
      write.csv(newdf, filename)
    }
    print(year)
  }
  print(team)
}



teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2022)
  for(year in yearlist){
    setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 11.0/", team, "/", year))
    filelist <- list.files()
    for(file in filelist){
      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 11.0/", team, "/", year))
      filename <- file
      gamedf <- suppressMessages(read_csv(file, show_col_types = FALSE) |> select(-1))
      gamedf <- gamedf |> 
        mutate(winningTm = if_else(as.numeric(gamedf[8][nrow(gamedf),]) > 
                                     as.numeric(gamedf[9][nrow(gamedf),]),
                                   colnames(gamedf[8]), colnames(gamedf[9])))
      gamedf$QBsingameScore <- NA
      gamedf$QBsingameWinLoss <- NA
      qblist <- unique(na.omit(gamedf$QBsingame))
      for(q in c(1:length(qblist))){
        gamedf <- gamedf |> 
          mutate(dummycol = if_else(QBsingame==qblist[q], scaledZScores, 0)) |> 
          mutate(dummycol = if_else(is.na(dummycol), 0, dummycol))
        gamedf <- gamedf |> 
          mutate(dummycol2 = sum(dummycol))
        gamedf <- gamedf |> 
          mutate(dummycol3 = case_when(
            Possession==winningTm & QBsingame==qblist[q] ~ "W",
            Possession!=winningTm & QBsingame==qblist[q] ~ "L",
            TRUE ~ NA
          )) 
        myval <- na.omit(gamedf$dummycol3)[1]
        gamedf <- gamedf |> 
          mutate(dummycol3 = if_else(is.na(dummycol3), myval, dummycol3))
        gamedf <- gamedf |> 
          mutate(QBsingameScore = if_else(QBsingame==qblist[q], dummycol2, QBsingameScore)) |> 
          mutate(QBsingameWinLoss = if_else(QBsingame==qblist[q], dummycol3, QBsingameWinLoss))
        colnames(gamedf)[colnames(gamedf)=="dummycol"] <- gsub(" ", "", qblist[q])
        colnames(gamedf)[colnames(gamedf)=="dummycol2"] <- gsub(" ", "", paste0(qblist[q],"Score"))
        colnames(gamedf)[colnames(gamedf)=="dummycol3"] <- gsub(" ", "", paste0(qblist[q],"WinLoss"))
      }

      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 12.0/", team, "/", year))
      write.csv(gamedf, filename)
    }
    print(year)
  }
  print(team)
}

finaldflist <- list()
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2022)
  for(year in yearlist){
    setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 12.0/", team, "/", year))
    filelist <- list.files()
    for(file in filelist){
      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 12.0/", team, "/", year))
      filename <- file
      gamedf <- suppressMessages(read_csv(file, show_col_types = FALSE) |> select(-1))
      gamedf$QBsingameTeam <- ""
      gamedf$QBsingameFinalMargin <- NA
      ###
      gamedf <- gamedf |> 
        mutate(QBsingameTeam = if_else(Possession==colnames(gamedf)[8], 
                                       colnames(gamedf)[8],
                                       QBsingameTeam)) |> 
        mutate(QBsingameTeam = if_else(Possession==colnames(gamedf)[9], 
                                       colnames(gamedf)[9],
                                       QBsingameTeam)) |> 
        mutate(QBsingameFinalMargin = if_else(QBsingameTeam==colnames(gamedf)[8],
                                              as.numeric(gamedf[8][nrow(gamedf),]) -
                                                as.numeric(gamedf[9][nrow(gamedf),]),
                                              QBsingameFinalMargin)) |> 
        mutate(QBsingameFinalMargin = if_else(QBsingameTeam==colnames(gamedf)[9],
                                              as.numeric(gamedf[9][nrow(gamedf),]) -
                                                as.numeric(gamedf[8][nrow(gamedf),]),
                                              QBsingameFinalMargin))
        
      ###
      slctdf <- gamedf |> select(Year, gamenum, QBsingame, QBsingameScore, 
                                 QBsingameWinLoss, QBsingameTeam, QBsingameFinalMargin)
      nonadf <- na.omit(slctdf)
      uniqdf <- unique(nonadf)
      finaldflist <- list.append(finaldflist, uniqdf)
    }
    print(year)
  }
  print(team)
}

basefile <- finaldflist[[1]]
for(b in c(2:length(finaldflist))){
  basefile <- rbind(basefile, finaldflist[[b]])
  print(b)
}

basefile <- unique(basefile)

ggplot(data = basefile, aes(x = QBsingameWinLoss, y = QBsingameScore)) +
  geom_boxplot()

winfile <- basefile |> filter(QBsingameWinLoss=="W")
lossfile <- basefile |> filter(QBsingameWinLoss=="L")
mean(winfile$QBsingameScore)
mean(lossfile$QBsingameScore)

ggplot(data = basefile, aes (x = QBsingameScore, y = QBsingameFinalMargin)) + 
  geom_point()

mod <- lm(QBsingameFinalMargin ~ QBsingameScore, basefile)
summary(mod)


a = 0
b = 100

basefile$variable_scaled <- a + (basefile$QBsingameScore - 
                                   min(basefile$QBsingameScore)) * (b - a) / 
  (max(basefile$QBsingameScore) - min(basefile$QBsingameScore))




setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files")
saveRDS(basefile, "AaronQBGrades.rds")


# Making Plots of QB Career Scores ----------------------------------------
mydf <- readRDS("AaronQBGrades.rds")

### Making Scatterplot
fqbr_boxplot <- ggplot(data = mydf, aes(x = QBsingameWinLoss, y = variable_scaled, group = QBsingameWinLoss)) + 
  geom_boxplot() +
  labs(x = "Win or Loss",
       y = "FQBR") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Making Histogram
fqbr_hist <- ggplot(data = mydf, aes(x = variable_scaled)) +
  geom_histogram() + 
  labs(title = "Distribution of Friedlander Quarterback Rating (FQBR)",
       x = "FQBR") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#### Making Graphs for Grades vs. Wins
mymod <- lm(QBsingameFinalMargin ~ variable_scaled, data = mydf)
summary(mymod)

### FQBR Graph
fqbrplot <- ggplot(mydf, aes(x = variable_scaled, y = QBsingameFinalMargin)) +
  geom_point(size = 0.8, alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "FQBR Grade",
       y = "QB's Team's Point Diff.",
       title = "FQBR Grade vs. Quarterback's Team's Point Differential         "~ R^2 ~ "= .2329") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


fqbrplot + plot_annotation(
  caption = "Figure 9: FQBR Model"
) &
  theme(
    plot.caption = element_text(size = 12, hjust = 0.5)  # Adjust size and alignment
  )

####

combdf <- readRDS("QBTeamYearWeek.rds")

qbrankdf <- tibble(QB = rep(NA, 300), careerval = rep(NA, 300), 
                   avgGrade = rep(NA, 300), gamesplayed = rep(NA, 300))

### Fixing Derek Carr in combdf
combdf <- combdf |> 
  mutate(Player = if_else(Player=="Derek Carr ", "Derek Carr", Player))
###

playerlist <- unique(mydf$QBsingame)
for(player in playerlist){
  playerdf <- mydf |> filter(QBsingame==player)
  if(isTRUE(player=="Alex Smith")){
    playerdf <- playerdf %>% filter(QBsingameTeam %in% c("SFO", "KAN", "WAS"))
  }
  playercombdf <- combdf |> filter(Player == player)
  minidflist <- list()
  for(r in c(1:nrow(playercombdf))){
    minidf <- playerdf |> filter(QBsingameTeam==playercombdf$Tm[r], 
                                 Year==playercombdf$Year[r])
    minidflist <- list.append(minidflist, minidf)
  }
  minifile <- minidflist[[1]]
  if(isTRUE(length(minidflist) > 1)){
    for(mf in c(2:length(minidflist))){
      minifile <- rbind(minifile, minidflist[[mf]])
    }
  }
  playerdf <- arrange(minifile, Year, gamenum)
  playerdf$playergamenum <- seq(1, nrow(playerdf))
  ###
  playerdf <- playerdf |> 
    mutate(var_scale_res = variable_scaled - 47.1)
  ###
  plusminus <- sum(playerdf$variable_scaled) / 10
  numkey <- which(playerlist == player)
  qbrankdf$QB[numkey] <- player
  qbrankdf$careerval[numkey] <- plusminus
  playermean <- mean(playerdf$variable_scaled)
  qbrankdf$avgGrade[numkey] <- playermean
  qbrankdf$gamesplayed[numkey] <- nrow(playerdf)
  tmlist <- unique(playerdf$QBsingameTeam)
  
  playerdf <- playerdf |> 
    mutate(QBsingameTeam = factor(QBsingameTeam, levels = unique(QBsingameTeam)))
  myplot <- ggplot(playerdf, aes(x = playergamenum, y = variable_scaled, color = QBsingameTeam)) + 
    geom_point() +
    geom_line() + 
    geom_hline(yintercept = 47.1) +
    geom_hline(yintercept = playermean, linetype = "dashed") +
    ylim(0, 100) +
    labs(title = paste0(player, " Career Game by Game FQBR"),
         subtitle = "Dashed Bar is QB's Average Rating, Solid Bar is League Average",
         x = "Game Number",
         y = "Friedlander Quarterback Rating (FQBR)",
         color = "Team" ) +
    scale_color_manual(values = c("ARI" = "#97233F", "ATL" = "#A71930",
                                  "BAL" = "#241773", "BUF" = "#00338D",
                                  "CAR" = "#0085CA", "CHI" = "#0B162A",
                                  "CIN" = "#FB4F14", "CLE" = "#311D00",
                                  "DAL" = "#041E42", "DEN" = "#FB4F14", 
                                  "DET" = "#0076B6", "GNB" = "#203731",
                                  "HOU" = "#03202F", "IND" = "#002C5F",
                                  "JAX" = "#9F792C", "KAN" = "#E31837",
                                  "LAC" = "#0072CE", "SDG" = "#0072CE",
                                  "LAR" = "#B3995D", "STL" = "#B3995D",
                                  "LVR" = "#A5ACAF", "OAK" = "#A5ACAF",
                                  "MIA" = "#008E97", "MIN" = "#4F2683",
                                  "NOR" = "#D3BC8D", "NWE" = "#002244",
                                  "NYG" = "#0B2265", "NYJ" = "#203731",
                                  "PHI" = "#004C54", "PIT" = "#FFB81C",
                                  "SEA" = "#69BE28", "SFO" = "#AA0000",
                                  "TAM" = "#D50A0A", "TEN" = "#4B92DB",
                                  "WAS" = "#773141")) +
    theme_minimal()
  setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/QB AQR Graphs")
  filename <- paste0(player,".png")
  ggsave(filename = filename, plot = myplot, width = 7, height = 5)
  print(player)
}

qbrankdf <- na.omit(qbrankdf)
qbrankdf <- qbrankdf |> filter(QB != "Levi Brown")
qbrankdf <- qbrankdf |> filter(QB != "Matt Flynn")
qbrankdf <- qbrankdf |> filter(QB != "Kellen Clemens")
qbrankdf <- qbrankdf |> filter(QB != "Kyle Boller")

qualdf <- qbrankdf |> filter(gamesplayed>=30)

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/QB Ranks (AQR)")
saveRDS(qbrankdf, "AllQBRanks.rds")
saveRDS(qualdf, "QualQBRanks.rds")



# Clutch Plays ------------------------------------------------------------

dflist <- list()
teamlist <- list("crd","atl","rav","buf","car","chi","cin","cle","dal","den","det","gnb","htx","clt","jax","kan","ram","sdg","mia","min","nwe","nor","nyg","nyj","rai","phi","pit","sea","sfo","tam","oti","was")
for(team in teamlist){
  yearlist <- c(2006:2022)
  for(year in yearlist){
    setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 12.0/", team, "/", year))
    filelist <- list.files()
    for(file in filelist){
      setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs 12.0/", team, "/", year))
      filename <- file
      gamedf <- suppressMessages(read_csv(file, show_col_types = FALSE) |> select(-1))
      dflist <- list.append(dflist, gamedf)
    }
    print(year)
  }
  print(team)
}

basefile <- dflist[[1]] |> select(1:36)
colnames(basefile)[8] <- "Tm1Score"
colnames(basefile)[9] <- "Tm2Score"

for(b in c(2:length(dflist))){
  newfile <- dflist[[b]] |> select(1:36)
  colnames(newfile)[8] <- "Tm1Score"
  colnames(newfile)[9] <- "Tm2Score"
  basefile <- rbind(basefile, newfile)
  print(b)
}

saveRDS(basefile, "compileddffornov3.rds")

library(ggforce)
library(plotly)
library(reticulate)

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files")
mydf <- readRDS("compileddffornov3.rds")
mydf <- na.omit(mydf)

####
mydf <- mydf %>%
  separate(Time, into = c("Minutes", "Seconds", "ToDelete"), sep = ":")
mydf <- mydf |> select(-ToDelete)
mydf$Minutes <- as.numeric(mydf$Minutes)
mydf$Seconds <- as.numeric(mydf$Seconds)
mydf <- mydf |> 
  mutate(Timeleft = case_when(
    Quarter==1 ~ 2700 + 60*Minutes + Seconds,
    Quarter==2 ~ 1800 + 60*Minutes + Seconds,
    Quarter==3 ~ 900 + 60*Minutes + Seconds,
    Quarter==4 ~ 60*Minutes + Seconds,
  )) |> 
  mutate(Margin = abs(Tm1Score-Tm2Score))
####

mydf <- mydf |> 
  mutate(QBsingameTeam = Possession)
mydf$finalscaledZ <- scale(mydf$scaledZScores, center = TRUE, scale = FALSE)
mydf <- mydf |> 
  mutate(finalZscore = finalscaledZ / 4.05)
qbsdf <- readRDS("AaronQBGrades.rds")

playerlist <- unique(mydf$QBsingame)
playerlist <- na.omit(playerlist)

ggplot(mydf, aes(x = finalZscore)) + 
  geom_histogram()


# Define the path to the parent directory
# parent_directory <- "C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/QB Clutch Graphs"
# 
# # Create subfolders within the parent directory
# for (folder_name in playerlist) {
#   # Define the full path for each subfolder
#   subfolder_path <- file.path(parent_directory, folder_name)
#   
#   # Create the subfolder
#   dir.create(subfolder_path, recursive = TRUE, showWarnings = FALSE)
# }

allqbsdf <- tibble(Player = rep(NA, 500), totalovrvalueadded = rep(NA, 500),
                       avgovrvalueadded = rep(NA, 500),
                       totalclutchvalueadded = rep(NA, 500), 
                       avgclutchvalueadded = rep(NA, 500))
for(player in playerlist){
  playerdf <- mydf |> filter(QBsingame==player)
  if(isTRUE(player=="Alex Smith")){
    playerdf <- playerdf %>% filter(QBsingameTeam %in% c("SFO", "KAN", "WAS"))
  }
  unique_combinations <- playerdf %>% distinct(Year, gamenum)
  playerdf <- unique(playerdf)
  playervaluedf <- tibble(Year = rep(NA, 500), gamenum = rep(NA, 500), 
                          Tm = rep(NA, 500),
                     opponent = rep(NA, 500), ovrvalueadded = rep(NA, 500),
                     clutchvalueadded = rep(NA, 500))
  
  for(uniq in 1:nrow(unique_combinations)){
    combination <- unique_combinations[uniq,]
    year <- combination$Year
    gmnum <- combination$gamenum
    newdf <- playerdf |> filter(Year==year, gamenum==gmnum)
    opp <- if_else(newdf$Possession[1]==newdf$Team1Name[1], newdf$Team2Name[1],
                   newdf$Team1Name[1])
    myteam <- newdf$Possession[1]
    newdf$playnum <- seq(1, nrow(newdf))
    newdf$highlight <- newdf$Timeleft <= 240 &
      newdf$Margin <= 8
    ovradded <- round(sum(newdf$finalZscore), 2)
    clutchdf <- newdf |> filter(Timeleft <= 240, Margin <= 8)
    clutchadded <- round(sum(clutchdf$finalZscore), 2)
    p <- ggplot(newdf, aes(x = playnum, y = finalZscore, text = Detail)) + 
      geom_point(aes(color = highlight)) +
      scale_color_manual(values = c("black", "red")) +
      ylim(-4.5, 4.5) +
      geom_hline(yintercept = 0) +
      labs(color = "Crucial Play",
           x = "QB Play Count in the Game",
           y = "Play Value",
           title = paste0(player,": ", myteam, " vs. ", opp, " ", year, " Gm ", gmnum),
           subtitle = paste0("Total Value Added: ", ovradded, ", Value on Clutch Plays: ",
                             clutchadded)) +
      theme_minimal()
    setwd(paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/QB Clutch Graphs/", player))
    filename <- paste0(myteam, " vs. ", opp, " ", year, " Gm ", gmnum, ".png")
    ggsave(filename = filename, plot = p, width = 7, height = 5)
    playervaluedf$Year[uniq] <- year
    playervaluedf$gamenum[uniq] <- gmnum
    playervaluedf$Tm[uniq] <- myteam
    playervaluedf$opponent[uniq] <- opp
    playervaluedf$ovrvalueadded[uniq] <- ovradded
    playervaluedf$clutchvalueadded[uniq] <- clutchadded
  }
  playervaluedf$Player <- player
  playervaluedf <- playervaluedf |> relocate(Player, .before = Year)
  playervaluedf <- na.omit(playervaluedf)
  write.csv(playervaluedf, "Valuechart.csv")
  key <- which(playerlist==player)
  allqbsdf$Player[key] <- player
  allqbsdf$totalovrvalueadded[key] <- sum(playervaluedf$ovrvalueadded)
  allqbsdf$avgovrvalueadded[key] <- mean(playervaluedf$ovrvalueadded)
  allqbsdf$totalclutchvalueadded[key] <- sum(playervaluedf$clutchvalueadded)
  allqbsdf$avgclutchvalueadded[key] <- mean(playervaluedf$clutchvalueadded)
  print(player)
}

allqbsdf1 <- na.omit(allqbsdf)
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files")
saveRDS(allqbsdf1, "AllQBs.rds")

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files")
mydf <- readRDS("AllQBs.rds")
mydf <- mydf |> 
  mutate(GamesPlayed = totalovrvalueadded / avgovrvalueadded)

qbrankdf <- mydf |> filter(GamesPlayed >= 30)
qbrankdf <- qbrankdf |> filter(Player != "Levi Brown")
qbrankdf <- qbrankdf |> filter(Player != "Matt Flynn")
qbrankdf <- qbrankdf |> filter(Player != "Kellen Clemens")
qbrankdf <- qbrankdf |> filter(Player != "Kyle Boller")


### FQR Ranks
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/QB Ranks (AQR)")
newdf <- readRDS("QualQBRanks.rds")



### Adding figure names to Player Evaluation Graphs
Mahomes <- readPNG("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/QB FQBR Graphs/Patrick Mahomes.png")
