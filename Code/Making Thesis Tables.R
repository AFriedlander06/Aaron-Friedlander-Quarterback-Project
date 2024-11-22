
library(tidyverse)
library(flextable)
library(officer)



### Initial Gamelog Table
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Gamelogs/den/2022")
mydf <- read_csv("Game 1.csv") |> select(-1) |> select(-EPB)
mydf$Year <- as.character(mydf$Year)
detdf <- mydf |> select(Detail)
mydf <- mydf[1:7,] |> select(-EPA) |> select(-Detail)

ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 1: Initial Game Log Data Table")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

detdf <- detdf[1:2,]
detdf <- as.matrix(detdf)
detdf <- t(detdf)
detdf <- as.data.frame(detdf)
detdf$name <- "Detail"
detdf <- detdf |> relocate(name, .before = V1)

ft2 <- flextable(detdf)
ft2 <- border(ft2, part = "all", border = fp_border_default(color = "black", width = 1))
ft2 <- width(ft2, j = "V2", width = 20)
ft2 <- bg(ft2, i = seq(1, nrow(detdf), by = 1), bg = "gray90")
ft2 <- bold(ft2, j = "name", bold = TRUE)
ft2


### Table 2: Synopsis of variable functions
mydf <- tibble(Variable = c("gamenum", "Year", "Quarter", "Time", "Down", "ToGo",
                            "Location", "DEN", "SEA", "Detail"),
               Function = c("Refers to the game number of the team whose folder the game log came from",
                            "Indicates the Year (Season) in which the game was played",
                            "Indicates the Quarter (1, 2, 3, 4, or OT) in which the play occurs",
                            "Indicates the Time left in the game before the play",
                            "Indicates the Down (1, 2, 3, or 4) before the play",
                            "Indicates the yards needed for a first down before the play",
                            "Indicates what yard line the ball is on before the play",
                            "The Away Team. Can be any of the 32 NFL teams (in the case above: DEN)",
                            "The Home Team. Can be any of the 32 NFL teams (in the case above: SEA",
                            "Explains what happened during the play"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 2: The Functions of the Game Log Variables")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft <- italic(ft, j = "Variable", italic = TRUE)
ft

### Table 3: Sample of Player Team Year Week
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project")
mydf <- read_csv("NewTeamPlayerWeek.csv")
mydf <- mydf[1:5,]
mydf$Year <- as.character(mydf$Year)
mydf$LowWeek <- as.character(mydf$LowWeek)
mydf$UpWeek <- as.character(mydf$UpWeek)
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 3: A Sample of my NFL Player Database")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Key: The abbreviated names for NFL teams
mydf <- tibble(`Full Name` = c("Baltimore Ravens", "Pittsburgh Steelers",
                               "Cleveland Browns", "Cincinnati Bengals",
                               "New England Patriots", "Buffalo Bills",
                               "Miami Dolphins", "New York Jets",
                               "Tennessee Titans", "Houston Texans",
                               "Jacksonville Jaguars", "Indianapolis Colts",
                               "Kansas City Chiefs", "Denver Broncos",
                               "San Diego Chargers", "Los Angeles Chargers",
                               "Oakland Raiders", "Las Vegas Raiders",
                               "Green Bay Packers", "Detroit Lions",
                               "Minnesota Vikings", "Chicago Bears",
                               "Philadelphia Eagles", "Dallas Cowboys",
                               "Washington Commanders", "New York Giants",
                               "New Orleans Saints", "Atlanta Falcons",
                               "Tampa Bay Buccaneers", "Carolina Panthers",
                               "Seattle Seahawks", "San Francisco 49ers",
                               "Arizona Cardinals", "St. Louis Rams",
                               "Los Angeles Rams"),
               `Abbreviated Name` = c("BAL", "PIT", "CLE", "CIN", "NWE", "BUF",
                                      "MIA", "NYJ", "TEN", "HOU", "JAX", "IND",
                                      "KAN", "DEN", "SDG", "LAC", "OAK", "LVR",
                                      "GNB", "DET", "MIN", "CHI", "PHI", "DAL",
                                      "WAS", "NYG", "NOR", "ATL", "TAM", "CAR",
                                      "SEA", "SFO", "ARI", "STL", "LAR"))
mydf <- arrange(mydf, `Full Name`)
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Key: The Abbreviated Names for NFL Teams")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Table 4: Game Info
setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/Game Info")
mydf <- read_csv("AllGameInfo.csv")
mydf <- mydf[1:5,][-1]
mydf$Year <- as.character(mydf$Year)
mydf <- mydf |> select(-Vegas.Line) |> select(-Over.Under)
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 4: Game Info Table Sample")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Table 5: Game Info Variable Functions
mydf <- tibble(Variable = c("FolderTm", "Year", "gamenum", "Won.Toss", "Roof",
                            "Surface", "Duration", "Attendance", "Weather",
                            "FavTeam", "PtSpread", "OvUnd", "OvUndResult"),
               Function = c("Designates which team's folder a game info csv should go to",
                            "Indicates which year (season) the game took place in",
                            "Indicates which game number the game represents for the given team",
                            "Indicates which team won the coin toss",
                            "Indicates what type of roof (outdoors or dome) the stadium had",
                            "Indicates what type of field surface (grass or turf) the stadium had",
                            "Indicates how long the game lasted",
                            "Indicates how many people attended the game",
                            "Indicates the weather (most do not have information for this)",
                            "Indicates which team was favored to win",
                            "Indicates how many points the favored team was expected to win by",
                            "Indicates the expected total score of the game",
                            "Ordinal variable for actual total score in relation to expected total score
                            (under, over, or push)"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 5: Game Info Variable Functions")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Table 6: First Down Probability Model Response Variables
mydf <- tibble(Variable = c("YdsGained", "Down", "ToGo", "BasePlayPass",
                            "BasePlayRun", "BasePlayST"),
               Function = c("Indicates yards gained on the play by the offense",
                            "Indicates which down (1, 2, 3, or 4) the offense faced before the play",
                            "Indicates how many yards the offense was away from the first down
                            marker before the play",
                            "Binary variable (0 if not a pass play, 1 if a pass play",
                            "Binary variable (0 if not a run play, 1 if a run play",
                            "Binary variable (0 if not a special teams play, 1 if a special
                            teams play"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 6: First Down Probability Model Response Variables")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Tables 7, 8, and 9: First Down Probability Model Results.
# Note: You have to run the logistic regression model in `Building the Models.R`
# to be able to create these data sets
mydf <- tibble("Dataset" = c("Training", "Test"), "yes_accuracy" = c(".746", ".756"),
               "no_accuracy" = c(".813", ".809"), "total_accuracy" = c(".767", ".773"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 7: First Down Probability Model Accuracy")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft1 <- ft
ft1

mydf <- as.data.frame(confusion_matrix)
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 8: Confusion Matrix for the Training Data")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft2 <- ft
ft2


mydf <- as.data.frame(confusion_matrix)
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 9: Confusion Matrix for the Test Data")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft3 <- ft
ft3

### Table 10: Scoring Probability Model Response Variables
mydf <- tibble(Variable = c("FavYdstoEZAft", "Quarter", "FDProb", "FGMakeProb",
               "FGAtt", "Turnover", "PuntonPlay", "FavPossession",
               "FavScoreonPlayInd", "FavPosAftPlay"),
               Function = c("Indicates the yards away from the end zone",
                            "Indicates the quarter (1, 2, 3, or 4)",
                            "The results from the previous model",
                            "Indicates the probability of making a field goal",
                            "Binary variable that indicates if a field goal is attempted",
                            "Binary variable that indicates if a turnover occurred",
                            "Binary variable that indicates if a punt occurred",
                            "Binary variable that indicates if the favored team has possession of the ball before the play",
                            "Variable that indicates if a team scored on the play",
                            "Binary variable that indicates if the favored team has possession of the ball after the play"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 10: Scoring Probability Model Response Variables")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Table 11: Winning Probability Model Response Variables
mydf <- tibble(Variable = c("NewPtSpread", "FavExpPts", "NonFavExpPts"),
               Function = c("Variable that combines point spread, the margin of the
                            favored team, and time remaining (Discussed more below)",
                            "Indicates the expected points for the favored team 
                            (created in previous model)",
                            "Indicates the expected points for the underdog team
                            (created in previous model)"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 11: Winning Probability Model Response Variables")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft


### Table 12: Win Probability Model Accuracy
mydf <- tibble("Dataset" = c("Training", "Test"), "yes_accuracy" = c(".795", ".805"),
               "no_accuracy" = c(".722", ".696"), "total_accuracy" = c(".777", ".780"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 12: Scoring Probability Model Response Variables")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Tables 13 and 14: Confusion Matrices for Win Prob Model
# Note: You have to run the logistic regression model in `Building the Models.R`
# to be able to create these data sets
mydf <- as.data.frame(confusion_matrix1)
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 13: Confusion Matrix for the Training Data")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

mydf <- as.data.frame(confusion_matrix2)
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 14: Confusion Matrix for the Test Data")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Table 15: Residuals of the Three Win Probability Models
mydf <- tibble("Residual Type" = c("Sum of Absolute Value", "Sum of Squared"),
               "My Model" = c(".857", ".011"),
               "PFR Model" = c("8.259", ".847"),
               "Vegas Model" = c(".656", ".007"))
ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 15: Residuals of the Three Win Probability Models")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

### Table 16: FQBR Rankings
mydf1 <- newdf
mydf <- mydf1 |> arrange(desc(avgGrade))
mydf <- mydf |> filter(QB != "Taysom Hill")
mydf <- mydf[1:10,]
mydf$careerval <- round(mydf$careerval, 2)
mydf$avgGrade <- round(mydf$avgGrade, 2)
mydf$careerval <- as.character(mydf$careerval)
mydf$avgGrade <- as.character(mydf$avgGrade)
mydf$gamesplayed <- as.character(mydf$gamesplayed)


ft <- flextable(mydf) |> 
  theme_vanilla() |> 
  autofit() |> 
  set_caption(caption = "Table 16: Ranking Quarterbacks by FQBR")
ft <- border(ft, part = "all", border = fp_border_default(color = "black", width = 1))
ft <- bg(ft, i = seq(1, nrow(mydf), by = 2), bg = "gray90")
ft

