
# CREATING MODEL FOR DOWNS RESULT----------------------------------------
### I doubt all of these are necessary, but I just copy them all for everything
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

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")

### The data set for the first logistic regression model for first downs
logmod1dat <- readRDS("LogModel1Data.rds")

### splits data into training and test data
mydftraining <- logmod1dat %>% filter(Year<=2022)
mydftest <- logmod1dat %>% filter(Year==2023)

### makes DownsResult a factor
mydftraining$DownsResult <- as.factor(mydftraining$DownsResult)
mydftest$DownsResult <- as.factor(mydftest$DownsResult)

### Creates the modle
testmodel <- glm(DownsResult ~ YdsGained + Down + ToGo + BasePlayPass +
                   BasePlayRun + BasePlayST
                 , family = "binomial", data = mydftraining)

# ggplot(logmod1dat, aes(x=YdsGained, y=DownsResult)) +
#   geom_point() +
#   geom_jitter()

modcoeffs <- tidy(testmodel)

logmod1dat$FDProb <- predict(testmodel, logmod1dat, type = "response")
mydftraining$FDProb <- predict(testmodel, mydftraining, type = "response")
mydftest$FDProb <- predict(testmodel, mydftest, type = "response")
summary(testmodel)

## Training accuracy
mydftraining$WinProb <- predict(testmodel, mydftest, type = "response")
mydftraining <- mydftraining %>% 
  mutate(accresult = case_when(
    FDProb>=0.5 & DownsResult=="Yes" ~ "TP",
    FDProb>=0.5 & DownsResult=="No" ~ "FP",
    FDProb<=0.5 & DownsResult=="No" ~ "TN",
    FDProb<=0.5 & DownsResult=="Yes" ~ "FN"
  ))

TP <- length(which(mydftraining$accresult == "TP"))
FP <- length(which(mydftraining$accresult == "FP"))
TN <- length(which(mydftraining$accresult == "TN"))
FN <- length(which(mydftraining$accresult == "FN"))

yesacc <- TP / (TP + FP)
noacc <- TN / (TN + FN)
acc<- (TP + TN) / (TP + FP + TN + FN)
yesacc
noacc
acc


### test accuracy
mydftest$WinProb <- predict(testmodel, mydftest, type = "response")
mydftest <- mydftest %>% 
  mutate(accresult = case_when(
    FDProb>=0.5 & DownsResult=="Yes" ~ "TP",
    FDProb>=0.5 & DownsResult=="No" ~ "FP",
    FDProb<=0.5 & DownsResult=="No" ~ "TN",
    FDProb<=0.5 & DownsResult=="Yes" ~ "FN"
  ))

TP <- length(which(mydftest$accresult == "TP"))
FP <- length(which(mydftest$accresult == "FP"))
TN <- length(which(mydftest$accresult == "TN"))
FN <- length(which(mydftest$accresult == "FN"))


yesacc <- TP / (TP + FP)
noacc <- TN / (TN + FN)
acc<- (TP + TN) / (TP + FP + TN + FN)
yesacc
noacc
acc



confusion_matrix <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE,
                           dimnames = list("Actual" = c("0", "1"), "Predicted" = c("0", "1")))
confusion_matrix


mydftraining <- mydftraining |> 
  mutate(DownsResult = if_else(DownsResult=="Yes", 1, 0))
mydftest <- mydftest |> 
  mutate(DownsResult = if_else(DownsResult=="Yes", 1, 0))

test_prob = predict(final_model, type = "prob",
                    new_data = mydftest)
test_class = predict(final_model, type = "class",
                     new_data = mydftest)


### Ask Dr. Patrick about ROC curve

####
m <- modcoeffs

### Creates first down prob for the test data using the coefficients from the
### model summary
mydftest <- mydftest %>% 
  mutate(MyFDProb = 1-((exp(-(m$estimate[1] + m$estimate[2]*YdsGained +
                                m$estimate[3]*Down + m$estimate[4]*ToGo +
                                m$estimate[5]*BasePlayPass + m$estimate[6]*BasePlayRun +
                                m$estimate[7]*BasePlayST)))/
                         (1 + (exp(-(m$estimate[1] + m$estimate[2]*YdsGained +
                                       m$estimate[3]*Down + m$estimate[4]*ToGo +
                                       m$estimate[5]*BasePlayPass + m$estimate[6]*BasePlayRun +
                                       m$estimate[7]*BasePlayST)))))
  )

mydf3 <- logmod1dat

### Creates variables for first down probability for all ydsgained -20:40
ydlist <- c(-100, -20:99)
for(yd in ydlist){
  mydf3 <- mydf3 %>% 
    mutate(YdsGnd = 1-((exp(-(m$estimate[1] + m$estimate[2]*yd +
                                m$estimate[3]*Down + m$estimate[4]*ToGo +
                                m$estimate[5]*BasePlayPass + m$estimate[6]*BasePlayRun +
                                m$estimate[7]*BasePlayST)))/
                         (1 + (exp(-(m$estimate[1] + m$estimate[2]*yd +
                                       m$estimate[3]*Down + m$estimate[4]*ToGo +
                                       m$estimate[5]*BasePlayPass + m$estimate[6]*BasePlayRun +
                                       m$estimate[7]*BasePlayST))))))
  mydf3 <- mydf3 |> 
    mutate(YdsGnd = if_else(yd >= ToGo, 1, YdsGnd))
  newyd <- as.character(yd)
  colnames(mydf3)[colnames(mydf3)=="YdsGnd"] <- paste0(newyd,"YdsGnd")
  print(yd)
}

mydf <- mydf3 |> 
  mutate(FDProb = if_else(YdsGained >= ToGo, 1, FDProb))

FDLogRegdf <- mydf3
saveRDS(FDLogRegdf, "FirstDownLogRegResults.rds")


# Creating Ordinal Model for Scoring -------------------------------------

### Dataset for ordinal regression model
ordregdf <- readRDS("FirstDownLogRegResults.rds")


ordregdf <- ordregdf |> 
  mutate(Tm1ScoreonPlay = if_else(Team1Name==lag(Team1Name) & Team2Name==lag(Team2Name) &
                                    Year==lag(Year) & Week==lag(Week) &
                                    !grepl("no play", Detail), Tm1-lag(Tm1),0)) |> 
  mutate(Tm2ScoreonPlay = if_else(Team1Name==lag(Team1Name) & Team2Name==lag(Team2Name) &
                                    Year==lag(Year) & Week==lag(Week) &
                                    !grepl("no play", Detail), Tm2-lag(Tm2),0))
ordregdf <- ordregdf |> 
  mutate(Tm1ScoreonPlay = if_else(Tm1ScoreonPlay==6 & lead(Tm1ScoreonPlay)==1, 7, Tm1ScoreonPlay)) |> 
  mutate(Tm2ScoreonPlay = if_else(Tm2ScoreonPlay==6 & lead(Tm2ScoreonPlay)==1, 7, Tm2ScoreonPlay))

ordregdf <- ordregdf |> 
  mutate(Tm1ScoreonPlay = if_else(Tm1ScoreonPlay==1, 0 , Tm1ScoreonPlay)) |> 
  mutate(Tm2ScoreonPlay = if_else(Tm2ScoreonPlay==1, 0 , Tm2ScoreonPlay))

ordregdf <- ordregdf |> 
  mutate(FavScoreonPlay = case_when(
    FavTeam==Team1Name & Tm1ScoreonPlay>0 ~ Tm1ScoreonPlay,
    FavTeam==Team1Name & Tm2ScoreonPlay>0 ~ -Tm2ScoreonPlay,
    FavTeam==Team2Name & Tm1ScoreonPlay>0 ~ -Tm1ScoreonPlay,
    FavTeam==Team2Name & Tm2ScoreonPlay>0 ~ Tm2ScoreonPlay,
    TRUE ~ NA
  ))

ordregdf <- ordregdf |> 
  mutate(EndDrive1 = if_else(Team1Name==lag(Team1Name) & Team2Name==lag(Team2Name) &
                               Year==lag(Year) & Week==lag(Week) & Possession!=lead(Possession) &
                               is.na(FavScoreonPlay),
                             "Yes", ""))

ordregdf <- ordregdf |> 
  mutate(FavScoreonPlay = if_else(FavScoreonPlay==4, 0, FavScoreonPlay)) |>
  mutate(FavScoreonPlay = if_else(FavScoreonPlay==13, 0, FavScoreonPlay)) |> 
  mutate(FavScoreonPlay = if_else(FavScoreonPlay==14, 0, FavScoreonPlay)) |> 
  mutate(FavScoreonPlay = if_else(FavScoreonPlay==9, 0, FavScoreonPlay)) |> 
  mutate(FavScoreonPlay = if_else(FavScoreonPlay==-4, 0, FavScoreonPlay)) |> 
  mutate(FavScoreonPlay = if_else(FavScoreonPlay==10, 0, FavScoreonPlay))

ordregdf <- ordregdf |> 
  mutate(FavScoreonPlay = if_else(EndDrive1=="Yes", 0, FavScoreonPlay)) |> 
  mutate(FavScoreonPlay1 = FavScoreonPlay)

ordregdf <- ordregdf |> fill(FavScoreonPlay1, .direction = "up")
ordregdf <- ordregdf |> select(-EndDrive)

ordregdf <- ordregdf |> 
  mutate(FavScoreonPlay = if_else(is.na(FavScoreonPlay), 0, FavScoreonPlay))
colnames(ordregdf)[colnames(ordregdf)=="FavScoreonPlay1"] <- "FavScoreonDrive"


ordregdf <- ordregdf |> 
  mutate(FavScoreonDrive = if_else(is.na(FavScoreonDrive), 0 , FavScoreonDrive))

ordregdf <- ordregdf |> 
  mutate(FavScoreonPlay = if_else(is.na(FavScoreonPlay), 0, FavScoreonPlay))
ordregdf <- ordregdf |> 
  mutate(Turnover = if_else(LostFumble=="Yes" | Intercpetion=="Yes", 1, 0)) |> 
  mutate(ScoreonPlay = if_else(FavScoreonPlay!=0,1,0)) |> 
  mutate(FavPossession = if_else(FavTeam==Possession,1,-1))
### Making YdstoEZAft in terms of FavTeam
# ordregdf <- ordregdf |>
#   mutate(YdstoEZAft = if_else(YdstoEZAft==0, 100, YdstoEZAft))

ordregdf <- ordregdf |> 
  mutate(FavYdstoEZAft = if_else(Possession!=FavTeam & PuntonPlay==0 & 
                               Turnover==0 , 100 - YdstoEZAft, YdstoEZAft)) |> 
  mutate(FavYdstoEZAft = if_else(grepl("ield goal no good", Detail), 100 - FavYdstoEZAft,
                                 FavYdstoEZAft)) |> 
  mutate(FavYdstoEZAft = if_else(Possession==FavTeam & Turnover==1, 100 - FavYdstoEZAft, FavYdstoEZAft))

ordregdf <- ordregdf |> 
  mutate(FavPosAftPlay = if_else(Team1Name==lead(Team1Name) & Team2Name==lead(Team2Name) &
                                   Year==lead(Year) & Week==lead(Week) 
                                 & ScoreonPlay==0
                                 , lead(FavPossession), FavPossession))
ordregdf$FavPosAftPlay[length(ordregdf$FavPosAftPlay)] <- -1

ordregdf <- ordregdf |> 
  mutate(FavScoreonPlayInd = case_when(
    FavScoreonPlay>0 ~ 1,
    FavScoreonPlay<0 ~ -1,
    FavScoreonPlay==0 ~ 0
  ))

ordregdf <- ordregdf |> 
  mutate(FavScoreonDrive = if_else(FavScoreonDrive>=6, 7, FavScoreonDrive)) |> 
  mutate(FavScoreonDrive = if_else(FavScoreonDrive<=-6, -7, FavScoreonDrive))
### Makes ScoreonDrive a factor
ordregdf$FavScoreonDrive <- as.factor(ordregdf$FavScoreonDrive)






### Additional necessary packages
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(sure)
### Creates the ordinal model
testmod <- polr(FavScoreonDrive ~ FavYdstoEZAft + Quarter + FDProb +
                  FGMakeProb + FGAtt + Turnover + PuntonPlay + FavPossession +
                  FavScoreonPlayInd + FavPosAftPlay, data = ordregdf, Hess = TRUE)


### Code I got from ChatGPT to get coefficients, intercepts, and residuals
coefficients <- coef(testmod)
intercepts <- testmod$zeta

res <- resids(testmod)
ordregdf$residuals <- res

### Graph of Residuals (Takes a bit to run)
# ggplot(ordregdf, aes(x = ScoreonDrive, y = residuals)) +
#   geom_point() +
#   labs(title = "Scatterplot of X vs Y", x = "X-Axis", y = "Y-Axis")

summary(testmod)
m<- tidy(testmod)

### Creates table of probabilities to bind to main dataframe
testvals <- predict(testmod, type = "probs")
valtable <- as.data.frame(testvals)
ordf <- cbind(ordregdf, valtable)

### Renames columns to be more understandable
colnames(ordf)[colnames(ordf)==".pred_No"] <- "pred_No(First Down)"
colnames(ordf)[colnames(ordf)==".pred_Yes"] <- "pred_Yes(First Down)"
colnames(ordf)[colnames(ordf)=="-7"] <- "pred_neg7"
colnames(ordf)[colnames(ordf)=="-3"] <- "pred_neg3"
colnames(ordf)[colnames(ordf)=="-2"] <- "pred_neg2"
colnames(ordf)[colnames(ordf)=="0"] <- "pred_0"
colnames(ordf)[colnames(ordf)=="2"] <- "pred_2"
colnames(ordf)[colnames(ordf)=="3"] <- "pred_3"
colnames(ordf)[colnames(ordf)=="7"] <- "pred_7"


### Creates variables that measure Expected Points and the Probability of Scoring
finalordf <- ordf
# finalordf <- finalordf %>% 
#   mutate(FavExpPts = case_when(
#     FavTeam==Possession ~ 7*pred_7 + 3*pred_3 - 2*pred_neg2 - 7*pred_neg7,
#     FavTeam!=Possession ~ -7*pred_neg7  + -3*pred_neg3 + 2*pred_2 + 7*pred_7)) %>%
#   mutate(pred_Score = 1-pred_0)

finalordf <- finalordf |> 
  mutate(FavExpPts = 7*pred_7 + 3*pred_3 + 2*pred_2) |> 
  mutate(NonFavExpPts = 7*pred_neg7 + 3*pred_neg3 + 2*pred_neg2)

finalordf <- finalordf |> 
  mutate(pred_Score = 1-pred_0)

### Calculates the accuracy of the model
finalordf <- finalordf %>% 
  mutate(oracc = case_when(
    ScoreonDrive>0 & pred_Score>=.5 ~ 1,
    ScoreonDrive>0 & pred_Score<.5 ~ 0,
    ScoreonDrive==0 & pred_Score>=.5 ~ 0,
    ScoreonDrive==0 & pred_Score<.5 ~ 1
  )
  )
finalordf1 <- na.omit(finalordf)
sum(finalordf1$oracc)/length(finalordf$gamenum)


### Writes a csv of data (First one) and estimates (Second one) in order to 
### restart R to fix packages
saveRDS(finalordf, "HalfORdf.rds")
saveRDS(m, "OREstimates.rds")


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

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")


testdf <- readRDS("HalfORdf.rds")
m <- readRDS("OREstimates.rds")
ydlist <- c(-100, -20:99)
for(yd in ydlist){
  mycolname <- paste0(yd,"YdsGnd")
  colkey <- which(names(testdf)==mycolname)
  testdf <- testdf %>% 
    mutate(testFavYdstoEZAft = if_else(Possession==FavTeam, YdstoEZBef - yd,
                                       (100 - YdstoEZBef) + yd)) |> 
    mutate(dummyScoreonPlay = case_when(
      Possession==FavTeam & yd>=YdstoEZBef ~ 1,
      Possession!=FavTeam & yd>=YdstoEZBef ~ -1,
      TRUE ~ 0
    ))
  
  testdf <- testdf |> 
    mutate(dummyFavPosAftPlay = FavPossession) |> 
    mutate(testFavYdstoEZAft = if_else(testFavYdstoEZAft>=100, 100, testFavYdstoEZAft)) |> 
    mutate(testFavYdstoEZAft = if_else(testFavYdstoEZAft<=1, 1, testFavYdstoEZAft))
             
  
  testdf <- testdf %>% 
    mutate(testPred_neg7 = 1/(1+exp(-(m$estimate[11] -  m$estimate[1]*testFavYdstoEZAft -
                                     m$estimate[2]*Quarter - m$estimate[3]*testdf[[colkey]] -
                                     m$estimate[4]*FGMakeProb - m$estimate[5]*FGAtt -
                                     m$estimate[8]*FavPossession - m$estimate[9]*dummyScoreonPlay -
                                     m$estimate[10]*dummyFavPosAftPlay
                                     
    )))) |>  
    mutate(testPred_neg3 = (1/(1+exp(-(m$estimate[12] -  m$estimate[1]*testFavYdstoEZAft -
                                      m$estimate[2]*Quarter - m$estimate[3]*testdf[[colkey]] -
                                      m$estimate[4]*FGMakeProb - m$estimate[5]*FGAtt -
                                      m$estimate[8]*FavPossession - m$estimate[9]*dummyScoreonPlay -
                                      m$estimate[10]*dummyFavPosAftPlay))
                                    
    )) - testPred_neg7) |> 
    mutate(testPred_neg2 = (1/(1+exp(-(m$estimate[13] -  m$estimate[1]*testFavYdstoEZAft -
                                      m$estimate[2]*Quarter - m$estimate[3]*testdf[[colkey]] -
                                      m$estimate[4]*FGMakeProb - m$estimate[5]*FGAtt -
                                      m$estimate[8]*FavPossession - m$estimate[9]*dummyScoreonPlay -
                                      m$estimate[10]*dummyFavPosAftPlay))
                                    
    )) - testPred_neg7 - testPred_neg3) |> 
    mutate(testPred_0 = (1/(1+exp(-(m$estimate[14] -  m$estimate[1]*testFavYdstoEZAft -
                                      m$estimate[2]*Quarter - m$estimate[3]*testdf[[colkey]] -
                                      m$estimate[4]*FGMakeProb - m$estimate[5]*FGAtt -
                                      m$estimate[8]*FavPossession - m$estimate[9]*dummyScoreonPlay -
                                      m$estimate[10]*dummyFavPosAftPlay))
                                    
    )) - testPred_neg7 - testPred_neg3 - testPred_neg2) |> 
    mutate(testPred_2 = (1/(1+exp(-(m$estimate[15] -  m$estimate[1]*testFavYdstoEZAft -
                                     m$estimate[2]*Quarter - m$estimate[3]*testdf[[colkey]] -
                                     m$estimate[4]*FGMakeProb - m$estimate[5]*FGAtt -
                                     m$estimate[8]*FavPossession - m$estimate[9]*dummyScoreonPlay -
                                     m$estimate[10]*dummyFavPosAftPlay))
                                   
    )) - testPred_neg7 - testPred_neg3 - testPred_neg2 - testPred_0) |> 
    mutate(testPred_3 = (1/(1+exp(-(m$estimate[16] -  m$estimate[1]*testFavYdstoEZAft -
                                     m$estimate[2]*Quarter - m$estimate[3]*testdf[[colkey]] -
                                     m$estimate[4]*FGMakeProb - m$estimate[5]*FGAtt -
                                     m$estimate[8]*FavPossession - m$estimate[9]*dummyScoreonPlay -
                                     m$estimate[10]*dummyFavPosAftPlay))
                                   
    )) - testPred_neg7 - testPred_neg3 - testPred_neg2 - testPred_0 - testPred_2)
  
  
  testdf <- testdf |> 
    mutate(testPred_7 = 1 - (testPred_neg7 + testPred_neg3 + testPred_neg2 +
                               testPred_0 + testPred_2 + testPred_3))

  # testdf <- testdf |> 
  #   mutate(testExpPts = case_when(
  #     Possession==FavTeam ~ 7*testPred_7 + 3*testPred_3 - 2*testPred_neg2 - 7*testPred_neg7,
  #     FavTeam!=Possession ~ -7*testPred_neg7  + -3*testPred_neg3 + 2*testPred_2 + 7*testPred_7
  #   )) 
  
  testdf <- testdf |> 
    mutate(testFavExpPts = 7*testPred_7 + 3*testPred_3 + 2*testPred_2) |> 
    mutate(testNonFavExpPts = 7*testPred_neg7 + 3*testPred_neg3 + 2*testPred_neg2)
  
  myyd <- as.character(yd)
  colnames(testdf)[colnames(testdf)=="testFavExpPts"] <- paste0(myyd,"FavExpPts")
  colnames(testdf)[colnames(testdf)=="testNonFavExpPts"] <- paste0(myyd,"NonFavExpPts")
  testdf <- testdf |> select(-testPred_neg7)
  testdf <- testdf |> select(-testPred_neg3)
  testdf <- testdf |> select(-testPred_neg2)
  testdf <- testdf |> select(-testPred_0)
  testdf <- testdf |> select(-testPred_2)
  testdf <- testdf |> select(-testPred_3)
  testdf <- testdf |> select(-testPred_7)
  testdf <- testdf |> select(-testFavYdstoEZAft)
  testdf <- testdf |> select(-dummyScoreonPlay)
  testdf <- testdf |> select(-dummyFavPosAftPlay)
  print(yd)
}

saveRDS(testdf, "ORModelResults.rds")




# Final Tier (Logistic Regression for Win Probability) --------------------

finallogregdf <- readRDS("ORModelResults.rds")

finallogregdf <- finallogregdf |> 
  mutate(Seconds = case_when(
    Quarter == 1 ~ Time2 + 2700,
    Quarter == 2 ~ Time2 + 1800,
    Quarter == 3 ~ Time2 + 900,
    Quarter >= 4 ~ Time2
  ))

### Setting up Variables for the model
finallogregdf <- finallogregdf %>% 
  mutate(FavWinLoss = if_else(FavTeam==Team1Name,Tm1WinLoss,Tm2WinLoss)) %>% 
  mutate(FavWinLoss = if_else(FavWinLoss=="L",0,1)) %>% 
  mutate(FavTmMargin = if_else(FavTeam==Team1Name,Tm1-Tm2,Tm2-Tm1)) %>% 
  mutate(Seconds = if_else(Seconds<=1, 2, Seconds)) 


begtest <- finallogregdf %>% filter(YdstoEZBef==75) %>% filter(Down==1) %>% filter(Quarter==1) %>% 
  filter(Seconds>=3550) %>% filter(YdsGained>=2) %>% filter(YdsGained<=8)
begtest <- begtest |> 
  mutate(absExpPts = (FavExpPts + NonFavExpPts))

myExpPts <- mean(begtest$absExpPts)

finallogregdf <- finallogregdf |> 
  mutate(NewSeconds = Seconds / 3600) |> 
  mutate(NewPtSpread = (PtSpread*NewSeconds) + (FavTmMargin))



### Creating training and test data
mydftraining <- finallogregdf %>% filter(Year<=2022)
mydftest <- finallogregdf %>% filter(Year>=2023)

mydftraining$FavWinLoss <- as.factor(mydftraining$FavWinLoss)
mydftest$FavWinLoss <- as.factor(mydftest$FavWinLoss)

testmodel <- glm(FavWinLoss ~ NewPtSpread + FavExpPts + NonFavExpPts,
                 family = "binomial", data = mydftraining)

### Estimates
modcoeffs <- tidy(testmodel)
summary(testmodel)

### Creating my own confusion matrix for training data
mydftraining$WinProb <- predict(testmodel, mydftraining, type = "response")
mydftraining <- mydftraining %>% 
  mutate(accresult = case_when(
    WinProb>=0.5 & FavWinLoss==1 ~ "TP",
    WinProb>=0.5 & FavWinLoss==0 ~ "FP",
    WinProb<=0.5 & FavWinLoss==0 ~ "TN",
    WinProb<=0.5 & FavWinLoss==1 ~ "FN"
  ))

TP <- length(which(mydftraining$accresult == "TP"))
FP <- length(which(mydftraining$accresult == "FP"))
TN <- length(which(mydftraining$accresult == "TN"))
FN <- length(which(mydftraining$accresult == "FN"))


confusion_matrix1 <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE,
                           dimnames = list("Actual" = c("0", "1"), "Predicted" = c("0", "1")))
print(confusion_matrix1)



### Creating my own confusion matrix for test data
mydftest$WinProb <- predict(testmodel, mydftest, type = "response")
mydftest <- mydftest %>% 
  mutate(accresult = case_when(
    WinProb>=0.5 & FavWinLoss==1 ~ "TP",
    WinProb>=0.5 & FavWinLoss==0 ~ "FP",
    WinProb<=0.5 & FavWinLoss==0 ~ "TN",
    WinProb<=0.5 & FavWinLoss==1 ~ "FN"
  ))

TP <- length(which(mydftest$accresult == "TP"))
FP <- length(which(mydftest$accresult == "FP"))
TN <- length(which(mydftest$accresult == "TN"))
FN <- length(which(mydftest$accresult == "FN"))

yesacc <- TP / (TP + FP)
noacc <- TN / (TN + FN)
acc<- (TP + TN) / (TP + FP + TN + FN)
yesacc
noacc
acc



confusion_matrix2 <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE,
                           dimnames = list("Actual" = c("0", "1"), "Predicted" = c("0", "1")))
print(confusion_matrix2)

finallogregdf$WinProb <- predict(testmodel, finallogregdf, type = "response")

results <- finallogregdf
colnames(results)[colnames(results)=="WinProb"] <- "FavWinProb"

### Histogram of Win Probs
histogram(results$FavWinProb)

### For the actual Thesis in code script `Creating Dataset for Vegas Model`
plot1 <- ggplot(results, aes(x = FavWinProb)) + 
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  labs(x = "Win Probability",
       y = "Frequency",
       title = "Frequencies of probabilities of the Favored Team Winning (My Model)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman"),
    axis.title.y = element_text(size = 12, family = "Times New Roman")
  )


### Calculates the Residuals
p1 <- 0
p2 <- .01
list1 <- c()
list2 <- c()
list3 <- c()
list4 <- c()
list5 <- c()
while(p1<.99999){
  p1 <- round(p1, 2)
  p2 <- round(p2, 2)
  x1 <- results %>% filter(FavWinProb>=p1&FavWinProb<p2)
  prob1 <- length(which(x1$FavWinLoss==1))/length(x1$FavWinLoss)
  dif1 <- (p2+p1)/2 - prob1
  name1 <- paste0(p1,"-",p2)
  list1 <- list.append(list1, name1)
  list2 <- list.append(list2, prob1)
  list3 <- list.append(list3, dif1)
  print(p1)
  p1 <- p1 + .01
  p2 <- p2 + .01
}

probdf <- tibble(WP = list1, MyModel = list2, MyModelres = list3)
probdf$newx <- seq(0, 0.99, by = 0.01)
plot(probdf$MyModelres)

### plot for thesis and `Creating Dataset for Vegas Models.R`
plot4 <- ggplot(probdf, aes(x = newx, y = MyModelres)) + 
  geom_point(color = "royalblue") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-.2, .2) + 
  labs(x = "Win Probability",
       y = "Model Residuals",
       title = "Residuals for Win Probability Model (My Model)") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
plot4
  
###

probdf <- probdf |> 
  mutate(abs_res = abs(MyModelres)) |> 
  mutate(sq_res = MyModelres^2)
sum(probdf$abs_res)
sum(probdf$sq_res)


### Adding WPB (Win Probability Before) and WPA (Win Probability After)
mydf <- results
mydf$FavWPA <- mydf$FavWinProb
mydf <- mydf %>% 
  mutate(FavWPB = if_else(Year==lag(Year) & gamenum==lag(gamenum) & Team1Name==lag(Team1Name) &
                            Team2Name==lag(Team2Name), lag(FavWPA),NA))

####
mydf <- mydf |> relocate(Detail, .before = FavWinProb)
mydf <- mydf |> relocate(NonFavExpPts, .before = Detail)
mydf <- mydf |> relocate(FavExpPts, .before = NonFavExpPts)
####

m <- modcoeffs

for(n in (1:nrow(mydf))){
  if(isTRUE(is.na(mydf$FavWPB[n]))){
    dummy_df <- data.frame(NewPtSpread = mydf$PtSpread[n],
                           FavExpPts = if_else(mydf$FavTeam[n]==mydf$Possession[n],myExpPts,0.5), 
                           NonFavExpPts = if_else(mydf$FavTeam[n]!=mydf$Possession[n],myExpPts,0.5))
    test_logit <- predict(testmodel, newdata = dummy_df, type = "link")
    myprob <- 1 / (1 + exp(-test_logit[1]))
    mydf$FavWPB[n] <- myprob
    print(n)
  }
}

### Creating a variable for Win Probability Added/Difference
mydf <- mydf %>% 
  mutate(FavWPDiff = FavWPA-FavWPB)
mydf$FavWPDiff <- round(mydf$FavWPDiff, 6)

mydf <- mydf |> relocate(Detail, .after = FavExpPts)
mydf <- mydf |> relocate(FavWPB, .before = FavWPA)
mydf <- mydf |> relocate(FavTmMargin, .before = Detail)

testdf <- mydf |> filter(PuntonPlay==0) |> filter(!grepl("kneel", Detail)) |> 
  filter(!grepl("spike", Detail))

##############
for(yd in c(-100, -20:99)){
  myyd <- as.character(yd)
  Favexp <- paste0(myyd, "FavExpPts")
  Favcolkey <- which(names(testdf)==Favexp)
  NonFavexp <- paste0(myyd, "NonFavExpPts")
  NonFavcolkey <- which(names(testdf)==NonFavexp)
  testdf <- testdf |> 
    mutate(dummyFavTmMargin = if_else(Team1Name==lag(Team1Name) & Team2Name==lag(Team2Name) &
                                        Year==lag(Year) & Week==lag(Week) & 
                                        yd>=YdstoEZBef & Possession==FavTeam, 
                                      lag(FavTmMargin)+7, lag(FavTmMargin)
                                      )) |> 
    mutate(dummyFavTmMargin = if_else(Team1Name==lag(Team1Name) & Team2Name==lag(Team2Name) &
                                        Year==lag(Year) & Week==lag(Week) & 
                                        yd>=YdstoEZBef & Possession!=FavTeam, 
                                      lag(FavTmMargin)-7, lag(FavTmMargin)
    )) |> 
    mutate(dummyFavTmMargin = if_else(is.na(dummyFavTmMargin),0, dummyFavTmMargin))
  testdf <- testdf %>%
    mutate(finalFavExpPts = testdf[[Favcolkey]]) |> 
    mutate(finalNonFavExpPts = testdf[[NonFavcolkey]]) |> 
    mutate(finalPtSpread = PtSpread*NewSeconds + dummyFavTmMargin)
 
  testdf1 <- testdf |> select(finalPtSpread, finalFavExpPts, finalNonFavExpPts)
  colnames(testdf1)[colnames(testdf1)=="finalPtSpread"] <- "NewPtSpread"
  colnames(testdf1)[colnames(testdf1)=="finalFavExpPts"] <- "FavExpPts"
  colnames(testdf1)[colnames(testdf1)=="finalNonFavExpPts"] <- "NonFavExpPts"
  testdf1$logit <- predict(testmodel, newdata = testdf1, type = "link")
  testdf1 <- testdf1 |> 
    mutate(myprob = 1 / (1 + exp(-logit)))
  testdf$dummywp <- testdf1$myprob
  testdf <- testdf |> 
    mutate(dummywpdiff = dummywp - FavWPB) |> 
    mutate(possdummywpdiff = if_else(Possession==FavTeam, dummywpdiff, -dummywpdiff))
  testdf <- testdf |> select(-dummywp)
  testdf <- testdf |> select(-dummywpdiff)
  testdf <- testdf |> select(-finalFavExpPts)
  testdf <- testdf |> select(-finalNonFavExpPts)
  testdf <- testdf |> select(-finalPtSpread)
  colnames(testdf)[colnames(testdf)=="possdummywpdiff"] <- paste0("Fav",myyd,"WPDiff")
  print(yd)
}

##############


########################################################
testdf <- testdf |> filter(BasePlayType!="ST")

testdf <- testdf |> 
  mutate(dummyYdstoEZBef = case_when(
    YdstoEZBef <= 5 ~ 5,
    YdstoEZBef > 5 & YdstoEZBef <= 10 ~ 10,
    YdstoEZBef > 10 & YdstoEZBef <= 15 ~ 15,
    YdstoEZBef > 15 & YdstoEZBef <= 20 ~ 20,
    YdstoEZBef > 20 & YdstoEZBef <= 25 ~ 25,
    YdstoEZBef > 25 & YdstoEZBef <= 30 ~ 30,
    YdstoEZBef > 30 & YdstoEZBef <= 35 ~ 35,
    YdstoEZBef > 35 & YdstoEZBef <= 40 ~ 40,
    YdstoEZBef > 40 & YdstoEZBef <= 45 ~ 45,
    YdstoEZBef > 45 & YdstoEZBef <= 50 ~ 50,
    YdstoEZBef > 50 & YdstoEZBef <= 55 ~ 55,
    YdstoEZBef > 55 & YdstoEZBef <= 60 ~ 60,
    YdstoEZBef > 60 & YdstoEZBef <= 65 ~ 65,
    YdstoEZBef > 65 & YdstoEZBef <= 70 ~ 70,
    YdstoEZBef > 70 & YdstoEZBef <= 75 ~ 75,
    YdstoEZBef > 75 & YdstoEZBef <= 80 ~ 80,
    YdstoEZBef > 80 & YdstoEZBef <= 85 ~ 85,
    YdstoEZBef > 85 & YdstoEZBef <= 90 ~ 90,
    YdstoEZBef > 90 & YdstoEZBef <= 95 ~ 95,
    YdstoEZBef > 95 & YdstoEZBef <= 100 ~ 100
  )) |> 
  mutate(dummyToGo = case_when(
    ToGo <= 3 ~ 3,
    ToGo > 3 & ToGo <= 6 ~ 6,
    ToGo > 6 & ToGo <= 9 ~ 9,
    ToGo > 9 & ToGo <= 12 ~ 12,
    ToGo > 12 & ToGo <= 15 ~ 15,
    ToGo > 15 & ToGo <= 18 ~ 18,
    ToGo > 18 & ToGo <= 21 ~ 21,
    ToGo > 21 & ToGo <= 24 ~ 24,
    ToGo > 24 & ToGo <= 27 ~ 27,
    ToGo > 27 & ToGo <= 30 ~ 30,
    ToGo > 30 & ToGo <= 33 ~ 33,
    ToGo > 33 & ToGo <= 36 ~ 36,
    ToGo > 36 & ToGo <= 39 ~ 39,
    ToGo > 39 ~ 40,
  ))

probchart <- tibble(Down = c(1,2,3,4))
probchart <- expand.grid(Down = probchart$Down, 
                         YdstoEZBef = c(5,10,15,20,25,30,35,40,45,50,55,60,65,
                                        70,75,80,85,90,95,100))
probchart <- expand.grid(Down = probchart$Down, YdstoEZBef = probchart$YdstoEZBef,
                         ToGo = c(3,6,9,12,15,18,21,24,27,30,33,36,39,40))
probchart <- unique(probchart)
for(r in c(-100, -20:99)){
  myyd <- as.character(r)
  myyd <- if_else(grepl("-", myyd), gsub("\\-","neg",myyd), paste0("pos", myyd))
  probchart$dummycol <- ""
  colnames(probchart)[colnames(probchart)=="dummycol"] <- myyd
}


for(o in c(1:nrow(probchart))){
  minidf <- testdf |> filter(Down==probchart$Down[o], 
                             dummyYdstoEZBef==probchart$YdstoEZBef[o],
                             dummyToGo==probchart$ToGo[o])
  for(yd in c(-100, -20:99)){
    myyd <- as.character(yd)
    mycol <- if_else(grepl("-", myyd), gsub("\\-","neg",myyd), paste0("pos", myyd))
    indkey <- which(colnames(probchart)==mycol)
    if(isTRUE(yd==-100)){
      myprob <- length(which(as.integer(minidf$YdsGained) < -20))/nrow(minidf)
    }
    if(isFALSE(yd==-100)){
      myprob <- length(which(minidf$YdsGained==yd))/nrow(minidf)
    }
    probchart[[indkey]][o] <- myprob
  }
  print(o)
}

probchart2 <- probchart |> filter(neg20!=NaN)
saveRDS(probchart2, "ydsprobchart.rds")

saveRDS(testdf, "Beforejoinwithprobchart.rds")
#######################################################


setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
testdf <- readRDS("Beforejoinwithprobchart.rds")
probchart <- readRDS("ydsprobchart.rds")

colnames(probchart)[colnames(probchart)=="YdstoEZBef"] <- "dummyYdstoEZBef"
colnames(probchart)[colnames(probchart)=="ToGo"] <- "dummyToGo"

testdf1 <- left_join(testdf, probchart)

for(yd in c(-100, -20:99)){
  myyd <- as.character(yd)
  indkey <- which(colnames(testdf1)==paste0("Fav", myyd, "WPDiff"))
  myyd <- if_else(grepl("-", myyd), gsub("\\-","neg",myyd), paste0("pos", myyd))
  preindkey <- which(colnames(testdf1)==myyd)
  testdf1[[preindkey]] <- as.numeric(testdf1[[preindkey]])
  mycolname <- paste0(myyd, "Expperyd")
  mycolname2 <- paste0(myyd, "Expsqperyd")
  testdf1 <- testdf1 |> 
    mutate(dummycol = testdf1[[preindkey]] * testdf1[[indkey]]) |> 
    mutate(dummycol2 = testdf1[[preindkey]] * (testdf1[[indkey]])^2)
  colnames(testdf1)[colnames(testdf1)=="dummycol"] <- mycolname
  colnames(testdf1)[colnames(testdf1)=="dummycol2"] <- mycolname2
  print(yd)
}

testdf1 <- testdf1 |> 
  mutate(ExpWPDiff = rowSums(across(contains("Expper")))) |> 
  mutate(ExpsqWPDiff = rowSums(across(contains("Expsqper")))) |> 
  mutate(VarWPDiff = ExpsqWPDiff - (ExpWPDiff)^2)

testdf1 <- testdf1 |> select(-(neg100Expperyd:pos99Expsqperyd))
testdf1 <- testdf1 |> select(-(`-100YdsGnd`:`99YdsGnd`))
testdf1 <- testdf1 |> select(-(`-100FavExpPts`:`99NonFavExpPts`))
testdf1 <- testdf1 |> select(-(neg100:pos99))


testdf1 <- testdf1 |> select(-(`Fav-100WPDiff`:`Fav99WPDiff`))

testdf1 <- testdf1 |> 
  mutate(PossWPDiff = if_else(Possession==FavTeam, FavWPDiff, -FavWPDiff)) |> 
  mutate(WPDiffZScore = round((PossWPDiff - ExpWPDiff) / sqrt(VarWPDiff), 3))

saveRDS(testdf1, "NewFinalModResults.rds")


##############

wpdf <- readRDS("NewFinalModResults.rds")
wpdf <- wpdf %>% 
  mutate(TimePassed = if_else(Quarter==5,3600+(900-Seconds),3600-Seconds))
wpdf <- wpdf %>% relocate(TimePassed, .after = Time)
wpdf <- wpdf %>% 
  mutate(TmColor = case_when(
    FavTeam=="ARI" ~ "#97233F",
    FavTeam=="ATL" ~ "#A71930",
    FavTeam=="BAL" ~ "#241773",
    FavTeam=="BUF" ~ "#00338D",
    FavTeam=="CAR" ~ "#0085CA",
    FavTeam=="CHI" ~ "#0B162A",
    FavTeam=="CIN" ~ "#FB4F14",
    FavTeam=="CLE" ~ "#311D00",
    FavTeam=="DAL" ~ "#041E42",
    FavTeam=="DEN" ~ "#FB4F14",
    FavTeam=="DET" ~ "#0076B6",
    FavTeam=="GNB" ~ "#203731",
    FavTeam=="HOU" ~ "#03202F",
    FavTeam=="IND" ~ "#002C5F",
    FavTeam=="JAX" ~ "#9F792C",
    FavTeam=="KAN" ~ "#E31837",
    FavTeam=="LAC" ~ "#0072CE",
    FavTeam=="SDG" ~ "#0072CE",
    FavTeam=="LAR" ~ "#B3995D",
    FavTeam=="STL" ~ "#B3995D",
    FavTeam=="LVR" ~ "#A5ACAF",
    FavTeam=="OAK" ~ "#A5ACAF",
    FavTeam=="MIA" ~ "#008E97",
    FavTeam=="MIN" ~ "#4F2683",
    FavTeam=="NOR" ~ "#D3BC8D",
    FavTeam=="NWE" ~ "#002244",
    FavTeam=="NYG" ~ "#0B2265",
    FavTeam=="NYJ" ~ "#203731",
    FavTeam=="PHI" ~ "#004C54",
    FavTeam=="PIT" ~ "#FFB81C",
    FavTeam=="SEA" ~ "#69BE28",
    FavTeam=="SFO" ~ "#AA0000",
    FavTeam=="TAM" ~ "#D50A0A",
    FavTeam=="TEN" ~ "#4B92DB",
    FavTeam=="WAS" ~ "#773141"
  ))

yearlist <- c(2006:2023)
for(year in yearlist){
  if(year>=2021){
    weeklist <- c(1:18)
  }
  if(year<2021){
    weeklist <- c(1:17)
  }
  for(week in weeklist){
    mydf <- wpdf %>% filter(Year==year) %>% filter(Week==week)
    tm1list <- unique(mydf$Team1Name)
    for(tm1 in tm1list){
      mydf1 <- mydf %>% filter(Team1Name==tm1)
      mydf1 <- mydf1 %>% filter(FavTeam==FavTeam[1])
      if(isTRUE(mydf1$FavTeam[1]==mydf1$Team1Name[1])){
        oppcolordf <- wpdf %>% filter(wpdf$FavTeam==mydf1$Team2Name[1])
      }
      if(isTRUE(mydf1$FavTeam[1]==mydf1$Team2Name[1])){
        oppcolordf <- wpdf %>% filter(wpdf$FavTeam==mydf1$Team1Name[1])
      }
      
      favcolor <- mydf1$TmColor[1]
      oppcolor <- oppcolordf$TmColor[1]
      
      myplot <- ggplot(mydf1, aes(x=TimePassed, y=FavWPA)) +
        geom_line(color = mydf1$TmColor, linewidth = .75) +
        ylim(0,1) +
        geom_hline(yintercept = 0.5) +
        geom_ribbon(aes(ymin = pmin(FavWPA, 0.5), ymax = pmax(FavWPA, 0.5)), fill = mydf1$TmColor, alpha = 0.5) +
        ggtitle(paste0(mydf1$Team1Name[1]," @ ",mydf1$Team2Name[1],"                         ",
                       "Favorite: ", mydf1$FavTeam[1], " -", mydf1$PtSpread[1]))
      mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/Win Prob Graphs/",year,"/Week ",week)
      setwd(mywd)
      filename <- paste0(mydf1$Team1Name[1], " @ ",mydf1$Team2Name[1],".png")
      ggsave(filename = filename, plot = myplot, width = 6, height = 4)
    }
  }
  print(year)
}
