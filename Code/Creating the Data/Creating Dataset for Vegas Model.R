library(nflreadr)
library(nflfastR)
library(tidyverse)
library(extr)
library(patchwork)

#####
dflist <- list()
for(year in 2006:2023){
  mydf <- nflreadr::load_pbp(year)
  dflist <- list.append(dflist, mydf)
  print(year)
}

basefile <- dflist[[1]]
for(i in 2:length(dflist)){
  basefile <- rbind(basefile, dflist[[i]])
  print(i)
}
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files")
saveRDS(basefile, "VegasData.rds")
####


setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files")
mydf <- readRDS("VegasData.rds")
newdf <- mydf |> select(season, week, time, qtr, posteam, home_team, home_score,
                        away_team, away_score, game_seconds_remaining,desc, wp, vegas_wp)

newdf <- na.omit(newdf)
newdf <- newdf |> filter(!grepl("kick", desc))
newdf <- newdf |> filter(!grepl("spike", desc))
newdf <- newdf |> filter(!grepl("kneel", desc))
newdf <- newdf |> filter(!grepl("punt", desc))

newdf <- newdf |> 
  mutate(posteam = case_when(
    posteam=="GB" ~ "GNB",
    posteam=="KC" ~ "KAN",
    posteam=="LA" & season<=2015 ~ "STL",
    posteam=="LA" & season>2015 ~ "LAR",
    posteam=="LAC" & season<=2016 ~ "SDG",
    posteam=="LV" & season<=2019 ~ "OAK",
    posteam=="LV" & season>2019 ~ "LVR",
    posteam=="NE" ~ "NWE",
    posteam=="NO" ~ "NOR",
    posteam=="SF" ~ "SFO",
    posteam=="TB" ~ "TAM",
    TRUE ~ posteam
  ))
colnames(newdf)[colnames(newdf)=="season"] <- "Year"
colnames(newdf)[colnames(newdf)=="week"] <- "Week"
colnames(newdf)[colnames(newdf)=="posteam"] <- "Possession"
colnames(newdf)[colnames(newdf)=="game_seconds_remaining"] <- "Seconds"


aarondf <- readRDS("NewFinalModResults.rds")
miniaardf <- aarondf |> select(Year, Week, Seconds, gamenum, FavTeam, Possession)
unique_rows <- miniaardf %>%
  distinct(Year, Week, Seconds, FavTeam, Possession, .keep_all = TRUE)

finaldf <- left_join(newdf, unique_rows)
finaldf1 <- na.omit(finaldf)

finaldf1 <- finaldf1 |> 
  mutate(away_team = case_when(
    away_team=="GB" ~ "GNB",
    away_team=="KC" ~ "KAN",
    away_team=="LA" & Year<=2015 ~ "STL",
    away_team=="LA" & Year>2015 ~ "LAR",
    away_team=="LAC" & Year<=2016 ~ "SDG",
    away_team=="LV" & Year<=2019 ~ "OAK",
    away_team=="LV" & Year>2019 ~ "LVR",
    away_team=="NE" ~ "NWE",
    away_team=="NO" ~ "NOR",
    away_team=="SF" ~ "SFO",
    away_team=="TB" ~ "TAM",
    TRUE ~ away_team
  )) |> 
  mutate(home_team = case_when(
    home_team=="GB" ~ "GNB",
    home_team=="KC" ~ "KAN",
    home_team=="LA" & Year<=2015 ~ "STL",
    home_team=="LA" & Year>2015 ~ "LAR",
    home_team=="LAC" & Year<=2016 ~ "SDG",
    home_team=="LV" & Year<=2019 ~ "OAK",
    home_team=="LV" & Year>2019 ~ "LVR",
    home_team=="NE" ~ "NWE",
    home_team=="NO" ~ "NOR",
    home_team=="SF" ~ "SFO",
    home_team=="TB" ~ "TAM",
    TRUE ~ home_team
  ))

finaldf1 <- finaldf1 |> 
  mutate(favGameMargin = if_else(FavTeam==home_team, home_score - away_score,
                                 away_score - home_score)) |> 
  mutate(FavWinLoss = if_else(favGameMargin > 0, 1, 0)) |> 
  mutate(favPFRwp = if_else(FavTeam==Possession, wp, 1 - wp)) |> 
  mutate(favVegaswp = if_else(FavTeam==Possession, vegas_wp, 1 - vegas_wp))

### For PFR
plot2 <- ggplot(finaldf1, aes(x = favPFRwp)) + 
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  labs(x = "Win Probability",
       y = "Frequency",
       title = "Frequencies of probabilities of the Favored Team Winning (PFR Model)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman"),
    axis.title.y = element_text(size = 12, family = "Times New Roman")
  )

### For Vegas
plot3 <- ggplot(finaldf1, aes(x = favVegaswp)) + 
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  labs(x = "Win Probability",
       y = "Frequency",
       title = "Frequencies of probabilities of the Favored Team Winning (Vegas Model)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, family = "Times New Roman"),
    axis.title.y = element_text(size = 12, family = "Times New Roman")
  )
plot3

comb_plot <- (plot1 / plot2 / plot3) +
  plot_annotation(
    caption = "Figure 5: Comparison of Win Probability Models"
  ) &
  theme(
    plot.caption = element_text(size = 12, hjust = 0.5)  # Adjust size and alignment
  )

### Final plot
comb_plot


# Residual Plots ----------------------------------------------------------

### PFR Model
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
  x1 <- finaldf1 %>% filter(favPFRwp>=p1&favPFRwp<p2)
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

plot5 <- ggplot(probdf, aes(x = newx, y = MyModelres)) + 
  geom_point(color = "royalblue") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-.2, .2) +
  labs(x = "Win Probability",
       y = "Model Residuals",
       title = "Residuals for Win Probability Model (PFR Model)") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
plot5

probdf <- probdf |> 
  mutate(abs_res = abs(MyModelres)) |> 
  mutate(sq_res = MyModelres^2)
sum(probdf$abs_res)
sum(probdf$sq_res)

### Vegas Model
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
  x1 <- finaldf1 %>% filter(favVegaswp>=p1&favVegaswp<p2)
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

plot6 <- ggplot(probdf, aes(x = newx, y = MyModelres)) + 
  geom_point(color = "royalblue") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Win Probability",
       y = "Model Residuals",
       title = "Residuals for Win Probability Model (Vegas Model)") +
  ylim(-.25, .25) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
plot6

probdf <- probdf |> 
  mutate(abs_res = abs(MyModelres)) |> 
  mutate(sq_res = MyModelres^2)
sum(probdf$abs_res)
sum(probdf$sq_res)

comb_plot2 <- (plot4 / plot5 / plot6) +
  plot_annotation(
    caption = "Figure 6: Comparison of Residuals of Win Probability Models"
  ) &
  theme(
    plot.caption = element_text(size = 12, hjust = 0.5)  # Adjust size and alignment
  )

### Final plot
comb_plot2
