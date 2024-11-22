library(tidyverse)
library(patchwork)

setwd("C:/Users/Aaron Friedlander/Desktop/Updated QB Project/QB Data/Compiled PFF Datasets")
mydf <- read_csv("2006+QBPFFData.csv")
mydf <- mydf |> 
  mutate(TmPtDiff = TmScore - OppScore) |> 
  mutate(WinLoss = if_else(TmPtDiff > 0, 1, 0))

newdf <- mydf |> select(player, Year, Week, grades_offense.x, qb_rating, WinLoss, TmPtDiff)

### PFF boxplot
pff_boxplot <- ggplot(data = newdf, aes(x = WinLoss, y = grades_offense.x, group = WinLoss)) +
  geom_boxplot() +
  labs(x = "Win or Loss",
       y = "PFF Grade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### PFR boxplot
pfr_boxplot <- ggplot(data = newdf, aes(x = WinLoss, y = qb_rating, group = WinLoss)) +
  geom_boxplot() +
  labs(x = "Win or Loss",
       y = "PFR Passer Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

comb_box <- fqbr_boxplot + pff_boxplot + pfr_boxplot +
  plot_annotation(
    caption = "Figure 9: Boxplots of QB Metrics for Wins and Losses"
  ) &
  theme(
    plot.caption = element_text(size = 12, hjust = 0.5)  # Adjust size and alignment
  )
comb_box
  


### PFF  hist
pff_hist <- ggplot(data = newdf, aes(x = grades_offense.x)) + 
  geom_histogram() +
  labs(title = "Distribution of PFF Offensive Grades",
       x = "PFF Grades") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### PFR hist
pfr_hist <- ggplot(data = newdf, aes(x = qb_rating)) + 
  geom_histogram() + 
  labs(title = "Distribution of PFR Passer Rating",
       x = "PFR Passer Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Comb hist
comb_hist <- fqbr_hist / pff_hist / pfr_hist +
  plot_annotation(
    caption = "Figure 8: Histograms of QB Grade Metrics"
  ) &
  theme(
    plot.caption = element_text(size = 12, hjust = 0.5)  # Adjust size and alignment
  )
comb_hist
  


### PFF Graph
mymod <- lm(TmPtDiff ~ grades_offense.x, data = newdf)
summary(mymod)

pffplot <- ggplot(newdf, aes(x = grades_offense.x, y = TmPtDiff)) +
  geom_point(size = 0.8, alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "PFF Grade",
       y = "QB's Team's Point Diff.",
       title = "PFF Grade vs. Quarterback's Team's Point Differential         "~ R^2 ~ "= .2056") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


pffplot + plot_annotation(
  caption = "Figure 7: PFF Model"
) &
  theme(
    plot.caption = element_text(size = 12, hjust = 0.5)  # Adjust size and alignment
  )

### PFR Graph
mymod <- lm(TmPtDiff ~ qb_rating, data = newdf)
summary(mymod)

pfrplot <- ggplot(newdf, aes(x = qb_rating, y = TmPtDiff)) +
  geom_point(size = 0.8, alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "PFR Passer Rating",
       y = "QB's Team's Point Diff.",
       title = "PFR Passer Rating vs. Quarterback's Team's Point Differential         "~ R^2 ~ "= .2692") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


combplot <- fqbrplot / pffplot / pfrplot +
  plot_annotation(
    caption = "Figure 7: QB Grade Models vs. Point Differential"
  ) &
  theme(
    plot.caption = element_text(size = 12, hjust = 0.5)  # Adjust size and alignment
  )
combplot
