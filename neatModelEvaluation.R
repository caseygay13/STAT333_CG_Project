library(tidyverse)
library(tinytex)
library(corrplot)
library(GGally)
library(car)
library(MASS)
library(knitr)
library(gridExtra)
library(forecast)
library(nlme)


#Set-Up
nba_data <- read.csv("nba_data(in).csv")
nba_data$Season <- as.factor(nba_data$Season)
nba_data$Team <- as.factor(nba_data$Team)
colnames(nba_data)[7] <- "FG_PCT"
colnames(nba_data)[10] <- "X3P_PCT"
colnames(nba_data)[13] <- "X2P_PCT"
colnames(nba_data)[16] <- "FT_PCT"
colnames(nba_data)[28] <- "WIN_PCT"

#Variable Combinations
full_model <- lm(WIN_PCT ~ FG+FGA+FG_PCT+X3P+X3PA+X3P_PCT+X2P+X2PA+X2P_PCT+FT+FTA+FT_PCT+ ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PTS, data = nba_data)
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)
#beta_0= -3.618277
#beta_FGA= -0.148010, beta_3P= 0.082932, beta_3PA= 0.151552, beta_2P= 0.063517, beta_2PA= 0.148857, beta_FT=0.022892,
#beta_ORB= -0.020803, beta_TRB= 0.019636, beta_AST= -0.006443, beta_STL= 0.026814, beta_TOV= -0.012467, beta_PF= 0.008277

shooting_model <- lm(WIN_PCT ~ X2PA+X2P_PCT+X3PA+X3P_PCT+FTA+FT_PCT+ORB+DRB+AST+TOV+PF+STL+BLK, data = nba_data)
#beta_0= -6.706766
#beta_2PA= 0.033850, beta_2PT_PCT= 3.217521, beta_3PA= 0.033251, beta_3P_PCT= 2.521863, beta_FTA= 0.017801, beta_FT_PCT= 0.572540, 
#beta_ORB= -0.00958, beta_DRB= 0.020244, beta_AST= -0.003120, beta_TOV= -0.013708, beta_PF= 0.007554, beta_STL= 0.025787, beta_BLK= 0.000203

PTS_model <- lm(WIN_PCT ~ ORB+TRB+STL+TOV+PF+PTS, data=nba_data)
#beta_0= -3.464188
#beta_ORB= -0.019834, beta_TRB= 0.021152, beta_STL= 0.028197, beta_TOV= -0.015543, beta_PF= 0.007803, beta_PTS= 0.027396

#Data Prepping & Splitting

#I recommend that we use the 18-19, 20-221, 21-22, and 22-23 seasons for training and 23-24 alone for testing
#This will make it easier for us, but we will need to remember to change this in our report and presentation

#We will need to make note in our report of our edits/additions to the data set, since these mutations are not public information
lag1_data <- nba_data %>%
  arrange(Team, Season) %>%
  group_by(Team) %>%
  mutate(across(where(is.numeric), lag, .names = "{.col}_Last")) %>%
  ungroup()
train_1yr <- lag1_data %>% 
  filter(Season %in% c("2019", "2021", "2022", "2023")) %>%
  filter(!if_any(ends_with("_Last"), is.na))
test_1yr <- lag1_data %>% filter(Season == "2024")

nba5 <- nba_data %>% filter(Season != "Overall")
nba5$Season <- as.numeric(nba5$Season)
test_avg <- nba5 %>%
  group_by(Team) %>%
  arrange(Season) %>%
  slice_tail(n=4) %>%
  summarize(Season="TestAvg", across(where(is.numeric),mean,na.rm=TRUE))
nba_extended_data <- bind_rows(nba_data, test_avg)
train_5yr <- nba_extended_data %>% filter(!Season %in% c("2024","Overall","TestAvg"))
test_5yr <- nba_extended_data %>% filter(Season == "TestAvg")

train_ar <- lag1_data %>% 
  filter(Season %in% c("2019", "2021", "2022", "2023")) %>%
  filter(!if_any(ends_with("_Last"), is.na))
test_ar <- lag1_data %>% filter(Season == "2024")

#1-Year Lag Model Testing
PTS_train1 <- lm(WIN_PCT ~ ORB_Last+TRB_Last+STL_Last+TOV_Last+PF_Last+PTS_Last, data=train_1yr)
PTS_preds1 <- predict(PTS_train1, newdata=test_1yr)
PTS_MSPE1 <- mean((test_1yr$WIN_PCT - PTS_preds1)^2) #0.02221101

shooting_train1 <- lm(WIN_PCT ~ X2PA_Last+X2P_PCT_Last+X3PA_Last+X3P_PCT_Last+FTA_Last+FT_PCT_Last+ORB_Last+DRB_Last+AST_Last+TOV_Last+PF_Last+STL_Last+BLK_Last, data = train_1yr)
shooting_preds1 <- predict(shooting_train1, newdata=test_1yr)
shooting_MSPE1 <- mean((test_1yr$WIN_PCT - shooting_preds1)^2) #0.02653551

step_train1 <- lm(WIN_PCT ~ FGA_Last+X3P_Last+X3PA_Last+X2P_Last+X2PA_Last+FT_Last+ORB_Last+TRB_Last+AST_Last+STL_Last+TOV_Last+PF_Last, data = train_1yr)
step_preds1 <- predict(step_train1, newdata=test_1yr)
step_MSPE1 <- mean((test_1yr$WIN_PCT - step_preds1)^2) #0.02540999

#5-Year Average Lag Model Testing (data must be changed to take 4-yr averages for testing)
PTS_train2 <- lm(WIN_PCT ~ ORB+TRB+STL+TOV+PF+PTS, data=train_5yr)
PTS_preds2 <- predict(PTS_train2, newdata=test_5yr)
PTS_MSPE2 <- mean((test_5yr$WIN_PCT - PTS_preds2)^2) #0.001322774

shooting_train2 <- lm(WIN_PCT ~ X2PA+X2P_PCT+X3PA+X3P_PCT+FTA+FT_PCT+ORB+DRB+AST+TOV+PF+STL+BLK, data = train_5yr)
shooting_preds2 <- predict(shooting_train2, newdata=test_5yr)
shooting_MSPE2 <- mean((test_5yr$WIN_PCT - shooting_preds2)^2) #0.001412503

step_train2 <- lm(WIN_PCT ~ FGA+X3P+X3PA+X2P+X2PA+FT+ORB+TRB+AST+STL+TOV+PF, data = train_5yr)
step_preds2 <- predict(step_train2, newdata=test_5yr)
step_MSPE2 <- mean((test_5yr$WIN_PCT - step_preds2)^2) #0.001170818

#Autoregressive AR(1) Model Testing (*need to change this from lm*)
PTS_train3 <- lm(WIN_PCT ~ WIN_PCT_Last+ORB_Last+TRB_Last+STL_Last+TOV_Last+PF_Last+PTS_Last, data=train_ar)
PTS_preds3 <- predict(PTS_train3, newdata=test_ar)
PTS_MSPE3 <- mean((test_ar$WIN_PCT - PTS_preds3)^2) #0.02176663

shooting_train3 <- lm(WIN_PCT ~ WIN_PCT_Last+X2PA_Last+X2P_PCT_Last+X3PA_Last+X3P_PCT_Last+FTA_Last+FT_PCT_Last+ORB_Last+DRB_Last+AST_Last+TOV_Last+PF_Last+STL_Last+BLK_Last, data = train_ar)
shooting_preds3 <- predict(shooting_train3, newdata=test_ar)
shooting_MSPE3 <- mean((test_ar$WIN_PCT - shooting_preds3)^2) #0.02679393

step_train3 <- lm(WIN_PCT ~ WIN_PCT_Last+FGA_Last+X3P_Last+X3PA_Last+X2P_Last+X2PA_Last+FT_Last+ORB_Last+TRB_Last+AST_Last+STL_Last+TOV_Last+PF_Last, data = train_ar)
step_preds3 <- predict(step_train3, newdata=test_ar)
step_MSPE3 <- mean((test_ar$WIN_PCT - step_preds3)^2) #0.02545581


# Current Progress Evaluation
# An AR(1) model using the PTS Model Variables is the preferred model by the MSPE criterion






# Trying to make AR(1) models work. # For PTS model with AR(1) errors


# We will use this for the ar1 models
PTS_gls <- gls(WIN_PCT~WIN_PCT_Last+ ORB_Last+TRB_Last+ STL_Last+ TOV_Last+ PF_Last+ PTS_Last, correlation=corAR1(form=~1),data=train_ar)

# Shooting GLS
shooting_gls <- gls(WIN_PCT ~ WIN_PCT_Last+X2PA_Last+X2P_PCT_Last+X3PA_Last+X3P_PCT_Last+FTA_Last+FT_PCT_Last+ORB_Last+DRB_Last+AST_Last+TOV_Last+PF_Last+STL_Last+BLK_Last, correlation=corAR1(form=~1),data=train_ar)

# Stepwise GLS
step_gls <- gls(WIN_PCT ~ WIN_PCT_Last+FGA_Last+X3P_Last+X3PA_Last+X2P_Last+X2PA_Last+FT_Last+ORB_Last+TRB_Last+AST_Last+STL_Last+TOV_Last+PF_Last, correlation=corAR1(form=~1),data=train_ar)

# forecasts


PTS_gls_preds <- predict(PTS_gls, newdata = test_ar)
PTS_gls_MSPE <- mean((test_ar$WIN_PCT - PTS_gls_preds)^2) # 0.02092756

shooting_gls_preds <- predict(shooting_gls, newdata = test_ar)
shooting_gls_MSPE <- mean((test_ar$WIN_PCT - shooting_gls_preds)^2) # 0.02695467

step_gls_preds <- predict(step_gls, newdata = test_ar)
step_gls_MSPE <- mean((test_ar$WIN_PCT - step_gls_preds)^2) # 0.02493319

