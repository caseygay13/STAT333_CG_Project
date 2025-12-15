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
library(dplyr)
library(slider)
library(ggimage)
library(ggplot2)
library(ggtext) 


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
  filter(Season %in% c("2021", "2022", "2023")) %>%
  filter(!if_any(ends_with("_Last"), is.na))
test_1yr <- lag1_data %>% filter(Season == "2024")

nba_avg3 <- nba_data %>%
  filter(Season != "Overall") %>%
  arrange(Team, Season) %>%
  group_by(Team) %>%
  mutate(across(
    where(is.numeric),
    ~ slide_dbl(.x, mean, .before = 3, .after=-1, .complete = TRUE),
    .names = "{.col}_Avg3"
  )) %>%
  ungroup()
train_avg3 <- nba_avg3 %>% filter(Season %in% c("2021", "2022", "2023"))
test_avg3  <- nba_avg3 %>% filter(Season == "2024")

train_ar <- lag1_data %>% 
  filter(Season %in% c("2021", "2022", "2023")) %>%
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

#1-Year Lag Model Testing w/ Last Win%
PTS_train4 <- lm(WIN_PCT ~ WIN_PCT_Last+ORB_Last+TRB_Last+STL_Last+TOV_Last+PF_Last+PTS_Last, data=train_1yr)
PTS_preds4 <- predict(PTS_train4, newdata=test_1yr)
PTS_MSPE4 <- mean((test_1yr$WIN_PCT - PTS_preds4)^2) #0.02176663

shooting_train4 <- lm(WIN_PCT ~ WIN_PCT_Last+X2PA_Last+X2P_PCT_Last+X3PA_Last+X3P_PCT_Last+FTA_Last+FT_PCT_Last+ORB_Last+DRB_Last+AST_Last+TOV_Last+PF_Last+STL_Last+BLK_Last, data = train_1yr)
shooting_preds4 <- predict(shooting_train4, newdata=test_1yr)
shooting_MSPE4 <- mean((test_1yr$WIN_PCT - shooting_preds4)^2) #0.02679393

step_train4 <- lm(WIN_PCT ~ WIN_PCT_Last+FGA_Last+X3P_Last+X3PA_Last+X2P_Last+X2PA_Last+FT_Last+ORB_Last+TRB_Last+AST_Last+STL_Last+TOV_Last+PF_Last, data = train_1yr)
step_preds4 <- predict(step_train4, newdata=test_1yr)
step_MSPE4 <- mean((test_1yr$WIN_PCT - step_preds4)^2) #0.02545581

#3-Year Average Lag Model Testing (data must be changed to take 4-yr averages for testing)
PTS_train2 <- lm(WIN_PCT ~ ORB_Avg3+TRB_Avg3+STL_Avg3+TOV_Avg3+PF_Avg3+PTS_Avg3, data=train_avg3)
PTS_preds2 <- predict(PTS_train2, newdata=test_avg3)
PTS_MSPE2 <- mean((test_avg3$WIN_PCT - PTS_preds2)^2) #0.02453875

shooting_train2 <- lm(WIN_PCT ~ X2PA_Avg3+X2P_PCT_Avg3+X3PA_Avg3+X3P_PCT_Avg3+FTA_Avg3+FT_PCT_Avg3+ORB_Avg3+DRB_Avg3+AST_Avg3+TOV_Avg3+PF_Avg3+STL_Avg3+BLK_Avg3, data = train_avg3)
shooting_preds2 <- predict(shooting_train2, newdata=test_avg3)
shooting_MSPE2 <- mean((test_avg3$WIN_PCT - shooting_preds2)^2) #0.03185405

step_train2 <- lm(WIN_PCT ~ FGA_Avg3+X3P_Avg3+X3PA_Avg3+X2P_Avg3+X2PA_Avg3+FT_Avg3+ORB_Avg3+TRB_Avg3+AST_Avg3+STL_Avg3+TOV_Avg3+PF_Avg3, data = train_avg3)
step_preds2 <- predict(step_train2, newdata=test_avg3)
step_MSPE2 <- mean((test_avg3$WIN_PCT - step_preds2)^2) #0.0289682

# ARIMA forecasts
# PTS forecast
PTS_ar_model <- Arima(train_ar$WIN_PCT,
                  xreg = as.matrix(train_ar[, c("WIN_PCT_Last", "ORB_Last", "TRB_Last", "STL_Last", "TOV_Last", "PF_Last", "PTS_Last")]),
                  order = c(1,0,0))
PTS_fc <- forecast(PTS_ar_model, xreg = as.matrix(test_ar[, c("WIN_PCT_Last", "ORB_Last", "TRB_Last", "STL_Last", "TOV_Last", "PF_Last", "PTS_Last")]))

PTS_ar_preds <- as.numeric(PTS_fc$mean)
PTS_ar_MSPE <- mean((test_ar$WIN_PCT - PTS_ar_preds)^2) # 0.02056293

# Shooting forecast
shooting_ar_model <- Arima(train_ar$WIN_PCT,
                      xreg = as.matrix(train_ar[, c("WIN_PCT_Last","X2PA_Last","X2P_PCT_Last","X3PA_Last","X3P_PCT_Last","FTA_Last","FT_PCT_Last","ORB_Last","DRB_Last","AST_Last","TOV_Last","PF_Last","STL_Last","BLK_Last")]),
                      order = c(1,0,0))
shooting_fc <- forecast(shooting_ar_model, xreg = as.matrix(test_ar[, c("WIN_PCT_Last","X2PA_Last","X2P_PCT_Last","X3PA_Last","X3P_PCT_Last","FTA_Last","FT_PCT_Last","ORB_Last","DRB_Last","AST_Last","TOV_Last","PF_Last","STL_Last","BLK_Last")]))

shooting_ar_preds <- as.numeric(shooting_fc$mean)
shooting_ar_MSPE <- mean((test_ar$WIN_PCT - shooting_ar_preds)^2) # 0.02617148

# stepwise forecast
step_ar_model <- Arima(train_ar$WIN_PCT,
                      xreg = as.matrix(train_ar[, c("WIN_PCT_Last","FGA_Last","X3P_Last","X3PA_Last","X2P_Last","X2PA_Last","FT_Last","ORB_Last","TRB_Last","AST_Last","STL_Last","TOV_Last","PF_Last")]),
                      order = c(1,0,0))
step_fc <- forecast(step_ar_model, xreg = as.matrix(test_ar[, c("WIN_PCT_Last","FGA_Last","X3P_Last","X3PA_Last","X2P_Last","X2PA_Last","FT_Last","ORB_Last","TRB_Last","AST_Last","STL_Last","TOV_Last","PF_Last")]))

step_ar_preds <- as.numeric(step_fc$mean)
step_ar_MSPE <- mean((test_ar$WIN_PCT - step_ar_preds)^2) # 0.02422745

# new ARIMA forecasts
# PTS forecast
PTS_ar_model2 <- Arima(train_ar$WIN_PCT,
                      xreg = as.matrix(train_ar[, c("ORB_Last", "TRB_Last", "STL_Last", "TOV_Last", "PF_Last", "PTS_Last")]),
                      order = c(1,0,0))
PTS_fc2 <- forecast(PTS_ar_model2, xreg = as.matrix(test_ar[, c("ORB_Last", "TRB_Last", "STL_Last", "TOV_Last", "PF_Last", "PTS_Last")]))

PTS_ar_preds2 <- as.numeric(PTS_fc2$mean)
PTS_ar_MSPE2 <- mean((test_ar$WIN_PCT - PTS_ar_preds2)^2) #0.02175304

# Shooting forecast
shooting_ar_model2 <- Arima(train_ar$WIN_PCT,
                           xreg = as.matrix(train_ar[, c("X2PA_Last","X2P_PCT_Last","X3PA_Last","X3P_PCT_Last","FTA_Last","FT_PCT_Last","ORB_Last","DRB_Last","AST_Last","TOV_Last","PF_Last","STL_Last","BLK_Last")]),
                           order = c(1,0,0))
shooting_fc2 <- forecast(shooting_ar_model2, xreg = as.matrix(test_ar[, c("X2PA_Last","X2P_PCT_Last","X3PA_Last","X3P_PCT_Last","FTA_Last","FT_PCT_Last","ORB_Last","DRB_Last","AST_Last","TOV_Last","PF_Last","STL_Last","BLK_Last")]))

shooting_ar_preds2 <- as.numeric(shooting_fc2$mean)
shooting_ar_MSPE2 <- mean((test_ar$WIN_PCT - shooting_ar_preds2)^2) # 0.02680037

# stepwise forecast
step_ar_model2 <- Arima(train_ar$WIN_PCT,
                       xreg = as.matrix(train_ar[, c("FGA_Last","X3P_Last","X3PA_Last","X2P_Last","X2PA_Last","FT_Last","ORB_Last","TRB_Last","AST_Last","STL_Last","TOV_Last","PF_Last")]),
                       order = c(1,0,0))
step_fc2 <- forecast(step_ar_model2, xreg = as.matrix(test_ar[, c("FGA_Last","X3P_Last","X3PA_Last","X2P_Last","X2PA_Last","FT_Last","ORB_Last","TRB_Last","AST_Last","STL_Last","TOV_Last","PF_Last")]))

step_ar_preds2 <- as.numeric(step_fc2$mean)
step_ar_MSPE2 <- mean((test_ar$WIN_PCT - step_ar_preds2)^2) # 0.02560242


#Model Evaluation
MSPE_table <- data.frame(
  MODEL = c("1-Yr Lag w/ PTS Variable Combination", "1-Yr Lag w/ Shooting Variable Combination", "1-Yr Lag w/ Stepwise Variable Combination", "3-Yr Average w/ PTS Variable Combination", "3-Yr Average w/ Shooting Variable Combination", "3-Yr Average w/ Stepwise Combination", "AR(1) w/ PTS Variable Combination", "AR(1) w/ Shooting Variable Combination", "AR(1) w/ Stepwise Variable Combination"),
  MSPE = c("0.0222", "0.0265", "0.0254", "0.0245", "0.0319", "0.0290", "0.0206", "0.0262", "0.0242")
)
kable(MSPE_table)

MSPE_ordered <- data.frame(
  MODEL = c("AR(1) w/ PTS Variable Combination", "1-Yr Lag w/ PTS Variable Combination", "AR(1) w/ Stepwise Variable Combination", "3-Yr Average w/ PTS Variable Combination", "1-Yr Lag w/ Stepwise Variable Combination", "AR(1) w/ Shooting Variable Combination", "1-Yr Lag w/ Shooting Variable Combination", "3-Yr Average w/ Stepwise Variable Combination", "3-Yr Average w/ Shooting Variable Combination"),
  MSPE = c("0.0206", "0.0222", "0.0242", "0.0245", "0.0254", "0.0262", "0.0265", "0.0290", "0.0319")
)
kable(MSPE_ordered)


#Forecasting 2024-2025
chosen_mod <- PTS_ar_model
summary(chosen_mod)

predict_2025_ar <- lag1_data %>%
  filter(Season == "2024") %>%
  dplyr::select(Team, WIN_PCT_Last, ORB_Last, TRB_Last, STL_Last, TOV_Last, PF_Last, PTS_Last)

PTS_2025_fc <- forecast(
  PTS_ar_model, xreg=as.matrix(predict_2025_ar[, -1]))

pred_2025 <- as.numeric(PTS_2025_fc$mean)
pred_lower95 <- as.numeric(PTS_2025_fc$lower[,2])
pred_higher95 <- as.numeric(PTS_2025_fc$upper[,2])
pred_lower80 <- as.numeric(PTS_2025_fc$lower[,1])
pred_higher80 <- as.numeric(PTS_2025_fc$upper[,1])
actual <- c(0.49, 0.74, 0.32, 0.23, 0.48, 0.78, 0.48, 0.61, 0.54, 0.59, 0.63, 0.61, 0.61, 0.61, 0.59, 0.45, 0.59, 0.60, 0.26, 0.62, 0.83, 0.50, 0.29, 0.44, 0.44, 0.49, 0.42, 0.37, 0.21, 0.22)

table_2025 <- predict_2025_ar %>%
  dplyr::select(Team) %>%
  mutate(Actual_WIN_PCT = actual, WIN_PCT_Prediction = pred_2025, Difference = abs(actual - WIN_PCT_Prediction), Lower_Bound_95 = pred_lower95, Upper_Bound_95 = pred_higher95, Lower_Bound_80 = pred_lower80, Upper_Bound_80 = pred_higher80)

kable(table_2025, digits=2)







# acf of chosen model 
# Extract residuals
res <- residuals(PTS_ar_model)

# Plot ACF of residuals
acf(res, main = "ACF of Residuals from ARIMA Model")



# Final graphic of predicted team win pct
 
plot_2025 <- table_2025 %>%
  mutate(
    Team = factor(Team, levels = Team),
    # Flag teams outside 95% PI
    Outside_95 = Actual_WIN_PCT < Lower_Bound_95 | Actual_WIN_PCT > Upper_Bound_95
  )

# Create a color vector for x-axis text
axis_colors <- ifelse(plot_2025$Outside_95, "red", "black")

ggplot(plot_2025, aes(x = Team)) +
  
  # 95% Prediction Interval
  geom_ribbon(aes(ymin = Lower_Bound_95, ymax = Upper_Bound_95),
              fill = "#b3d1ff", alpha = 0.55) +
  
  # 80% Prediction Interval
  geom_ribbon(aes(ymin = Lower_Bound_80, ymax = Upper_Bound_80),
              fill = "#66a3ff", alpha = 0.45) +
  
  # Predicted WIN% (blue)
  geom_point(aes(y = WIN_PCT_Prediction, color = "Predicted"),
             size = 3.7) +
  
  # Actual WIN% (red)
  geom_point(aes(y = Actual_WIN_PCT, color = "Actual"),
             size = 3.7) +
  
  # Lines connecting prediction → actual
  geom_segment(aes(xend = Team,
                   y = WIN_PCT_Prediction,
                   yend = Actual_WIN_PCT),
               color = "gray30", linewidth = 0.9) +
  
  # Legend colors
  scale_color_manual(
    values = c("Predicted" = "blue", "Actual" = "red"),
    name = ""
  ) +
  
  labs(
    title = "NBA 2024–25 Win%: Forecast vs Actual",
    subtitle = "Teams outside the 95% prediction interval are highlighted in red on the x-axis",
    x = "Team",
    y = "Winning Percentage"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, color = axis_colors),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )







