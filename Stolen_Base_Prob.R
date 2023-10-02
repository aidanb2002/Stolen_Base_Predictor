library(tidyverse)
library(Boruta)
library(caret)
library(randomForest)

# import retrosheet pbp files and catcher/runner data

stolen <- read.csv("/Users/aidanbeilke/Downloads/2022events.csv")
names <- read.csv("/Users/aidanbeilke/Downloads/biofile.csv")
poptime <- read.csv("/Users/aidanbeilke/Downloads/poptime (1).csv")
sprint_speed <- read.csv("/Users/aidanbeilke/Downloads/sprint_speed (1).csv")

# edit variable names for readability

new_colnames <- sub("^X[0-9]+\\.+", "", colnames(stolen))
new_colnames <- gsub("^res\\.", "", new_colnames)
new_colnames <- sub("\\.$", "", new_colnames)
colnames(stolen) <- new_colnames


#select important variables and create is_stolen
df_sec <- stolen %>%
  select(catcher, first.runner, SB.for.runner.on.1st.flag, CS.for.runner.on.1st.flag, pitcher) %>%
  filter(SB.for.runner.on.1st.flag == "TRUE" | CS.for.runner.on.1st.flag == "TRUE") %>%
  mutate(is_stolen = ifelse(SB.for.runner.on.1st.flag == "TRUE", 1, 0)) %>%
  select(-SB.for.runner.on.1st.flag, -CS.for.runner.on.1st.flag)


#get names to match with playerid codes
names <- names %>% 
  select(PLAYERID, NICKNAME, LAST)

# Merge with pitcher
df_sec <- merge(names, df_sec, by.x = "PLAYERID", by.y = "pitcher") %>% 
  mutate(pitcher = paste(NICKNAME, LAST)) %>% 
  select(-c(PLAYERID, NICKNAME, LAST))

# Merge with catcher
df_sec <- merge(names, df_sec, by.x = "PLAYERID", by.y = "catcher") %>% 
  mutate(catcher = paste(NICKNAME, LAST)) %>% 
  select(-c(PLAYERID, NICKNAME, LAST))

# Merge with first.runner
df_sec <- merge(names, df_sec, by.x = "PLAYERID", by.y = "first.runner") %>% 
  mutate(runner = paste(NICKNAME, LAST)) %>% 
  select(-c(PLAYERID, NICKNAME, LAST)) %>% 
  select(pitcher, catcher, runner, is_stolen)

# Get poptime info for catchers
poptime <- poptime %>% 
  select(catcher, pop_2b_sba, maxeff_arm_2b_3b_sba, exchange_2b_3b_sba) %>% 
  na.omit()

# Merge catcher information
df_sec <- merge(poptime, df_sec, by = "catcher")

# Get sprint speed for runners
sprint_speed <- sprint_speed %>% 
  mutate(
    runner = paste(trimws(first_name), trimws(last_name)), # Combine first_name and last_name
    runner = chartr("ñí", "ni", runner), # Replace special characters
    runner = gsub("\\bJr\\.\\b", "", runner), # Remove "Jr."
    runner = trimws(runner) # Trim any resulting white space
  ) %>% 
  select(runner, sprint_speed)

df_sec <- merge(sprint_speed, df_sec, by = "runner")

# Get attempts and success rates for runner
summary_p <- df_sec %>%
  group_by(pitcher) %>%
  summarise(
  total_attempts_p = n(),
  total_stolen_p = sum(is_stolen, na.rm = TRUE),
  success_rate_p = round((total_stolen_p / total_attempts_p),2)
)

# Add to data frame 

df_sec <- df_sec %>%
  left_join(summary_p, by = "pitcher")

# Get attempts and success rates for runner
summary_r <- df_sec %>%
  group_by(runner) %>%
  summarise(
    total_attempts_r = n(),
    total_stolen_r = sum(is_stolen, na.rm = TRUE),
    success_rate_r = round((total_stolen_r / total_attempts_r),2)
  )

# Add to data frame
df_sec <- df_sec %>%
  left_join(summary_r, by = "runner")

# Begin building models to predict is_stolen
# Separate by stealing 2nd and 3rd base

df_sec <- df_sec %>% 
  mutate(is_stolen = as.factor(is_stolen))

# Feature Selection
set.seed(123)
boruta <- Boruta(is_stolen ~ ., data = df_sec, doTrace = 2)

plot(boruta, las = 2, cex.axis = 0.5)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

# CV 
train_control <- trainControl(method = "cv", number = 5)

# Train the model using CV
set.seed(333)
rf_cv <- train(is_stolen ~ ., data = df_sec, 
               method = "rf", 
               trControl = train_control)


# Prediction & confusion matrix - test
p <- predict(rf_cv, df_sec)
confusionMatrix(p, df_sec$is_stolen)

# Add predictions to dataset
df_sec$is_stolen_pred <- p

# Get Probabilities of Both Classes
prob <- as.data.frame(predict(rf_cv,df_sec, type = "prob"))
colnames(prob) <- c("prob_cs", "prob_stolen")

# Add Probabilities to the Test Dataframe
df_sec <- cbind(df_sec, prob)

# Print accuracies
cat("Training Accuracy:", cm_train$overall['Accuracy'], "\n")
cat("Test Accuracy:", confusionMatrix(p, df_sec$is_stolen)$overall['Accuracy'], "\n")

