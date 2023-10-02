catcher_df <- df_sec %>% 
  select(catcher, pop_2b_sba, maxeff_arm_2b_3b_sba, exchange_2b_3b_sba) %>% 
  group_by(catcher) %>% 
  summarise(pop_2b_sba = mean(pop_2b_sba, na.rm =T),
            maxeff_arm_2b_3b_sba = mean(maxeff_arm_2b_3b_sba, na.rm = T),
            exchange_2b_3b_sba = mean(exchange_2b_3b_sba, na.rm = T))

runner_df <- df_sec %>% 
  select(runner, sprint_speed, total_attempts_r, total_stolen_r, success_rate_r) %>% 
  group_by(runner) %>% 
  summarise(sprint_speed = mean(sprint_speed, na.rm = T),
            total_attempts_r = max(total_attempts_r),
            total_stolen_r = max(total_stolen_r),
            success_rate_r = mean(success_rate_r, na.rm = T))

pitcher_df <- df_sec %>% 
  select(pitcher, total_attempts_p, total_stolen_p, success_rate_p) %>% 
  group_by(pitcher) %>% 
  summarise(total_attempts_p = max(total_attempts_p),
            total_stolen_p = max(total_stolen_p),
            success_rate_p = mean(success_rate_p, na.rm = T))


predict_stolen <- function(pitcher_name, catcher_name, runner_name, model, runner_df, catcher_df) {
  
  # Look up the features for the runner and catcher
  runner_features <- runner_df[runner_df$runner == runner_name, ]
  catcher_features <- catcher_df[catcher_df$catcher == catcher_name, ]
  pitcher_features <- pitcher_df[pitcher_df$pitcher == pitcher_name, ]
  
  # Create a one-row dataframe with the input variables and looked-up features
  input_data <- data.frame(
    runner = runner_name,
    sprint_speed = runner_features$sprint_speed, 
    total_attempts_r = runner_features$total_attempts_r,
    total_stolen_r = runner_features$total_stolen_r,
    success_rate_r = runner_features$success_rate_r,
    total_attempts_p = pitcher_features$total_attempts_p,
    total_stolen_p = pitcher_features$total_stolen_p,
    success_rate_p = pitcher_features$success_rate_p,
    catcher = catcher_name,
    pop_2b_sba = catcher_features$pop_2b_sba, 
    maxeff_arm_2b_3b_sba = catcher_features$maxeff_arm_2b_3b_sba, 
    exchange_2b_3b_sba = catcher_features$exchange_2b_3b_sba,
    pitcher = pitcher_name
  )
  
  # Use the model to make a prediction and get class probabilities
  prediction_prob <- predict(model, input_data, type = "prob")
  
  return(prediction_prob)
}

# Usage
pitcher_input <- "Justin Steele"
catcher_input <- "Gabriel Moreno"
runner_input <- "Rowdy Tellez"

is_stolen_prob <- predict_stolen(pitcher_input, catcher_input, runner_input, rf_cv, runner_df, catcher_df)
print(is_stolen_prob)


