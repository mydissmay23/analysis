library(glmnet)

game_results <- read_csv("game_results.csv")
round_results <- read_csv("round_results.csv")



# models for the game results

games_mod2 <- lm(effort ~ factor(feedback_type)
                 + factor(ability)
                 + factor(feedback_type):factor(ability)
                 + factor(gameId),
                 data = game_results)
summary(games_mod2)


# use log transformation of effort

# add a column equal to log of effort
log_effort <- log(game_results$effort)
log_effort[log_effort == -Inf] <- NA # changes 8 observations
game_results$log_effort <- log_effort

# regressions
games_mod2_log <- lm(log_effort ~ factor(feedback_type)
                     + factor(ability)
                     + factor(feedback_type):factor(ability)
                     + factor(gameId),
                     data = game_results)
summary(games_mod2_log)



# lasso regression for the game results, including survey responses in the variables

# disregard columns that won't be used
games_lasso_df <- game_results %>%
  select(-c(playerId, numSliders, score, log_effort))

# make feedback_type and ability indicator variables
games_lasso_df$low <- as.integer(games_lasso_df$ability == "low")
games_lasso_df$relative <- as.integer(games_lasso_df$feedback_type == "relative")

# create interaction term for low and relative
games_lasso_df$low_relative <- games_lasso_df$low * games_lasso_df$relative

# remove ability and feedback type columns
games_lasso_df <- games_lasso_df %>%
  select(-c(ability, feedback_type))

# impute missing values of continuous variable (or in this case the incorrect value for age)
mean_age <- mean(games_lasso_df$age[games_lasso_df$age != 1])
games_lasso_df$age_indicator <- as.numeric(games_lasso_df$age == 1)
games_lasso_df[games_lasso_df$age == 1, "age"] <- mean_age

# add new category for missing data in the categorical survey responses
non_survey <- c("gameId", "feedback_type", "ability", "effort")
games_lasso_df[,!(colnames(games_lasso_df) %in% non_survey)][games_lasso_df[,!(colnames(games_lasso_df) %in% non_survey)] == ""] <- "not available"

# get regressors
games_x <- model.matrix(~., games_lasso_df %>% select(-effort))

# get response variable
games_y <- games_lasso_df %>% pull(effort)

# lasso regression
set.seed(23)
lasso_model <- cv.glmnet(x=games_x, y=games_y, alpha=1)
best_lambda <- lasso_model$lambda.min
best_lambda
plot(lasso_model)
lasso_model_best_lambda <- glmnet(x=games_x, y=games_y, alpha=1, lambda = best_lambda)
coef(lasso_model_best_lambda)



# models for round results

rounds_mod1 <- lm(effort_change ~ round_number 
                  + factor(playerId), 
                  data = round_results)
summary(rounds_mod1)


rounds_mod2 <- lm(effort_change ~ factor(round_number)
                  + factor(playerId), 
                  data = round_results)
summary(rounds_mod2)

rounds_mod3 <- lm(effort_change ~ round_number 
                  + lag_rank 
                  + factor(feedback_type) 
                  + lag_rank:factor(feedback_type) 
                  + factor(playerId), 
                  data = round_results)
summary(rounds_mod3)

rounds_mod4 <- lm(effort_change ~ round_number 
                  + factor(lag_rank) 
                  + factor(feedback_type) 
                  + factor(lag_rank):factor(feedback_type) 
                  + factor(playerId), 
                  data = round_results)
summary(rounds_mod4)

rounds_mod5 <- lm(effort_change ~ factor(round_number)
                  + lag_rank
                  + factor(feedback_type)
                  + lag_rank:factor(feedback_type)
                  + factor(playerId),
                  data = round_results)
summary(rounds_mod5)

rounds_mod6 <- lm(effort_change ~ third_round
                  + fourth_round
                  + fifth_round
                  + factor(lag_rank)
                  + factor(feedback_type)
                  + factor(lag_rank):factor(feedback_type)
                  + factor(playerId),
                  data = round_results)
summary(rounds_mod6)
