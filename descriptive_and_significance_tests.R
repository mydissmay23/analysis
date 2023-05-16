library(ggplot2)
library(tidyverse)
library(rcompanion)

# load data and rename some variables
round_results <- read_csv("round_results.csv")
game_results <- read_csv("game_results.csv")



# descriptive statistics for number of sliders correctly adjusted to compare
# with past research

# mean, median, min, and max effort in the first round
round1 <- round_results$effort[round_results$round_number == 1]
mean(round1)
median(round1)
min(round1)
max(round1)

# mean, median, min, and max effort of all rounds total by absolute feedback participants
abs <- round_results$effort[round_results$feedback_type == "absolute"]
mean(abs)
median(abs)
min(abs)
max(abs)

# mean, median, min, and max effort in each round for absolute feedback participants
for (i in 1:5) {
  print(i)
  data <- round_results$effort[round_results$feedback_type == "absolute" & round_results$round_number == i]
  print(mean(data))
  print(median(data))
  print(min(data))
  print(max(data))
}

# mean, median, min, and max of effort in the first round by ability level
round1_high <- round_results$effort[round_results$round_number == 1 & round_results$ability == "high"]
mean(round1_high)
median(round1_high)
min(round1_high)
max(round1_high)
round1_low <- round_results$effort[round_results$round_number == 1 & round_results$ability == "low"]
mean(round1_low)
median(round1_low)
min(round1_low)
max(round1_low)

# histogram of effort in first round by ability level
round_results %>%
  filter(round_number == 1) %>%
  ggplot(aes(x = effort)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20)
  scale_x

# histogram of effort in all rounds for absolute feedback participants by ability level
round_results %>%
  filter(feedback_type == "absolute") %>%
  ggplot(aes(x = effort)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20)

# calculate percentage that adjusted 15 or more sliders in a round
# of everyone in the first round
nrow(filter(round_results, effort >= 15 & round_number == 1)) / 
  nrow(filter(round_results, round_number == 1)) * 100
# of people with absolute feedback in any given round
nrow(filter(round_results, effort >= 15 & feedback_type == "absolute")) / 
  nrow(filter(round_results, feedback_type == "absolute")) * 100



# check whether "high ability" participants did actually score and rank higher 
# as intended

# mean and median scores of high and low ability participants in the first round
high <- round_results$round_score[round_results$round_number == 1 & round_results$ability == "high"]
mu_high <- mean(high)
med_high <- median(high)
low <- round_results$round_score[round_results$round_number == 1 & round_results$ability == "low"]
mu_low <- mean(low)
med_low <- median(low)

# function to calculate p value with z score
p_value <- function(high, low) {
  se_high <- sd(high) / sqrt(length(high))
  se_low <- sd(low) / sqrt(length(low))
  z <- (mean(high) - mean(low)) / sqrt(se_high^2 + se_low^2)
  return (pnorm(z, lower.tail = FALSE))
}

# z-test based on scores in the first round
p <- p_value(high, low)



# histograms of scores by ability level

# using all participants' first round results
round_results %>%
  filter(round_number == 1) %>%
  ggplot(aes(x = round_score, fill = ability)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  xlab("round score")

# using all round results of participants that received absolute feedback
round_results %>%
  filter(feedback_type == "absolute") %>%
  ggplot(aes(x = round_score, fill = ability)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  xlab("round score")


# charts of ranks by ability level

# using all participants' first round results
round_results %>%
  filter(round_number == 1) %>%
  ggplot(aes(x = factor(rank), fill = ability)) +
  geom_bar(alpha = 0.5, position = "identity") +
  xlab("rank")

# using all round results of participants that received absolute feedback
round_results %>%
  filter(feedback_type == "absolute") %>%
  ggplot(aes(x = factor(rank), fill = ability)) +
  geom_bar(alpha = 0.5, position = "identity") +
  xlab("rank")



# examine instances where participants scored 0

# make a slider value column of numeric vectors
slider_values <- list()
for (i in 1:nrow(round_results)) {
  round_value <- unlist(strsplit(round_results$value[i], ","))
  num_sliders <- round_results$numSliders[i]
  slider_values[[length(slider_values) + 1]] <- as.numeric(round_value[1:num_sliders])
}
round_results$slider_values <- slider_values

# examine the observations where someone correctly adjusted 0 sliders in the first round
zeros1 <- round_results %>%
  filter(effort == 0 & round_number == 1)

# when someone that received absolute feedback correctly adjusted 0 sliders in a round
zeros_abs <- round_results %>%
  filter(effort == 0 & feedback_type == "absolute")

# number of participants that didn't correctly adjust any sliders in any round
# absolute feedback, high ability
nrow(filter(game_results, score == 0, ability == "high", feedback_type == "absolute"))
# absolute feedback, low ability
nrow(filter(game_results, score == 0, ability == "low", feedback_type == "absolute"))
# relative feedback, high ability
nrow(filter(game_results, score == 0, ability == "high", feedback_type == "relative"))
# relative feedback, low ability
nrow(filter(game_results, score == 0, ability == "low", feedback_type == "relative"))



# examine player's rankings and how much a player's ranking changed across the game

# find average rank, minimum rank, maximum rank, and the range between minimum and maximum per person
rank_ranges <- round_results %>%
  group_by(playerId) %>%
  summarise(
    worst_rank = max(rank), 
    best_rank = min(rank), 
    rank_range = max(rank) - min(rank),
    average_rank = mean(rank))

# plot rank ranges
ggplot(rank_ranges, aes(x = factor(rank_range))) +
  geom_bar() +
  xlab("Difference between highest and lowest rank per player")

rank_ranges <- merge(rank_ranges, game_results, by="playerId")
ggplot(rank_ranges, aes(x = factor(rank_range), fill = ability)) +
  geom_bar(alpha = 0.5, position = "identity")
mean(rank_ranges$rank_range)



# compare the effort of low ability participants that received absolute vs relative feedback

# get low ability observations
low_df <- game_results %>%
  filter(ability == "low")

# plot
low_df %>%
  ggplot(aes(x = effort, fill = feedback_type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20)

# t-test
low_abs <- low_df$effort[low_df$feedback_type == "absolute"]
low_rel <- low_df$effort[low_df$feedback_type == "relative"]
t.test(low_abs, low_rel, alternative = "greater", var.equal = TRUE)


# compare the effort of high ability participants receiving absolute and relative feedback

# get high ability observations
high_df <- game_results %>%
  filter(ability == "high")

# plot
high_df %>%
  ggplot(aes(x = effort, fill = feedback_type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20)

# t-test
high_abs <- high_df$effort[high_df$feedback_type == "absolute"]
high_rel <- high_df$effort[high_df$feedback_type == "relative"]
t.test(high_rel, high_abs, alternative = "greater", var.equal = TRUE)


# compare the effort of low and high ability participants receiving relative feedback

# get relative feedback observations
rel_df <- game_results %>%
  filter(feedback_type == "relative")

# plot
rel_df %>%
  ggplot(aes(x = effort, fill = ability)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20)

# t-test
rel_high <- rel_df$effort[rel_df$ability == "high"]
rel_low <- rel_df$effort[rel_df$ability == "low"]
t.test(rel_high, rel_low, alternative = "greater", var.equal = TRUE)
t.test(rel_high, rel_low, alternative = "less", var.equal = TRUE)


# compare coefficient of variations in effort for relative feedback groups and absolute feedback groups

# calculate coefficient of variation for effort in each game
cv_abs <- game_results %>%
  filter(feedback_type == "absolute") %>%
  group_by(gameId) %>%
  summarise(cv = sd(effort) / mean(effort)) %>%
  mutate(feedback_type = "absolute")

cv_rel <- game_results %>%
  filter(feedback_type == "relative") %>%
  group_by(gameId) %>%
  summarise(cv = sd(effort) / mean(effort)) %>%
  mutate(feedback_type = "relative")

cv_df <- rbind(cv_abs, cv_rel)

# t-test to see if coefficient of variation is higher for relative feedback groups
t.test(cv_rel$cv, cv_abs$cv, alternative = "greater", var.equal = TRUE)



# additional plots
ggplot(round_results, aes(x=round_number, y=effort_change, color=feedback_type, shape=ability)) + 
  geom_point(alpha = 0.5, size = 5) +
  scale_x_continuous(breaks=1:5)

ggplot(round_results, aes(x=lag_rank, y=effort_change, color=feedback_type, shape=ability)) +
  geom_point(alpha = 0.5, size = 5) +
  scale_x_continuous(breaks=1:10)
