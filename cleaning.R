library(tidyverse)

# load data and some initial cleaning
games <- read_csv("games.csv") %>%
  rename(id = "X_id")
rounds <- read.csv("rounds.csv") %>%
  rename(roundId = "X_id", round_number = "index")
players <- read.csv("players.csv") %>%
  rename(playerId = "X_id")
player_rounds <- read.csv("player-rounds.csv")
inputs <- read.csv("player-inputs.csv") %>%
  select(-c(X_id, gameId, createdAt))
treatments <- read.csv("treatments.csv") %>%
  rename(treatmentId = "X_id", feedback_type = "name")



# create data frame of player IDs and their respective game ID and treatment ID

# initialize empty vectors
playerId <- c()
gameId <- c()
treatmentId <- c()

# fill the vectors
for (i in 1:nrow(games)) {
  ids <- unlist(strsplit(games$playerIds[i], ","))
  for (id in ids) {
    playerId <- append(playerId, id)
    gameId <- append(gameId, games$id[i])
    treatmentId <- append(treatmentId, games$treatmentId[i])
  }
}

# create data frame and save as csv
games <- data.frame(playerId, gameId, treatmentId)
write_csv(games, "players_per_game.csv")



# make a dataframe of overall game results per each player, including final scores
# and survey responses

# merge dataframes and select needed columns
game_results <- merge(games, players, by = "playerId", all.x = TRUE, all.y = FALSE) %>%
  merge(treatments, by = "treatmentId", all.x = TRUE, all.y = FALSE) %>%
  merge(inputs, by = "playerId", all.x = TRUE, all.y = FALSE) %>%
  select(playerId, gameId, feedback_type, starts_with("data")) %>%
  select(-c(data.avatar))

# rename columns
names(game_results) <- gsub("data.", "", names(game_results))
game_results <- rename(game_results, Q5 = "multipleChoice")

# add column for total number of sliders adjusted
game_results <- game_results %>%
  mutate(effort = round(score * numSliders * 5))

# save dataframe in a csv file
write_csv(game_results, "game_results.csv")



# make a dataframe holding the round results per player, including just the results
# from each round and not the final results of the games

# merge dataframes and select needed columns
round_results <- merge(games, player_rounds, by = "playerId", all.x = TRUE, all.y = FALSE) %>%
  merge(rounds, by = "roundId", all.x = TRUE, all.y = FALSE) %>%
  merge(players, by = "playerId", all.x = TRUE, all.y = FALSE) %>%
  merge(treatments, by = "treatmentId", all.x = TRUE, all.y = FALSE) %>%
  select(playerId, gameId, feedback_type, round_number, data.ability, data.numSliders, data.value) %>%
  group_by(playerId) %>%
  arrange(round_number, .by_group = TRUE) %>%
  ungroup()

# make round number start at 1 instead of 0
round_results$round_number <- round_results$round_number + 1

# create separate columns for the different values currently all scored in data.value
effort <- c()
round_score <- c()
rank <- c()
for (i in 1:nrow(round_results)) {
  round_value <- unlist(strsplit(round_results$data.value[i], ","))
  num_sliders <- round_results$data.numSliders[i]
  effort[length(effort) + 1] <- as.numeric(round_value[num_sliders + 1])
  round_score[length(round_score) + 1] <- as.numeric(round_value[num_sliders + 2])
  rank[length(rank) + 1] <- as.numeric(round_value[length(round_value)])
}
round_results$effort <- effort
round_results$round_score <- round_score
round_results$rank <- rank

# rename columns
names(round_results) <- gsub("data.", "", names(round_results))

# make lag variable and change in effort between rounds
round_results <- round_results %>%
  group_by(playerId) %>%
  mutate(lag_effort = lag(effort), 
         lag_rank = lag(rank)) %>%
  ungroup() %>%
  mutate(effort_pct_change = (effort - lag_effort) / lag_effort,
         effort_change = effort - lag_effort)

# save dataframe in a csv file
write_csv(round_results, "round_results.csv")
