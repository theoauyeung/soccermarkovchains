library(StatsBombR)  
library(tidyverse)   
library(igraph)     
library(ggplot2)     
library(dtplyr)      
library(Matrix)
library(ggthemes)
library(lme4)
library(ggrepel)
library(ggshakeR)
library(soccerplotR)
library(soccerAnimate)


Comp<- StatsBombR::FreeCompetitions() |>                                                         #
  dplyr::filter(competition_name == "Premier League")
matches <- StatsBombR::FreeMatches(Comp)
data <- StatsBombR::free_allevents(MatchesDF = matches, Parallel = TRUE)
data <- allclean(data)

plottingData <- data

plottingData <- plottingData %>%
  mutate(
    x = as.numeric(location.x),
    y = as.numeric(location.y),
    finalX = case_when(
      type.name == "Pass" ~ as.numeric(pass.end_location.x),
      type.name == "Carry" ~ as.numeric(carry.end_location.x),
      TRUE ~ NA_real_
    ),
    finalY = case_when(
      type.name == "Pass" ~ as.numeric(pass.end_location.y),
      type.name == "Carry" ~ as.numeric(carry.end_location.y),
      TRUE ~ NA_real_
    )
  )

xTData <- calculate_threat(data = plottingData, type = "statsbomb")

EPVData <- calculate_epv(plottingData, type = "statsbomb")
## NOTE: from version 0.2.0 ALL function arguments are now in snake_case
## 'dataType' is now 'data_type', etc.


EPVData <- EPVData %>%
  mutate(EPV = EPVEnd - EPVStart)



xTData <- xTData %>%
  mutate(xT = xTEnd - xTStart)


# Calculate minutes played per player
minutes_played <- data %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(
    minutes = max(minute) - min(minute) + 1,
    .groups = 'drop'
  )

# Calculate progressive passes xT
progressive_passes <- xTData %>%
  filter(type.name == "Pass", 
         pass.outcome.name != "Incomplete",
         !is.na(xT)) %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(
    total_pass_xt = sum(xT, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate progressive carries xT
progressive_carries <- xTData %>%
  filter(type.name == "Carry",
         !is.na(xT)) %>%
  group_by(player.id, player.name, team.name) %>%
  summarise(
    total_carry_xt = sum(xT, na.rm = TRUE),
    .groups = 'drop'
  )

# Combine all stats
combined_stats <- minutes_played %>%
  full_join(progressive_passes, by = c("player.id", "player.name", "team.name")) %>%
  full_join(progressive_carries, by = c("player.id", "player.name", "team.name")) %>%
  mutate(
    total_pass_xt = replace_na(total_pass_xt, 0),
    total_carry_xt = replace_na(total_carry_xt, 0),
    pass_xt_per_90 = total_pass_xt / (minutes/90),
    carry_xt_per_90 = total_carry_xt / (minutes/90),
    combined_xt_per_90 = pass_xt_per_90 + carry_xt_per_90
  ) %>%
  filter(minutes >= 90) %>%
  arrange(desc(combined_xt_per_90))

# Create passing visualization
top_passers_plot <- combined_stats %>%
  arrange(desc(pass_xt_per_90)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(player.name, pass_xt_per_90), y = pass_xt_per_90)) +
  geom_bar(stat = "identity", fill = "#ff4b4b", alpha = 0.8) +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(
    title = "Most Threatening Passers Premier League 2003/04 & 2015/16",
    subtitle = "Players with 90+ minutes played",
    x = "",
    y = "Open play xT from passes per 90",
    caption = "Data: StatsBomb"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#5A5A5A"),
    panel.background = element_rect(fill = "#5A5A5A"),
    title = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white") 
  )

top_passers_plot

# Create combined passes and carries visualization
top_progressors_plot <- combined_stats %>%
  arrange(desc(combined_xt_per_90)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(player.name, combined_xt_per_90), y = combined_xt_per_90)) +
  geom_bar(stat = "identity", fill = "#ff4b4b", alpha = 0.8) +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(
    title = "Best Ball Progressors in Premier League 2003/04 & 2015/16",
    subtitle = "Players with 90+ minutes played",
    x = "",
    y = "Open play xT from passes and carries per 90",
    caption = "Data: StatsBomb"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#5A5A5A"),
    panel.background = element_rect(fill = "#5A5A5A"),
    title = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white") 
  )

top_progressors_plot


events <-data %>%
  unnest(location, names_sep = ".") %>%
  mutate(
    field_third = case_when(
      location <= 33 ~ "defensive",
      location <= 66 ~ "middle",
      TRUE ~ "attacking"
    ),
    team_possession = if_else(team.id == data$team.id[1], "home", "away"),
    shot_opportunity = !is.na(shot.statsbomb_xg),
    state = paste(team_possession, field_third, shot_opportunity)
  )

# Create state transition matrix
states <- unique(events$state)
n_states <- length(states)
transition_matrix <- matrix(0, nrow = n_states, ncol = n_states)
rownames(transition_matrix) <- states
colnames(transition_matrix) <- states

# Fill transition matrix
for(i in 1:(nrow(events)-1)) {
  current_state <- events$state[i]
  next_state <- events$state[i+1]
  current_idx <- which(states == current_state)
  next_idx <- which(states == next_state)
  transition_matrix[current_idx, next_idx] <- transition_matrix[current_idx, next_idx] + 1
}

# Convert to probabilities
transition_matrix <- transition_matrix / rowSums(transition_matrix)


# Calculate expected goals value for each state
state_xg <- sapply(states, function(s) {
  mean(events$shot.statsbomb_xg[events$state == s], na.rm = TRUE)
})
# Calculate scoring probabilities looking forward N events
n_look_ahead <- 3  # Look ahead 3 events

state_probabilities <- data.frame(
  state = states,
  scoring_prob = sapply(states, function(s) {
    # Find all instances of this state
    state_indices <- which(events$state == s)
    
    # Look ahead N events for each instance
    forward_probs <- sapply(state_indices, function(idx) {
      # Get next N events' xG values
      next_xg <- events$shot.statsbomb_xg[idx:(idx + n_look_ahead)]
      # Return max xG in that sequence
      max(c(next_xg, 0), na.rm = TRUE)
    })
    
    # Return mean of forward-looking probabilities
    mean(forward_probs, na.rm = TRUE)
  })
) %>%
  arrange(desc(scoring_prob))


# player_impact_with_scoring <- events %>%
#   filter(player.name != "Patrik Berger") %>% 
#   group_by(player.name) %>%
#   summarise(
#     transitions = n(),
#     # Calculate the average change in scoring probability per transition
#     avg_scoring_prob_added = mean(
#       lead(state_probabilities$scoring_prob[match(state, state_probabilities$state)], 
#            default = last(state_probabilities$scoring_prob[match(state, state_probabilities$state)])) -
#         state_probabilities$scoring_prob[match(state, state_probabilities$state)],
#       na.rm = TRUE
#     ),
#     # Calculate total impact as average change times number of transitions
#     total_scoring_prob_added = avg_scoring_prob_added * transitions
#   ) %>%
#   filter(transitions >= 10000) %>%  # Minimum number of transitions to be considered
#   arrange(desc(total_scoring_prob_added))
# 
# # Create improved visualization
# ggplot(player_impact_with_scoring, 
#        aes(x = transitions, y = avg_scoring_prob_added)) +
#   geom_point(aes(size = total_scoring_prob_added, 
#                  color = total_scoring_prob_added > median(total_scoring_prob_added))) +
#   geom_text(aes(label = ifelse(abs(total_scoring_prob_added) > 
#                                  quantile(abs(total_scoring_prob_added), 0.95), 
#                                player.name, "")),
#             vjust = 1.5,
#             size = 3) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(),
#         legend.position = "none") +
#   labs(title = "Player Impact: Average Scoring Probability Added vs Transitions",
#        x = "Number of Transitions",
#        y = "Average Scoring Probability Added per Transition")
# 
# # Create visualizations
# # 1. State scoring probabilities
# ggplot(state_probabilities, aes(x = reorder(state, scoring_prob), y = scoring_prob)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   labs(title = "Scoring Probability by State",
#        x = "State",
#        y = "Scoring Probability")



# Calculate player impact
player_impact <- events %>%
  group_by(player.name) %>%
  summarise(
    transitions = n(),
    positive_transitions = sum(
      lead(field_third) == "attacking" |
        lead(shot_opportunity) == TRUE,
      na.rm = TRUE
    ),
    positive_transition_rate = positive_transitions / transitions,
    total_xg_added = sum(
      ifelse(
        !is.na(shot.statsbomb_xg),
        shot.statsbomb_xg,
        0
      ),
      na.rm = TRUE
    ),
    xg_per_transition = total_xg_added / transitions
  ) %>%
  filter(transitions >= 5) %>%  
  arrange(desc(xg_per_transition))





transition_df <- as.data.frame(transition_matrix) %>%
  rownames_to_column("from_state") %>%
  pivot_longer(-from_state, names_to = "to_state", values_to = "probability")


# Create a comprehensive transition probability table
transition_scoring_df <- transition_df %>%
  left_join(
    state_probabilities %>% 
      rename(from_scoring_prob = scoring_prob), 
    by = c("from_state" = "state")
  ) %>%
  left_join(
    state_probabilities %>% 
      rename(to_scoring_prob = scoring_prob), 
    by = c("to_state" = "state")
  ) %>%
  mutate(
    scoring_prob_change = to_scoring_prob - from_scoring_prob,
    probability = round(probability, 4)
  ) %>%
  filter(probability > 0) %>%
  arrange(desc(probability)) %>%
  select(
    From = from_state,
    To = to_state,
    Transition_Prob = probability,
    From_Scoring_Prob = from_scoring_prob,
    To_Scoring_Prob = to_scoring_prob,
    Scoring_Prob_Change = scoring_prob_change
  )

# # Print the first few rows using knitr for nice formatting
# library(knitr)
# kable(head(transition_scoring_df, 20),
#       digits = 4,
#       caption = "Top 20 Most Common State Transitions with Scoring Probabilities")


ggplot(player_impact,
       aes(x = transitions, y = total_xg_added)) +
  geom_point(aes(color = transitions >= 14500 | total_xg_added > 30)) +
  geom_text(aes(label = ifelse(transitions >= 14500 | total_xg_added > 30, player.name, "")),
            vjust = 1.5,
            size = 3) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        legend.position = "none") +
  labs(title = "Player Impact: Total xG Added vs Number of Transitions",
       x = "Number of Transitions",
       y = "Total xG Added")




# Improved reward function and value iteration implementation


reward <- dplyr::cross_join(
  events %>% distinct(state) %>% rename(state_pre = state),
  events %>% distinct(state) %>% rename(state_post = state)) %>%
  dplyr::left_join(
    events %>% 
      group_by(state) %>% 
      summarise(
        avg_xg = mean(shot.statsbomb_xg, na.rm = TRUE),
        is_shot = any(!is.na(shot.statsbomb_xg))
      ),
    by = c("state_post" = "state")
  ) %>%
  dplyr::mutate(
    reward = case_when(
      is_shot ~ avg_xg * 1.5,
      # Give smaller rewards for entering attacking third
      str_detect(state_post, "attacking") ~ 0.2,
      # Give minimal rewards for maintaining possession
      str_detect(state_post, str_extract(state_pre, "home|away")) ~ 0.05,
      # Penalize losing possession
      TRUE ~ -0.1
    )
  ) %>%
  select(state_pre, state_post, reward)


value_initial <- events %>%
  distinct(state) %>%
  mutate(value = 0)

# Create terminal states 
value_terminal <- events %>%
  filter(!is.na(shot.outcome.name) & shot.outcome.name == "Goal") %>%
  distinct(state) %>%
  left_join(
    events %>% 
      group_by(state) %>% 
      summarise(avg_xg = mean(shot.statsbomb_xg, na.rm = TRUE)),
    by = "state"
  ) %>%
  mutate(value = 1 + avg_xg)  

# Use existing transition probability matrix but convert to dplyr format
transition_prob <- transition_df %>%
  rename(
    state_pre = from_state,
    state_post = to_state,
    probability = probability
  ) %>%
  filter(probability > 0)  

# Perform value iteration with proper convergence
gamma <- 0.95  # Slightly higher discount factor
max_delta <- Inf
threshold <- 1e-6  # Stricter convergence threshold
max_iterations <- 1000  # Add maximum iterations as safety
iteration <- 0
value_previous <- value_initial

while(max_delta > threshold && iteration < max_iterations) {
  value <- transition_prob %>%
    dplyr::left_join(reward, by = c("state_pre", "state_post")) %>%
    dplyr::left_join(value_previous, by = c("state_post" = "state")) %>%
    dplyr::group_by(state = state_pre) %>%
    dplyr::summarize(value = weighted.mean(reward + gamma * value, w = probability)) %>%
    dplyr::bind_rows(value_terminal)
  
  max_delta <- value_previous %>%
    dplyr::inner_join(value, by = "state", suffix = c("_before", "_after")) %>%
    dplyr::summarize(max_delta = max(abs(value_before - value_after))) %>%
    pull(max_delta)
  
  value_previous <- value
  iteration <- iteration + 1
  
  # Print progress every 100 iterations
  if(iteration %% 100 == 0) {
    print(paste("Iteration:", iteration, "Max delta:", max_delta))
  }
}




player_impact_with_values <- events %>%
  filter(!is.na(player.name)) %>%
  mutate(
    current_state_value = value$value[match(state, value$state)],
    next_state_value = lead(value$value[match(state, value$state)]),
    value_diff = next_state_value - current_state_value
  ) %>%
  group_by(player.name) %>%
  summarise(
    transitions = n(),
    # Calculate individual components separately for debugging
    total_value_change = sum(value_diff, na.rm = TRUE),
    avg_value_added = total_value_change / transitions,
    xg_contribution = sum(ifelse(!is.na(shot.statsbomb_xg), shot.statsbomb_xg, 0), na.rm = TRUE),
    xg_per_transition = xg_contribution / transitions,
    # Calculate total value added incorporating both state transitions and xG
    total_value_added = total_value_change + xg_contribution
  ) %>%
  filter(transitions >= 5000) %>%
  arrange(desc(total_value_added))


ggplot(player_impact_with_values, 
       aes(x = transitions, y = avg_value_added)) +
  geom_point(aes(size = total_value_added, 
                 color = transitions >= median(transitions)), 
             alpha = 0.7) +
  geom_text_repel(aes(label = ifelse(
   
    transitions >= quantile(transitions, 0.95) |  # Top 5% by transitions
      total_value_added >= quantile(total_value_added, 0.95), # Top 5% by value added
    player.name, 
    "")),
    size = 3,
    max.overlaps = 15) +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  ) +
  labs(
    title = "Player Impact Analysis: Average Value Added vs Number of Transitions",
    subtitle = "Size indicates total value added; highlighted points show above-median transition counts",
    x = "Number of Transitions",
    y = "Average Value Added per Transition"
  ) +

  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = c("#404040", "#008fd5"))


# Combine the xT and EPV data with player information
player_impact_advanced <- xTData %>%
  left_join(EPVData, by = names(.)[names(.) %in% names(EPVData)]) %>%
  group_by(player.name) %>%
  summarise(
    transitions = n(),
    total_xt = sum(xT, na.rm = TRUE),
    avg_xt = mean(xT, na.rm = TRUE),
    total_epv = sum(EPV, na.rm = TRUE),
    avg_epv = mean(EPV, na.rm = TRUE)
  ) %>%
  filter(transitions >= 5000) %>%  # Filter for minimum number of actions
  mutate(
    # Scale values after filtering and handle NAs
    scaled_xt = scale(total_xt)[,1],
    scaled_epv = scale(total_epv)[,1],
    combined_score = (scaled_xt + scaled_epv)/2
  ) %>%
  arrange(desc(combined_score))

# Create visualization with proper NA handling
ggplot(player_impact_advanced, 
       aes(x = total_xt, y = total_epv)) +
  geom_point(aes(size = transitions,
                 color = combined_score),
             na.rm = TRUE) +
  geom_text_repel(
    aes(label = ifelse(
      combined_score >= quantile(combined_score, 0.9, na.rm = TRUE) |  # Top 10% by combined score
        transitions >= quantile(transitions, 0.9, na.rm = TRUE),          # Top 10% by transitions
      player.name, 
      ""
    )),
    size = 3,
    max.overlaps = 15,
    na.rm = TRUE
  ) +
  scale_color_gradient(low = "#404040", high = "#008fd5") +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  ) +
  labs(
    title = "Player Performance Analysis: Expected Threat vs Expected Possession Value",
    subtitle = "Size indicates number of actions; color intensity shows combined performance score",
    x = "Total Expected Threat (xT)",
    y = "Total Expected Possession Value (EPV)"
  )




Comp2<- StatsBombR::FreeCompetitions() |>                                                         #
  dplyr::filter(competition_name == "La Liga",
                season_id == 2)
matches2 <- StatsBombR::FreeMatches(Comp2)
ligadata <- StatsBombR::free_allevents(MatchesDF = matches2, Parallel = TRUE)
ligadata <- allclean(ligadata)

# # Find the specific El Clásico match
# clasico <- matches2 |>
#   dplyr::filter(home_team.home_team_name == "Real Madrid",
#                 away_team.away_team_name == "Barcelona",
#                 match_date == "2017-04-23")
# 
# print(clasico$match_id)


# Filter for the El Clásico match and the sequence leading to Messi's goal
messi_winner <- ligadata |>
  dplyr::filter(match_id == 267569,  # You might need to verify this match_id
                minute >= 91,
                minute <= 93,
                team.name == "Barcelona") |>
  dplyr::arrange(timestamp) 
  #dplyr::select(timestamp, player.name, type.name, possession, minute, second, 
  #            possession_team.name, play_pattern.name, location, position.name)

messiplottingData <- messi_winner

messiplottingData <- messiplottingData %>%
  mutate(
    x = as.numeric(location.x),
    y = as.numeric(location.y),
    finalX = case_when(
      type.name == "Pass" ~ as.numeric(pass.end_location.x),
      type.name == "Carry" ~ as.numeric(carry.end_location.x),
      TRUE ~ NA_real_
    ),
    finalY = case_when(
      type.name == "Pass" ~ as.numeric(pass.end_location.y),
      type.name == "Carry" ~ as.numeric(carry.end_location.y),
      TRUE ~ NA_real_
    )
  )

messiEPVData <- calculate_epv(messiplottingData, type = "statsbomb")
## NOTE: from version 0.2.0 ALL function arguments are now in snake_case
## 'dataType' is now 'data_type', etc.


messiEPVData <- messiEPVData %>%
  mutate(EPV = EPVEnd - EPVStart)

messixTData <- calculate_threat(data = messiplottingData, type = "statsbomb")

messixTData <- messixTData %>%
  mutate(xT = xTEnd - xTStart)


messi_advanced <- messixTData %>%
  left_join(messiEPVData, by = names(.)[names(.) %in% names(messiEPVData)]) %>%
  group_by(player.name) %>%
  summarise(
    transitions = n(),
    total_xt = sum(xT, na.rm = TRUE),
    total_epv = sum(EPV, na.rm = TRUE),
  ) %>%
  #filter(transitions >= 5000) %>%  # Filter for minimum number of actions
  mutate(
    # Scale values after filtering and handle NAs
    scaled_xt = scale(total_xt)[,1],
    scaled_epv = scale(total_epv)[,1],
    combined_score = (scaled_xt + scaled_epv)/2
  ) %>%
  arrange(desc(combined_score))



# Example A: "base"
soccer_animate(messixTData, 480, 490, "base", export_gif = T)
