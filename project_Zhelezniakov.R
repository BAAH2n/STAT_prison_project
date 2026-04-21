# Load Data
prison_population_df <- read_csv("prison_population.csv")
prison_summary_df <- read_csv("prison_summary.csv")
pretrial_population_df <- read_csv("pretrial_population.csv")

states_map <- map_data("state")
# Data Explore
prison_population_df |> 
  filter(!is.na(prison_population)) |> 
  select(prison_population)

prison_population_df |> 
  count(state) |> 
  head(51)

prison_population_df |> select(state)

prison_summary |> select(pop_category)

prison_population_df |> 
  filter(!is.na(prison_population)) |> 
  select(prison_population)
# Change State Names NECESSARY
states_map <- as_tibble(states_map)

prison_population_df <- prison_population_df |> 
  mutate(state = tolower(state.name[match(state, state.abb)]))

pretrial_population_df <- pretrial_population_df |> 
  mutate(state = tolower(state.name[match(state, state.abb)]))
# Check why not all states in graph. And other problems with states

states_map_count_state <- states_map |> 
  count(region)

pretrial_population_df |> select(state)

prison_population_df_count_state <-prison_population_df |> 
  filter(!is.na(prison_population)) |> 
  count(state)

states_map_count_state |> 
  inner_join(prison_population_df_count_state, by = c("region" = "state"))

prison_population_df |> 
  filter(!is.na(prison_population)) |> 
  filter(prison_population == 0) |> 
  count(state)
# Visualization
font_add_google("Roboto", "roboto")

showtext_auto()
#  Map cart of prison population

prison_population_df |> 
  filter(!is.na(prison_population)) |> 
  ggplot(aes(map_id = state)) +
  geom_map(aes(fill = prison_population), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) + 
  scale_fill_distiller(palette = "Blues",  direction = 1,      
                       trans = "log10") +
  labs(title = "Amount of prisoners in US",
       fill = "Prison Population") +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
        panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "grey75", 
                                         colour = "black", 
                                         size = 1, 
                                         linetype = "solid"),
        text = element_text(family = "roboto"))

# Map cart of prison population
pretrial_population_df |> 
  filter(!is.na(pretrial_population)) |> 
  ggplot(aes(map_id = state)) +
  geom_map(aes(fill = pretrial_population), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) + 
  scale_fill_distiller(palette = "Blues",  direction = 1,      
                       trans = "log10") +
  labs(title = "Amount of pretrials in US",
         fill = "Pretrial Population") +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
        panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "grey75", 
                                         colour = "black", 
                                         size = 1, 
                                         linetype = "solid"),
        text = element_text(family = "roboto"),
        axis.ticks = element_blank())

