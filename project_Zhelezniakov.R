library(ggthemes)
library(showtext)
library(ggthemes)

# Load Data
prison_population_df <- read_csv("prison_population.csv")
prison_summary_df <- read_csv("prison_summary.csv")
pretrial_population_df <- read_csv("pretrial_population.csv")

states_map <- map_data("state")


# Change State Names NECESSARY
states_map <- as_tibble(states_map)

prison_population_df <- prison_population_df |> 
  mutate(state = tolower(state.name[match(state, state.abb)]))

pretrial_population_df <- pretrial_population_df |> 
  mutate(state = tolower(state.name[match(state, state.abb)]))

# Visualization
font_add_google("Roboto", "roboto")

showtext_auto()
#  Map cart of prison population

#1
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
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
        panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "grey75", 
                                         colour = "black", 
                                         size = 1, 
                                         linetype = "solid"),
        text = element_text(family = "roboto")) 

#3
trends |> 
  filter(!is.na(total_jail_pop) & !is.na(total_prison_pop)) |> 
  select(total_pop, total_jail_pop, total_prison_pop, urbanicity) |>
  mutate(Joil = total_jail_pop / total_pop * 100000,
         Prison = total_prison_pop/ total_pop * 100000) |> 
  pivot_longer(cols = Joil:Prison, 
               names_to = "joil_or_prison", values_to ="per_100k") |> 
  ggplot(aes(urbanicity, per_100k, fill = joil_or_prison)) +
  geom_col(position = "dodge") +
  theme_economist() +
  scale_fill_economist() +
  theme(legend.title = element_blank()) +
  labs(y = "Per 100k", 
       x = "Urbanicity Type",
       caption = "From Incarceration Trends Dataset",
       title = "Amount Joils and Prisoners for Urbanicity Type")
#4
trends |> 
  filter(!is.na(total_jail_pop) & !is.na(total_prison_pop)) |> 
  mutate(Prison = total_prison_pop / total_pop * 100000,
         decade = factor((year %/% 10) * 10),
         North_South = case_when(state %in% c("AL", "AR", "DE", "FL", "GA", "KY", "LA", 
                                              "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV", "DC") ~ "South",
                                 TRUE ~ "North")) |> 
  summarise(.by = c(decade, North_South),
            Prison = mean(Prison, rm.na = T)) |> 
  ggplot(aes(decade, Prison)) +
  geom_col(aes(y = Prison, fill = North_South), position = "dodge", stat = "identity") +
  geom_line(aes(y = Prison, group = North_South), linewidth = 2.5,
            position = position_dodge(width = 0.9),
            color = "black") +
  theme_economist() +
  theme(legend.title = element_blank()) +
  scale_fill_economist() +
  labs(
    x = "Decade",
    y = "Prisoners per 100k",
    title = "North vs Sout states amount of prisonres per 100k",
    subtitle = "over decades",
    caption = "From Incarceration Trends Dataset")
