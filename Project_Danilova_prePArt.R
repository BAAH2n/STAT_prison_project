library(tidyverse)
library(sf)
library(tigris)
library(plotly)
library(scales)

trends <- read_csv("incarceration_trends.csv")

trends |> 
  summary()

trends |> 
  head()
trends |> 
  select(capacity, total_jail_pop) |>
  summary()
#Overcrowding Heatmaps: Systemic failure or localized crisis?
#ails and prisons have a maximum rated capacity. Which states are chronically 
#running their facilities over 100% capacity, and is it getting worse over time?

#prepare the data
heatmap_data |> select(year) |> 
  arrange(year)

heatmap_data <- trends |> 
  filter(year == 2006) |> 
  filter(!is.na(total_jail_pop) & !is.na(capacity)) |> 
  group_by(state, year) |> 
  summarize(state_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            state_jail_capacity = sum(capacity, na.rm = TRUE)) |>
  mutate(usage_rate = state_jail_pop / state_jail_capacity)



#tooltip
heatmap_data <- heatmap_data |> 
  mutate(
tooltip_text = paste0(
  "Штат: ", state, "\n",
  "Заповненість: ", percent(usage_rate, accuracy = 0.1), "\n",
  "Ув'язнених: ", comma(state_jail_pop), "\n",
  "Місць: ", comma(state_jail_capacity)
))

#downloading the shapefiles
us_states <- st_read("cb_2018_us_state_500k.shp")

#join the data with the shapefiles 


heatmap <- us_states |> 
  left_join(heatmap_data, by = c("STUSPS" = "state")) 

#let's try to do only one-year observation

heatmap_plot <- heatmap |> 
  ggplot(aes(fill = usage_rate, text = tooltip_text)) +
  geom_sf(color = "white") +
  coord_sf(xlim = c(-125, -70), ylim = c(27, 47), expand = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "darkred", na.value = "grey90") +
  labs(title = "Заповненість в'язниць за штатами 2006",
       fill = "Заповненість") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 20))

ggplotly(heatmap_plot, tooltip = "text")
#okay... it can be better, well I still have a few days to nail it)

