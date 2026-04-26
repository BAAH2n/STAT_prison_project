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

plot_ly()


library(tidyverse)
library(plotly)
library(scales)

# 1. Підготовка даних
heatmap_data_plotly <- trends |> 
  filter(year >= 1990) |> 
  filter(!is.na(total_jail_pop) & !is.na(capacity)) |> 
  group_by(state, year) |> 
  summarize(
    state_jail_pop = sum(total_jail_pop, na.rm = TRUE),
    state_jail_capacity = sum(capacity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    usage_rate = state_jail_pop / state_jail_capacity,
    tooltip_text = paste0(
      "Штат: ", state, "<br>",
      "Рік: ", year, "<br>",
      "Заповненість: ", percent(usage_rate, accuracy = 0.1), "<br>",
      "Ув'язнених: ", comma(state_jail_pop), "<br>",
      "Місць: ", comma(state_jail_capacity)
    )
  ) |> 
  complete(state, year)

my_custom_colorscale <- list(
  c(0, "#FFFFFF"),       # 0% (Низька заповненість) - Білий
  c(0.5, "#3874AF"),     # 50% (Середня заповненість) - Ваш синій
  c(1, "#6B2AA4")        # 100% (Переповнено) - Ваш фіолетовий
)

# 2. Створення карти з вашою темою (theme_proj)
h1_plot <- plot_ly(
  data = heatmap_data_plotly,
  type = 'choropleth',
  locations = ~state,
  locationmode = 'USA-states',
  z = ~usage_rate,
  frame = ~year,
  colorscale = my_custom_colorscale, # Палітра самої карти (залиште або змініть за потреби)
  zmin = 0.5,
  zmax = 1.5,
  text = ~tooltip_text,
  hoverinfo = 'text',
  colorbar = list(
    title = "",                  # legend.title = element_blank()
    orientation = "h",           # legend.position = "bottom"
    xanchor = "center", x = 0.5, 
    yanchor = "top", y = -0.05,  
    len = 0.6,
    tickformat = ".0%",
    tickfont = list(color = "#3874AF", size = 12), # axis.text: color = "#3874AF", size = 12
    bgcolor = "#CADCED"          # legend.background: fill = "#CADCED"
  )
) |>
  layout(
    # Фони: plot.background та panel.background
    paper_bgcolor = '#CADCED', 
    plot_bgcolor  = '#CADCED',
    
    # Базовий текст: text = element_text(color = "#FFFFFF")
    font = list(color = "#FFFFFF"), 
    
    title = list(
      # Форматування заголовка (bold) та підзаголовка (italic) через HTML-теги
      text = '<b>Заповненість місцевих в\'язниць за штатами</b><br><span style="font-size:13px; font-style:italic; color:#FFFFFF;">Динаміка перевищення лімітів (1990-2018)</span>',
      # plot.title: size = 20, color = "#6B2AA4"
      font = list(size = 20, color = "#6B2AA4"),
      y = 0.95
    ),
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      bgcolor = '#CADCED',      # panel.background для географічної площини
      lakecolor = '#CADCED',    # Робимо озера кольором фону
      showlakes = TRUE
    ),
    margin = list(t = 80, b = 80) # Відступи зверху та знизу для заголовка/легенди
  ) |> 
  animation_opts(
    frame = 800, 
    transition = 0, 
    redraw = TRUE 
  ) |>
  animation_slider(
    currentvalue = list(
      prefix = "Рік: ", 
      font = list(color = "#6B2AA4", size = 14) # Стиль тексту повзунка
    ),
    bgcolor = "#CADCED"
  )

h1_plot
