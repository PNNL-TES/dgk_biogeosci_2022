# bbl 2021-08-24

library(readr)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(tidyr)

read_csv("Continent_Year_Pub_ForExcel.csv") %>% 
  pivot_longer(cols = -Continent, names_to = "Year") %>% 
  mutate(Year = as.numeric(Year),
         Continent = if_else(Continent == "No_Data", "(No data)", Continent)) %>% 
  group_by(Continent) %>% 
  mutate(Publications = cumsum(value)) ->
  x

x <- filter(x, Continent != "(No data)")

ggplot(x, aes(Year, Publications, fill = Continent, group = Continent)) + 
  geom_area() +
  theme(legend.position = c(0.25, 0.6),
        text = element_text(size = 14)) +
  scale_fill_viridis_d(direction = -1)

ggsave("dai_cumulative.pdf")
