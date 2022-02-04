# Quick plot of SRDB data matched to gapminder GDP

library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(gapminder)
library(tidyr)
library(readxl)

matching <- read_csv("country_matching.csv", col_types = "ccccc")
areas <- read_csv("continent_areas.csv", col_types = "cd")

# ------- January 2021: SOC observations
read_excel("SOC database_ver7.xlsx") %>% 
  rename(Country = Primarystudies_Country, Year = `Publication Year`) %>% 
  left_join(select(matching, SOC_country, gapminder_country), by = c("Country" = "SOC_country")) %>% 
  left_join(distinct(gapminder, country, continent), by = c("gapminder_country" = "country")) %>% 
  mutate(continent = as.character(continent),
         continent = if_else(continent == "Americas",
                             if_else(gapminder_country %in% c("Canada", "Mexico", "United States"), "North America", "South America"),
                             continent),
         continent = if_else(Country == "Antarctica", "Antarctica", continent)) %>% 
  replace_na(list(continent = "Asia")) %>% 
  group_by(continent, Year) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  complete(Year, continent, fill = list(n = 0)) %>% 
  # Asia goes up to 2025 :/ but most others don't; trim
  filter(Year < 2019) ->
  soc_continent_year

# Table with percentages by continent
soc_continent_year %>% 
  group_by(continent) %>% 
  summarise(total_n = sum(n)) %>%
  mutate(Percent = round(total_n / sum(total_n) * 100, 0))

# Normalize by area
soc_continent_year %>% 
  group_by(continent) %>% 
  arrange(Year) %>%
  mutate(cumn = cumsum(n)) %>%
  left_join(areas, by = "continent") %>% 
  mutate(cumn_area = cumn / area * 1e6)->
  soc

# Table with percentages for area-normalized data
soc %>% 
  filter(continent != "Antarctica") %>% 
  group_by(continent) %>% 
  summarise(cumn_area = max(cumn_area, na.rm = TRUE)) %>% 
  mutate(Percent = round(cumn_area / sum(cumn_area, na.rm = TRUE) * 100, 0))

p <- ggplot(soc, aes(Year, cumn_area, fill = continent)) + 
  geom_area() + 
  ylab(expression(Cumulative~SOC~observations~per~million~km^2)) + 
  theme(text = element_text(size = 14)) +
  scale_fill_viridis_d("Continent") +
  xlim(c(1960, 2020))

print(p)
ggsave("soc_by_continent.pdf", width = 8, height = 6)


# ------- N2O
read_excel("N2O data set_20201023.xlsx", sheet = "Sheet1") %>% 
  select(Country, Year = `Pulished year`, Lat, Long) %>% 
  mutate(Country = trimws(Country), Gas = "N[2]*O") ->
  n2o

matching_n2o <- select(matching, n2o_country, gapminder_country) %>% distinct(n2o_country, .keep_all = TRUE)
n2o %>% 
  left_join(matching_n2o, by = c("Country" = "n2o_country")) %>% 
  select(Year, gapminder_country, Gas) ->
  n2o_matched

# ------- CH4
read_excel("CH4 database_20201003.xlsx", sheet = "Forest_Gatica_2020", skip = 1) %>% 
  mutate(ecosystem = "Forest") %>% 
  select(Country, Year, Lat, Long, ecosystem) ->
  ch4_forest
read_excel("CH4 database_20201003.xlsx", sheet = "Wetlnd_Tan_2020", skip = 2) %>% 
  mutate(ecosystem = "Wetland", Year = `Observation year`, Lat = Latitude, Long = Longitude) %>% 
  tidyr::fill(Country, Year, References) %>% 
  filter(!is.na(CH4)) %>% 
  select(Country, Year, Lat, Long, ecosystem) ->
  ch4_wetland
read_excel("CH4 database_20201003.xlsx", sheet = "Landuse_Han_2020", skip = 1) %>% 
  mutate(ecosystem = "Landuse", Year = Published_year, Lat = Latitude, Long = Longitude) %>% 
  select(Country, Year, Lat, Long, ecosystem) ->
  ch4_landuse
read_excel("CH4 database_20201003.xlsx", sheet = "Coast_Al-Haj_2020") %>% 
  mutate(ecosystem = "Coast") %>% 
  select(Country, Year, Lat, Long, ecosystem) ->
  ch4_coast

rbind(ch4_forest, ch4_wetland, ch4_landuse, ch4_coast) %>% 
  mutate(Country = trimws(Country), Gas = "CH[4]", Year = as.integer(Year)) ->
  ch4

ch4 %>% 
  left_join(matching, by = c("Country" = "ch4_country")) %>% 
  select(Year, gapminder_country, Gas) ->
  ch4_matched

# ------- SRDB (CO2)
read.csv("srdb-data.csv", stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  filter(!is.na(Rs_annual)) %>% 
  mutate(Study_midyear = as.integer(Study_midyear), Gas = "CO[2]") %>%  
  select(Year = Study_midyear, Country, Gas) ->
  srdb

srdb %>% 
  left_join(matching, by = c("Country" = "srdb_country")) %>% 
  select(Year, gapminder_country, Gas) ->
  srdb_matched

srdb_matched %>% 
  left_join(distinct(gapminder, country, continent), by = c("gapminder_country" = "country")) %>% 
  mutate(continent = as.character(continent),
         continent = if_else(continent == "Americas",
                             if_else(gapminder_country %in% c("Canada", "Mexico", "United States"), "North America", "South America"),
                             continent)) %>% 
  replace_na(list(continent = "Asia")) %>% 
  group_by(continent, Year) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  complete(Year, continent, fill = list(n = 0)) %>% 
  group_by(continent) %>% 
  arrange(Year) %>%
  mutate(cumn = cumsum(n)) %>% 
  left_join(areas, by = "continent") %>% 
  mutate(cumn_area = cumn / area * 1e6) ->
  srdb_by_continent

p <- ggplot(srdb_by_continent, aes(Year, cumn, fill=continent)) + 
  geom_area() + 
  ylab("Cumulative number of observations") + 
  theme(text = element_text(size = 14)) +
  scale_fill_viridis_d("Continent")

print(p)
ggsave("Figure 3-srdb_by_continent.pdf", width = 8, height = 6)
print(p + xlim(c(1960, 2015)))
ggsave("Figure 3-srdb_by_continent_trimmed.pdf", width = 8, height = 6)

p <- ggplot(srdb_by_continent, aes(Year, cumn_area, fill=continent)) + 
  geom_area() + 
  ylab(expression(Cumulative~observations~per~million~km^2)) + 
  theme(text = element_text(size = 14)) +
  scale_fill_viridis_d("Continent")

print(p)
ggsave("srdb_by_continent_area.pdf", width = 8, height = 6)

# Create a 'complete' gapminder with all years 
myapprox <- function(x, y) {
  if(sum(is.finite(y)) > 1) {
    approx(x, y, rule = 2, xout = x)$y
  }
  else NA_real_
}

gapminder_unfiltered %>% 
  complete(nesting(country, continent), year = 1952:2020) %>% 
  arrange(country, continent, year) %>% 
  group_by(country, continent) %>% 
  mutate(gdpPercap = myapprox(year, gdpPercap)) ->
  gapminder_unfiltered_complete

areadata <- read_csv("countries_area.csv", col_types = "cd")

rbind(ch4_matched, n2o_matched, srdb_matched) %>% 
  mutate(year_gapminder = if_else(Year > 2006, 2006, Year)) %>% 
  left_join(gapminder_unfiltered_complete, 
            by = c("gapminder_country" = "country", "year_gapminder" = "year")) %>% 
  mutate(Study_5yr = floor(Year / 5) * 5 + 2.5) %>% 
  filter(!is.na(gdpPercap)) %>% 
  left_join(areadata, by = "gapminder_country") %>% 
  group_by(Study_5yr) %>% 
  mutate(GDP = as.character(cut(gdpPercap, 3, labels = c("Low", "Medium", "High"))),
         GDP = if_else(gapminder_country == "China", "(China)", GDP)) %>%
  group_by(Study_5yr, GDP, Gas) %>% 
  summarise(n = n(),
            n_area = n() / sum(land_area) * 1e6) ->
  y

p <- ggplot(y, aes(Study_5yr, n, fill = GDP)) +
  geom_col() +
  facet_grid(Gas~., scales = "free_y") +
  xlab("Year of observation") + ylab("Number of observations") +
  scale_fill_viridis_d()

print(p)
ggsave("gdp.pdf")

# cumulative plot
y %>% 
  group_by(Gas) %>% 
  complete(Study_5yr, GDP, fill = list(n = 0, n_area = 0)) %>% 
  group_by(Gas, GDP) %>% 
  arrange(Study_5yr) %>% 
  mutate(cumn = cumsum(n),
         cumn_area = cumsum(n_area),
         Gas = factor(Gas, levels = c("CO[2]", "N[2]*O", "CH[4]")),
         GDP = factor(GDP, levels = c("(China)", "High", "Medium", "Low"))) ->
  cum_pubs

p <- ggplot(cum_pubs, aes(Study_5yr, cumn, fill = GDP)) + 
  geom_area() +
  facet_grid(Gas~., scales = "free_y", labeller = label_parsed) +
  xlab("Year") + 
  ylab("Observations") +
  xlim(c(1970, 2018)) +
  theme(text = element_text(size = 14)) +
  scale_fill_viridis_d()

print(p)
ggsave("gdp_cumulative.pdf")

p <- ggplot(cum_pubs, aes(Study_5yr, cumn_area, fill = GDP)) + 
  geom_area() +
  facet_grid(Gas~., scales = "free_y", labeller = label_parsed) +
  xlab("Year") + 
  ylab(expression(Cumulative~observations~per~million~km^2)) + 
  xlim(c(1970, 2018)) +
  theme(text = element_text(size = 14)) +
  scale_fill_viridis_d()

print(p)
ggsave("gdp_cumulative_area.pdf")

  
write_csv(y, "srdb_gdp.csv")
