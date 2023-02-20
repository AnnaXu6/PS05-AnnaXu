library(readr)
## ---------- load and check ----------
## 1. For solving the problems, and answering the questions, 
## create a new rmarkdown document with an appropriate title
gapminder <- read_delim("gapminder.csv.bz2")

## 2. How many rows/columns do we have?
dim <- dim(gapminder)
cat("There are", dim, "rows and columns.")

## 3. Print a small sample of data.
library(dplyr)
sample_n(gapminder, 5)

## ------ Descriptive statistics ------ 
## 1. How many countries are there in the dataset?
countries_iso3 <- gapminder %>% distinct(iso3) %>% nrow()
countries_iso3
countries_iso2 <- gapminder %>% distinct(iso2) %>% nrow()
countries_iso2
countries_name <- gapminder %>% distinct(name) %>% nrow()
countries_name

## 2. If you did this correctly, you saw that there are more names than iso-2 codes, 
## and there are even more iso3 -codes. What is going on? Can you find it out?

## a. Find how many names are there for each iso-2 code. 
## Are there any iso-2 codes that corre- spond to more than one name? 
## What are these countries?
iso2_names <- gapminder %>%
  group_by(iso2) %>%
  summarize(num_names = n_distinct(name))

iso2_names %>%
  filter(num_names > 1)

cat("There are two country names that do not have iso2 codes.")

## b. Now repeat the same for name and iso3-code. 
## Are there country names that have more than one iso3-code? 
## What are these countries? Hint: two of these entitites are CHANISL and NLD CURACAO.
name_iso3 <- gapminder %>%
  group_by(name) %>%
  summarize(num_iso3 = n_distinct(iso3))

name_iso3 %>%
  filter(num_iso3 > 1)

cat("There are four country names that do not have iso3 codes.")

gapminder[is.na(gapminder$name),] %>% 
  group_by(iso3) %>% 
  summarize(
    n = n()
  )

cat("These entities are CHANISL, GBM, KOS, and NLD CURACAO")

## 3. What is the minimum and maximum year in these data?
min_year <- min(gapminder$time, na.rm = TRUE)
max_year <- max(gapminder$time, na.rm = TRUE)

cat("Minimum year: ", min_year, "\n")
cat("Maximum year: ", max_year, "\n")


## ---------- CO2 emissions ----------
## 1. How many missing co2 emissions are there for each year? 
## Analyze both missing CO2 and co2_PC. Which years have most missing data?
library(dplyr)

missing_co2 <- gapminder %>%
  group_by(time) %>%
  summarize(missing_co2 = sum(is.na(co2)), missing_co2_pc = sum(is.na(co2_PC))) %>%
  arrange(desc(missing_co2))
missing_co2

cat("Most missing year is 2017, 2018 and 2019")

## 2. Make a plot of total CO2 emissions over time for the U.S, China, and India. 
## Add a few more countries of your choice. Explain what do you see.
library(ggplot2)

gapminder %>%
  filter(name %in% c("United States of America", "China", "India", "France", "Germany")) %>%
  group_by(name, co2, time) %>%
  ggplot(aes(x = time, y = co2, color = name)) +
  geom_line() +
  labs(color = "Country",
       x = "Year",
       y = "CO2 Emissions (metric tons)")

## 3. Now let’s analyze the CO2 emissions per capita (co2_PC). 
## Make a similar plot of the same countries. What does this figure suggest?
gapminder %>%
  filter(name %in% c("United States of America", "China", "India", "France", "Germany")) %>%
  group_by(name, co2_PC, time) %>%
  ggplot(aes(x = time, y = co2_PC, color = name)) +
  geom_line() +
  labs(color = "Country",
       x = "Year",
       y = "CO2_PC Emissions (metric tons)")

## 4. Compute average CO2 emissions per capita across the continents (assume region is the
## same as continent). Comment what do you see.
## Note: just compute averages over countries and ignore the fact that countries are of different size.
## Hint: Americas 2016 should be 4.80.
gapminder %>%
  filter(time %in% c(1960, 2016), !is.na(co2_PC), !is.na(region)) %>%
  group_by(time, region) %>%
  summarize(avg_co2_PC = mean(co2_PC, na.rm = TRUE))

## 5. Make a barplot where you show the previous results–average CO2 emissions per capita
## across continents in 1960 and 2016.
gapminder %>%
  filter(time %in% c(1960, 2016), !is.na(co2_PC), !is.na(region)) %>%
  group_by(time, region) %>%
  summarize(avg_co2_PC = mean(co2_PC, na.rm = TRUE)) %>%
  ggplot(aes(x = region, y = avg_co2_PC, fill = as.factor(time))) +
  geom_col(position =  "dodge") +
  scale_fill_discrete(name = "Year") +
  labs(x = "Region",
       y = "Average CO2_PC Emissions (metric tons)")

## 6. Which countries are the three largest, and three smallest CO2 emitters (in terms of CO2 per
## capita) in 2016 for each continent? (Assume region is continent.)
gapminder %>%
  filter(time == 2016, !is.na(co2_PC), !is.na(region)) %>%
  group_by(region, name) %>%
  summarize(co2_data = co2_PC) %>%
  arrange(region, desc(co2_data)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <= 3 | ranking >= n() - 2) %>%
  ungroup() %>%
  arrange(region, ranking)


## ---------- GDP per capita ----------
## 1. Make a scatterplot of GDP per capita versus life expectancy by country, using data for
## 1960. Make the point size dependent on the country size, and color those according to the
## continent. Feel free to adjust the plot in other ways to make it better.
gapminder %>%
  filter(time == 1960, !is.na(region),
         !is.na(GDP_PC),
         !is.na(lifeExpectancy)) %>%
  ggplot(aes(GDP_PC, lifeExpectancy, size = totalPopulation, col = region)) +
  geom_point() +
  labs(title = "GDP per capita versus life expectancy by country(1960)", 
       x = "GDP per capita",
       y = "Life Expectancy")

## 2. Make a similar plot, but this time use 2016 data only.
gapminder %>%
  filter(time == 2019 & !is.na(region),
         !is.na(GDP_PC),
         !is.na(lifeExpectancy)) %>%
  ggplot(aes(GDP_PC, lifeExpectancy, size = totalPopulation, col = region)) +
  geom_point() +
  labs(title = "GDP per capita versus life expectancy by country(2019)", 
       x = "GDP per capita",
       y = "Life Expectancy")

## 3. Compare these two plots and comment what do you see. How has world developed
## through the last 60 years?
## The data shows that overall life expectancy and GDP per capita have increased significantly over the past 60 years. 

## 4. Compute the average life expectancy for each continent in 1960 and 2019. Do the results
## fit with what do you see on the figures?
gapminder %>%
  filter(time %in% c(1960, 2019), !is.na(region)) %>%
  group_by(region, time) %>%
  summarize(avg_life_ex = mean(lifeExpectancy, na.rm = TRUE)) 

## 5. Compute the average LE growth from 1960-2019 across the continents. Show the results
## in the order of growth. Explain what do you see.
gapminder %>%
  filter(time %in% c(1960, 2019), !is.na(region)) %>%
  group_by(region) %>%
  summarize(avg_life_ex = mean(lifeExpectancy, na.rm = TRUE)) %>%
  mutate(diff = avg_life_ex - lag(avg_life_ex)) %>%
  filter(!is.na(diff)) %>%
  group_by(region, diff) %>%
  arrange(desc(diff)) %>%
  print()

## 6. Show the histogram of GDP per capita for years of 1960 and 2019. Try to put both
## histograms on the same graph, see how well you can do it!
gapminder %>%
  filter(time %in% c(1960, 2019), !is.na(GDP_PC)) %>%
  ggplot(aes(GDP_PC, fill = factor(time))) +
  geom_histogram(position = "dodge", bins = 5) +
  labs(title = "GDP per capita for years of 1960 and 2019", 
       x = "GDP per capita",
       y = "Count",
       fill = "Year")

## 7. What was the ranking of US in terms of life expectancy in 1960 and in 2019? (When
## counting from top.)
gapminder %>%
  filter(time %in% c(1960, 2019), !is.na(name)) %>%
  group_by(time) %>%
  mutate(rank = rank(desc(lifeExpectancy))) %>%
  filter(iso3 == "USA") %>%
  select(name, time, lifeExpectancy, rank)

## 8.  If you did this correctly, then you noticed that US ranking has been falling quite a
## bit. But we also have more countries in 2019–what about the relative rank divided by the
## corresponding number of countries that have LE data in the corresponding year?
gapminder %>%
  filter(time %in% c(1960, 2019), !is.na(name) & !is.na(lifeExpectancy)) %>%
  group_by(time) %>%
  mutate(ranking = rank(desc(lifeExpectancy))) %>%
  mutate(p = ranking / n()) %>%
  filter(iso3 == "USA") %>%
  select(name, time, ranking, p)
