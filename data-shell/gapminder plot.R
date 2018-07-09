setwd("C:/Users/adaha/Desktop/data-shell")
gapminder <- read.csv(file="gapminder-FiveYearData.csv")

gapminder[1:20]
gapminder[1:20,]

gapminder_small <- data.frame(gapminder[c(1:9, 19:23),])
head(gapminder_small)

library(ggplot2)
ggplot(data = gapminder, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm")

#Extracts the first letter of every country
starts.with <- substr(gapminder$country, start=1, stop=1)

head(starts.with)
length(starts.with)

#Countries starting with A or Z
az.countries <- gapminder[starts.with %in% c("A", "Z"),]
View(az.countries)

ggplot(data = az.countries, aes(x=year, y=lifeExp, color=continent)) +
  geom_line() + facet_wrap(~country)
