ggplot(data=gapminder, aes(x=year, y=lifeExp, color=country)) +
  geom_point(show.legend = FALSE) + facet_wrap(~continent) +
  geom_line(show.legend = FALSE)

ggplot(data=gapminder, aes(x=year, y=lifeExp, color=continent)) +
  geom_smooth()
