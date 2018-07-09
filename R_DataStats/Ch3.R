library(tidyverse)

ggplot(data=mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy))

ggplot(data=mpg) +
  geom_point(mapping = aes(x=cyl, y=hwy))


ggplot(data=mpg) +
  geom_point(mapping = aes(x=drv, y=class), aes(color=displ<5))

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)

#facet on a continuous variable - it yells at you
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow=2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

#overlapping data points & fitted line
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(mapping = aes(group=drv), se = FALSE)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
