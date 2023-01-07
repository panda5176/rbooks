library(tidyverse)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap( ~ class, nrow = 2)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point(position = "jitter") +
  geom_smooth(mapping = aes(linetype = drv), se = FALSE)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()
nz <- map_data("nz")
ggplot(data = nz, mapping = aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()
ggplot(data = diamonds, mapping = aes(x = cut, fill = cut)) +
  geom_bar() +
  coord_polar()
