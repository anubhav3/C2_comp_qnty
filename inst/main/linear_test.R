#13.12.2020
library(dplyr)
library(tidyverse)

m <- 4
c <- 5
npoints <- 1001
x <- seq(0, 100, length.out = npoints)
epsilon <- rnorm(npoints, mean = 0, sd = 30)
y <- m * x + c + epsilon

dd_raw <- data.frame(x = x, y = y)
ggplot(dd_raw) +
  geom_point(aes(x = x, y = y))

dd_l <- dd_raw %>%
  filter(x <= 50)

dd_u <- dd_raw %>%
  filter(x >= 50)

lm_l <- lm(y ~ x, data = dd_l)
lm_u <- lm(y ~ x, data = dd_u)
