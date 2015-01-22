### Homework 3
library(dplyr)

# Qn 1
library(datasets)
data(mtcars)
t.test(mtcars$mpg)

# Qn 2
qt(0.975, 8)
qt(0.975, 8) / 3

# Qn 3
c4 <- mtcars %>%
  filter(cyl == 4) %>%
  select(mpg)

c6 <- mtcars %>%
  filter(cyl == 6) %>%
  select(mpg)

t.test(c4, c6, var.equal = T)
