library(tidyverse)
library(dslabs)
data(murders)

murder_rate <- murders$total / murders$population * 100000
murders$state[order(murder_rate, decreasing=TRUE)]

x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)
min(x)
which.min(x)
max(x)
which.max(x)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

hours <- time / 60
hours

speed <- distance / hours
speed
