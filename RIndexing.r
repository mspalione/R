library(tidyverse)
library(dslabs)
data(murders)

murder_rate <- murders$total / murders$population * 100000

index <- murder_rate < 0.71
index <- murder_rate <= 0.71
index

murders$state[index]

west <- murders$region == "West"
safe <- murder_rate <= 1

index <- safe & west
murders$state[index]

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state