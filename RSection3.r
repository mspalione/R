library(tidyverse)
library(dslabs)
data(murders)

###Indexing

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

### Data Wrangling
library(dplyr)

# adding a column with mutate
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# creating a new table with fewer columns by selecting columns with select
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)

# using the pipe %>% 
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# creating a data frame with stringAsFactors = FALSE to prevent strings from being stored as Factors
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

### Plots
# a simple scatterplot of total murders versus population
population_in_millions <- murders$population /10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

# a histogram of murder rates
hist(murders$rate)
murders$state[which.max(murders$rate)]

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

###Section 3 assessment
library(dslabs)
data(heights)
options(digits = 3)
avg <- mean(heights$height)
ind <- heights$height > avg & heights$sex == "Female"
sum(ind)
fem <- heights$sex == "Female"
mean(fem)
minheight <- min(heights$height)
ind <- match(minheight, heights$height)
heights$sex[ind]

maxheight <- max(heights$height)
maxheight
minheight:maxheight
x <- 50:82
sum(!x %in% heights$height)

heights2 <- mutate(heights, ht_cm = height * 2.54)
heights2[18]
mean(heights2$ht_cm)
females <- filter(heights2, sex == "Female")
length(females)
females
mean(females$ht_cm)


library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region, data=olive)
