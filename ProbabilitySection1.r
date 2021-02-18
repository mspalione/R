###Introduction to Discrete Probability
### https://rpubs.com/hgjerning/475916

beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

#using replace = TRUE instead of replicate. Replaces the beads back into the jar after drawing one out
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

#applying the mean() function to a logical vector returns the proportion of elements that are TRUE.
mean(beads == "blue")

#independence:
#the outcome of the first event does not impact the outcome of the second (like flipping a coin)
#the outcome of the first event DOES impact the outcome of the second (like drawing cards from a deck)

#probability of two events occuring is known as the multiplication rule
#The multiplication rule for independent events is: Pr(A and B and C)=Pr(A)×Pr(B)×Pr(C)
#The multiplication rule for dependent events considers the conditional probability of both events occurring: 
  #Pr(A and B)=Pr(A)×Pr(B???A)
#We can expand the multiplication rule for dependent events to more than 2 events: 
  #Pr(A and B and C)=Pr(A)×Pr(B???A)×Pr(C???A and B)
  
###Assessment: Introduction to Discrete Probability
balls <- rep(c("cyan", "magenta", "yellow"), times = c(3, 5, 7))
mean(balls != "cyan")
sample(balls, 1)    # sample 1 ball at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(balls, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions
0.3368 + 0.4634 
cyan <- 3
magenta <- 5
yellow <- 7

# The variable `p1` is the probability of choosing a cyan ball from the box on the first draw.
p1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p2` as the probability of not choosing a cyan ball on the second draw without replacement.
p2 <- 1 - (cyan - 1) / (cyan + magenta + yellow - 1)

# Calculate the probability that the first draw is cyan and the second draw is not cyan.
firstDrawCyanSecondNot <- p1 * p2

cyan <- cyan - 1

firstDrawCyan <- 0.1998
secondDrawNotCyan <- 0.8

firstDrawCyan * secondDrawNotCyan


###1.2 Combinations and Permutations
#Introducing paste() and expand.grid()

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

#Permutations and combinations
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

#Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

#Monte Carlo simulation of natural 21 in blackjack
# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

#The birthday problem
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


#Some functions automatically apply element-wise to vectors, such as sqrt() and *.
#However, other functions do not operate element-wise by default. This includes functions we define ourselves.
#The function sapply(x, f) allows any other function f to be applied element-wise to the vector x.

#The probability of an event happening is 1 minus the probability of that event not happening: Pr(event)=1???Pr(no event)
#We can compute the probability of shared birthdays mathematically: Pr(shared birthdays)=1???Pr(no shared birthdays)=1???(1×364365×363365×...×365???n+1365)

#Function for birthday problem Monte Carlo simulations
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

#Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

#Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

#The larger the number of Monte Carlo replicates  B , the more accurate the estimate.
#Determining the appropriate size for  B  can require advanced statistics.
#One practical approach is to try many sizes for  B  and look for sizes that provide stable estimates.

#Estimating a practical value of B
#This code runs Monte Carlo simulations to estimate the probability of shared birthdays using several B values and plots the results. 
#When B is large enough that the estimated probability stays stable, then we have selected a useful value of B.
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

###1.3 Addition Rule and Monty Hall
## Addition Rule
#The addition rule states that the probability of event A or event B happening 
#is the probability of event A plus the probability of event B 
#minus the probability of both events A and B happening together.
#Pr(A or B)=Pr(A)+Pr(B)???Pr(A and B)
#Note that (A or B) is equivalent to  (A|B)


#Example: The addition rule for a natural 21 in blackjack
#We apply the addition rule where  A  = drawing an ace then a facecard and  B  = drawing a facecard then an ace. 
#Note that in this case, both events A and B cannot happen at the same time, so  Pr(A and B)=0 .

#Pr(ace then facecard)=
  (4/52) * (16/51) 
#Pr(facecard then ace)=
  (16/52) * (4/51) 
#Pr(ace then facecard | facecard then ace)= 0.0483 
  (4/52) * (16/51) + (16/52) * (4/51) 

## Monty Hall
#Monte Carlo simulations can be used to simulate random outcomes, 
#which makes them useful when exploring ambiguous or less intuitive problems like the Monty Hall problem.
#In the Monty Hall problem, contestants choose one of three doors that may contain a prize. 
#Then, one of the doors that was not chosen by the contestant and does not contain a prize is revealed. 
#The contestant can then choose whether to stick with the original choice or switch to the remaining unopened door.
#Although it may seem intuitively like the contestant has a 1 in 2 chance of winning regardless of whether they stick or switch, 
#Monte Carlo simulations demonstrate that the actual probability of winning is 1 in 3 with the stick strategy and 2 in 3 with the switch strategy.

#Code: Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

#Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching

#Assessments
n <- 6
l <- list(0:1)
possibilities <- expand.grid(rep(l, n))
rowSums(possibilities, 1 >= 4)
rowSums(possibilities) >= 4

library(gtools)
library(tidyverse)

x <- permutations(3,3)
x
#The multiplication rule for independent events is: Pr(A and B and C)=Pr(A)×Pr(B)×Pr(C)
#The multiplication rule for dependent events considers the conditional probability of both events occurring: 
#Pr(A and B)=Pr(A)×Pr(B???A)
#We can expand the multiplication rule for dependent events to more than 2 events: 
#Pr(A and B and C)=Pr(A)×Pr(B???A)×Pr(C???A and B)

# The variable `p1` is the probability of choosing a cyan ball from the box on the first draw.
p1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p2` as the probability of not choosing a cyan ball on the second draw without replacement.
p2 <- 1 - (cyan - 1) / (cyan + magenta + yellow - 1)

# Calculate the probability that the first draw is cyan and the second draw is not cyan.
firstDrawCyanSecondNot <- p1 * p2


oneJamaicanWin <- 3 / 8
twoJamaicanWin <- 2/7
threeJamaicanWin <- 1/6
twoJamaicanMedals <- oneJamaicanWin * twoJamaicanWin
threeJamaicanMedals <- twoJamaicanMedals * threeJamaicanWin

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000
allJ <- replicate(B, {
  s <- sample(runners, 3)
  x <- s == c("Jamaica", "Jamaica", "Jamaica")
  all(x)
  })
allJ
mean(allJ)

#line 78
entree <- 1:6
sides <- 1:6
drinks <- 1:2

expand.grid(entree, sides, drinks)
combinations(6,2)
6*15*2 = 180
6*15*3
c <- combinations(6,3)
nrow(c)
6*20*3

mealCombos <- function(n){
  side <- nrow(combinations(n,2))
  6 * side * 3
}
sapply(2:12, mealCombos)

head(esoph)
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
total <- all_controls + all_cases
max(esoph$alcgp)

mean(esoph$alcgp == "120+" & esoph$ncases != 0)

esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(all = sum(ncontrols))
13/all_controls
esoph %>% filter(tobgp != "0-9g/day") %>% sum(esoph$ncases)/all_cases
  summarize(smoke=sum(tobgp != "0-9g/day"), tot=sum(ncases), probability=smoke/tot)
  
esoph %>% summarize(tot_cases = sum(ncases))
  esoph %>% filter(tobgp != "0-9g/day") %>%
    summarize(smoking10_cases = sum(ncontrols))
  450/all_controls
  
esoph %>% filter(tobgp == "30+") %>% summarize(all = sum(ncontrols))
controls <- 136/all_controls
cases <-  66/all_cases
cases/controls
