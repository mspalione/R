### Section 3 Random Variables, Sampling Models, and the Central Limit Theorem

##3.1 Random Variables and Sampling Models
#Random variables are numeric outcomes resulting from random processes.
#Statistical inference offers a framework for quantifying uncertainty due to randomness.

#Code: Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

##Sampling Models
#A sampling model models the random behavior of a process as the sampling of draws from an urn.
#The probability distribution of a random variable is the probability of the 
  #observed value falling in any given interval.
#We can define a CDF  F(a)=Pr(S???a)  to answer questions related to the probability of S being in any interval.
#The average of many draws of a random variable is called its expected value.
#The standard deviation of many draws of a random variable is called its standard error.

#Monte Carlo simulation: Chance of casino losing money on roulette
#We build a sampling model for the random variable  S  that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S
#We use the sampling model to run a Monte Carlo simulation and use the results to 
  #estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
    X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
    sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

#We can plot a histogram of the observed values of S as well as the normal 
  #density curve based on the mean and standard deviation of S.

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
    ggplot(aes(S, ..density..)) +
    geom_histogram(color = "black", binwidth = 10) +
    ylab("Probability") +
    geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

#binomial distribution -> (S + n) / 2 can be used instead of the monte carlo simulation 
  #to know the probability distribution of S
    # The probability distribution of a random variable tells us the probability 
      #of the observed value falling at any given interval. 

##Distributions versus Probability Distributions
#A random variable  X  has a probability distribution function  F(a)  that 
  #defines  Pr(X???a)  over all values of  a .
#Any list of numbers has a distribution. The probability distribution function 
  #of a random variable is defined mathematically and does not depend on a list of numbers.
#The results of a Monte Carlo simulation with a large enough number of observations 
  #will approximate the probability distribution of  X .
#If a random variable is defined as draws from an urn:
  #The probability distribution function of the random variable is defined as 
    #the distribution of the list of values in the urn.
  #The expected value of the random variable is the average of values in the urn.((a*p) + b * (1-p))
  #The standard error of one draw of the random variable is the standard deviation of the values of the urn.

##Notation for Random Variables
#Capital letters denote random variables ( X ) and lowercase letters denote observed values ( x ).
#In the notation  Pr(X=x) , we are asking how frequently the random variable  X  
  #is equal to the value  x . For example, if  x=6 , this statement becomes  Pr(X=6) .

##Central Limit Theorem
#The Central Limit Theorem (CLT) says that the distribution of the sum of a 
  #random variable is approximated by a normal distribution.
#The expected value of a random variable,  E[X]=?? , is the average of the values 
  #in the urn. This represents the expectation of one draw. 
#The standard error of one draw of a random variable is the standard deviation of the values in the urn.
#The expected value of the sum of draws is the number of draws times the expected value of the random variable. 
#The standard error of the sum of independent draws of a random variable is the 
  #square root of the number of draws times the standard deviation of the urn. 

#Equations
#These equations apply to the case where there are only two outcomes,  
  #a  and  b  with proportions  p  and  1???p  respectively. 
  #The general principles above also apply to random variables with more than two outcomes.

#Expected value of a random variable: 
#  a * p + b * (1???p)
#Expected value of the sum of n draws of a random variable: 
#  n * (a*p+b*(1???p)) 
#Standard deviation of an urn with two values (a and b) and two proportions (p and 1-p): 
  #abs(b - a) * sqrt((p : probability of a) * (1-p : 1 - probability of b))
  #roulette example: a = 1, b = -1, p(of a) = 10/19, 1-p(of b) = 9/19
  #abs(1 - (-1)) * sqrt((10/19) * (9/19))
  2 * sqrt(90)/19
#Standard error of the sum of n draws of a random variable:
  n <- 1000 #1000 playing
  2 * sqrt(90)/19 / sqrt(n) #use standard deviation formula (above) to determine standard error

#Standard deviation of a list is the square root of the average of the squared differences
  library(dslabs)
  x <- heights$height
  m <- mean(x)
  s <- sqrt(mean((x-m)^2))
  #using the sd function on a list returns a slightly different result. 
  #To compute the actual standard deviation (sd), use: sqrt(mean((x-m)^2))
  #If the list size is large, the R sd function and the above formula are practically equivalent
  
#Using CLT to compute the probability of the casino losing money:
  mu <- n * (20-18) / 38
  se <- sqrt(n) * 2 * sqrt(90)/19 
  probability_of_losing_money <- pnorm(0, mu, se)
  
#Subtract the probability of losing money (above) from 1 to determine the probability of winning money
  1 - probability_of_losing_money  

###3.2 The Central Limit Theorem Continued
##Averages and Proportions
  #?? <- expected value  
    #?? is the Greek letter for m, the first letter of mean, which is another term used for expected value
  #?? <- standard error
    #?? is the Greek letter for s, the first letter of standard error.
  
#Random variable times a constant
  #The expected value of a random variable multiplied by a constant is that 
  #constant times its original expected value:
  #E[aX]=a?? 
#The standard error of a random variable multiplied by a constant is that 
  #constant times its original standard error:
  #SE[aX]=a?? 
#Average of multiple draws of a random variable
  #The expected value of the average of multiple draws from an urn is the expected value of the urn ( ?? ).
  #The standard deviation of the average of multiple draws from an urn is the 
  #standard deviation of the urn divided by the square root of the number of draws ( ??/sqrt(n) ).

#The sum of multiple draws of a random variable
  #The expected value of the sum of  n  draws of a random variable is  n  times 
  #its original expected value:
  #E[nX]=n?? 
  #The standard error of the sum of  n  draws of random variable is  sqrt(n)  times 
  #its original standard error:
  #SE[nX]=sqrt(n??) 
#The sum of multiple different random variables
  #The expected value of the sum of different random variables is the sum of the 
  #individual expected values for each random variable:
  #E[X1+X2+???+Xn]=??1+??2+???+??n 
  #The standard error of the sum of different random variables is the square root 
  #of the sum of squares of the individual standard errors:
  #SE[X1+X2+???+Xn]=sqrt(??21+??22+???+??2n) 
#Transformation of random variables
  #If  X  is a normally distributed random variable and  a  and  b  are non-random constants, 
  #then  aX+b  is also a normally distributed random variable.
  
###Assessment
pcorrect <-  1/4
e_points <- (1*pcorrect) + (0 * (1-(3/4)))  
n_q <- 44
n_q * e_points
se <- sqrt(44) * abs(-.25 - 1) * sqrt(pcorrect * (4/5))
avg <- 44 * (1/5)
1 - pnorm(8, avg, se)
set.seed(21)
mc <- replicate(10000, {
  R <- sample(c(1, .25), avg, replace = TRUE, prob = c(1/5, 1 - 4/5))
  sum(R)
})
mean(mc > 8)

p <- seq(0.25, 0.95, 0.05)

p <- 5/38
a <- 6
b <- -1
n <- 500
expected_value <- (a*p) + b * (1-p)
standard_deviation <- abs(b-a) * sqrt(p * (1-p))
standard_error <- standard_deviation / sqrt(n)
standard_deviation * sqrt(n)
standard_deviation / sqrt(n)
