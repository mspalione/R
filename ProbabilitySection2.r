### Section 2 Continuous Probability

#The cumulative distribution function (CDF) is a distribution function for continuous 
  #data  x  that reports the proportion of the data below  a  for all values of  a :
  # F(a)=Pr(x???a) 
#The CDF is the probability distribution function for continuous variables. 
  #For example, to determine the probability that a male student is taller than 70.5 inches 
  #given a vector of male heights  x , we can use the CDF:
  #  Pr(x>70.5)=1???Pr(x???70.5)=1???F(70.5) 
#The probability that an observation is in between two values  a,b  is  F(b)???F(a) .
#Code: Cumulative distribution function
#Define x as male heights from the dslabs heights dataset:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

#Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

## Theoretical Distribution
#pnorm(a, avg, s) gives the value of the cumulative distribution function  F(a)  
  #for the normal distribution defined by average avg and standard deviation s.

#We say that a random quantity is normally distributed with average avg and 
  #standard deviation s if the approximation pnorm(a, avg, s) holds for all values of a.

#If we are willing to use the normal approximation for height, 
  #we can estimate the distribution simply from the mean and standard deviation of our values.
#If we treat the height data as discrete rather than categorical, 
  #we see that the data are not very useful because integer values are more common than 
  #expected due to rounding. This is called discretization.
#With rounded data, the normal approximation is particularly useful when computing 
  #probabilities of intervals of length 1 that include exactly one integer.

#Code: Using pnorm() to calculate probabilities
#Given male heights x:
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

#We can estimate the probability that a male is taller than 70.5 inches using:
1 - pnorm(70.5, mean(x), sd(x))

#Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

##Probability Density
#The probability of a single value is not defined for a continuous distribution.
#The quantity with the most similar interpretation to the probability of a single value 
  #is the probability density function  f(x) .
#The probability density  f(x)  is defined such that the integral of  f(x)  over a range 
  #gives the CDF of that range.
#F(a)=Pr(X???a)=???a??????f(x)dx 
#In R, the probability density function for the normal distribution is given by dnorm(). 
  #We will see uses of dnorm() in the future.
#Note that dnorm() gives the density function and pnorm() gives the distribution function, 
  #which is the integral of the density function.
avg <- mean(x)
s <- sd(x)
1 - pnorm(76, avg, s)

##Plotting the probability density for the normal distribution
#We can use dnorm() to plot the density curve for the normal distribution. 
  #dnorm(z) gives the probability density  f(z)  of a certain z-score, 
  #so we can draw a curve by calculating the density over a range of possible values of z.

#First, we generate a series of z-scores covering the typical range of the normal distribution. 
  #Since we know 99.7% of observations will be within  ???3???z???3 , 
  #we can use a value of  z  slightly larger than 3 and this will cover most likely values 
  #of the normal distribution. Then, we calculate  f(z) , which is dnorm() of the series of z-scores. 
  #Last, we plot  z  against  f(z) .

x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#Note that dnorm() gives densities for the standard normal distribution by default. Probabilities for 
  #alternative normal distributions with mean mu and standard deviation sigma can be evaluated with:
  #dnorm(z, mu, sigma)

##Monte Carlo Simulations
#rnorm(n, avg, s) generates n random numbers from the normal distribution with 
  #average avg and standard deviation s.
#rnorm(n = size, avg = average(default 0), s = standard deviation(default 1))
#By generating random numbers from the normal distribution, 
  #we can simulate height data with similar properties to our dataset. 
  #Here we generate simulated height data using the normal distribution.

#Code: Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

#Code: Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

##Other Continuous Distributions
#You may encounter other continuous distributions (Student t, chi-squared, exponential, gamma, beta, etc.).
#R provides functions for density (d), quantile (q), probability distribution (p) 
  #and random number generation (r) for many of these distributions.
#Each distribution has a matching abbreviation (for example, norm() or t()) 
  #that is paired with the related function abbreviations (d, p, q, r) to create appropriate functions.
#For example, use rt() to generate random numbers for a Monte Carlo simulation using the Student t distribution.

#Code: Plotting the normal distribution with dnorm
#Use d to plot the density function of a continuous distribution. 
#Here is the density function for the normal distribution (abbreviation norm()):
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

##Assessment
set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)
sd(act_scores)
sum(act_scores >= 36)
gt30 <- sum(act_scores <= 10)
gt30 / 10000

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

zScore <- (act_scores - mean(act_scores)) / sd(act_scores)
sum(zScore > 2) / length(zScore)
zScore[zScore == 2]
2*sd(act_scores)+mean(act_scores)
qnorm(0.975, mean(act_scores), sd(act_scores))

cdf <- function(v){
  v/36
}

blech <- sapply(1:36, cdf())
blech[35]
ceiling(qnorm(0.95, mean(act_scores), sd(act_scores)))
qnorm(0.95, 20.9, 5.7)

sample_quantiles <- seq(0.01, 0.99, 0.01)
quantile(act_scores, sample_quantiles)

theoretical_quantile <- qnorm(sample_quantiles, 20.9, 5.7)
qqplot(y=sample_quantiles, x=theoretical_quantile)
