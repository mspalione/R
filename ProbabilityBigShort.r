###The Big Short
##The Big Short: Interest Rates Explained
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02 
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Monte Carlo
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

#CLT
n*(p*loss_per_foreclosure + (1-p)*0)
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

#We can now set an interest rate to guarantee that, on average, we break even. 
#Basically, we need to add a quantity  x to each loan, which in this case are represented by draws, 
#so that the expected value is 0. If we define  l to be the loss per foreclosure, we need:
#l*p + x(1-p) = 0
#which implies x is
interest_rate <- loss_per_foreclosure*p/(1-p)
loss_per_foreclosure * p + interest_rate * (1-p)

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

profit_per_loan <- loss_per_foreclosure*p + x*(1-p)
total_expected_profit <- n*(loss_per_foreclosure*p + x*(1-p)) 

#Monte Carlo
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)
mean(profit<0)
