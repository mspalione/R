library(dslabs)
data(heights)
heights

heights$height[50]
heights[ 777, 1]

class(heights)
class(heights$sex)
class(heights$height)
class("Male")
class(75.00000)

heights$sex[777]
heights[1, 777]
heights[777,1]

n <- 1000
x <- seq(1,n)
sum(x)

class(murders)
str(murders)

library(dslabs)
data(movielens)
str(movielens)

str(heights)
min(heights$height)
which.min(heights$height)
median(heights$height)
table(heights$sex)
812+238
812/1050
count <- 0
table(heights$height)
sum(heights$height > 78 & heights$sex == "Male")
f <- heights$height
f
sum