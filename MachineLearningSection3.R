###Section 3 Machine Learning
# Linear Regression for Prediction, Smoothing, and Working with Matrices

rm(list = ls())

library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

##Linear regression
#Linear regression can be considered a machine learning algorithm. Although it can 
#be too rigid to be useful, it works rather well for some challenges. It also serves as a 
#baseline approach: if you can't beat it with a more complex approach, you probably want to 
#stick to linear regression. 

library(HistData)

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Suppose you are tasked with building a machine learning algorithm that predicts 
#the son's height  Y  using the father's height  X. Let's generate testing and training sets:
  
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#In this case, if we were just ignoring the father's height and guessing the son's height, 
#we would guess the average height of sons.
m <- mean(train_set$son) #69.18182

#Our squared loss is:
mean((m - test_set$son)^2) #7.651849

#Can we do better? In the regression chapter, we learned that if the pair (X, Y) follow a bivariate normal distribution, 
#the conditional expectation (what we want to estimate) is equivalent to the regression line:
  #f(x) = E(Y???X = x) = ??0 + ??1x

#In Section 18.3 we introduced least squares as a method for estimating the slope ??0 and intercept ??1:
  
fit <- lm(son ~ father, data = train_set)
fit$coef
#(Intercept)      father 
#35.9756122   0.4816698 

#This gives us an estimate of the conditional expectation:
# f(x) = 52 + 0.25x
#We can see that this does indeed provide an improvement over our guessing approach.
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2) #6.470245

##The predict function
#The predict function takes a fitted object from functions such as lm or glm 
#and a data frame with the new predictors for which to predict.
y_hat <- predict(fit, test_set)

#Using predict, we can get the same results as we did previously:
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)
# 6.47


##Logistic regression
library(dslabs)
data("heights")
y <- heights$height

set.seed(2) #if you are using R 3.5 or earlier

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#The regression approach can be extended to categorical data. In this section we 
#first illustrate how, for binary data, one can simply assign numeric values of 0 and 1 
#to the outcomes y, and apply regression as if the data were continuous. 
#We will then point out a limitation with this approach and introduce logistic regression as a solution. 
#Logistic regression is a specific case of a set of generalized linear models. 
#To illustrate logistic regression, we will apply it to our previous predicting sex example:
  
  #If we define the outcome Y as 1 for females and 0 for males, and X as the height, 
  #we are interested in the conditional probability:
#  Pr(Y = 1 ??? X = x)

#As an example, let's provide a prediction for a student that is 66 inches tall. 
#What is the conditional probability of being female if you are 66 inches tall? 
#In our dataset, we can estimate this by rounding to the nearest inch and computing:
train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))
#>   y_hat
#>   0.327

#To construct a prediction algorithm, we want to estimate the proportion of the 
#population that is female for any given height X = x, which we write as the conditional probability
#  Pr(Y = 1 ??? X = x)
#Let's see what this looks like for several values of x (we will remove strata of x with few data points):
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

#Since the results from the plot above look close to linear, we will try regression. We assume that:
# p(x) = Pr(Y = 1 | X = x) = ??0 + ??1x (Conditional Probability of Y = 1 given x is a line intercept + slope 8 height)
# Note: beacuse p0(x) = 1 - pa(x), we wo;; only estimate p1(x) and drop the 1 index.
#If we convert the factors to 0s and 1s, we can estimate ??0 and ??1 with least squares.
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)

#Once we have estimates ??0 and ??1, we can obtain an actual prediction. Our estimate 
#of the conditional probability p(x) is:
# p(x) = ??0 + ??1x
#To form a prediction, we define a decision rule: predict female if p(x) > 0.5. 
#We can compare our predictions to the outcomes using:
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)[["Accuracy"]]

#The function ??0 + ??1x can take any value including negatives and values larger than 1. 
#In fact, the estimate p(x) computed in the linear regression section does indeed become negative at around 76 inches.
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

#The range is
range(p_hat) # -0.331 1.036

#n R, we can fit the logistic regression model with the function glm: generalized linear models. 
#This function is more general than logistic regression so we need to specify the model we want through 
#the family parameter:
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")

#We can obtain prediction using the predict function:
  p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
  
#When using predict with a glm object, we have to specify that we want type="response" 
#if we want the conditional probabilities, since the default is to return the logistic transformed values.

#This model fits the data slightly better than the line.
#Because we have an estimate p(x), we can obtain predictions:
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)[["Accuracy"]]

## Logistic regression with more than one predictor
#we are interested in estimating a conditional probability that depends on two variables.
#To fit the model, use the following code:
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test, type="response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]

##Smoothing
#Other names given to this technique are curve fitting and low pass filtering.
#It is designed to detect trends in the presence of noisy data in cases in which the shape of the trend is unknown
#the concepts behind smoothing techniques are extremely useful in machine learning 
#because conditional expectations/probabilities can be thought of as trends of unknown shapes that 
#we need to estimate in the presence of uncertainty.

#estimate the time trend in the 2008 US popular vote poll margin (difference between Obama and McCain).
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

##Bin Smoothing
#In smoothing, we call the size of the interval the window size, bandwidth or span. 
#The idea behind bin smoothing is to make this calculation with each value of x as the center. 
#In the poll example, for each day, we would compute the average of the values within a week 
#with that day in the center. 
#By computing this mean for every point, we form an estimate of the underlying curve f(x)
#The final code and resulting estimate look like this:
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

##Kernels
#The final result from the bin smoother is quite wiggly. One reason for this is that each time the 
#window moves, two points change. We can attenuate this somewhat by taking weighted averages that give 
#the center point more weight than far away points, with the two points at the edges receiving very little weight.
# In the code above, we used the argument kernel="box" in our call to the function ksmooth. 
#This is because the weight function looks like a box. The ksmooth function provides a "smoother" 
#option which uses the normal density to assign weights.
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

##Local weighted regression (loess)
#A limitation of the bin smoother approach just described is that we need small 
#windows for the approximately constant assumptions to hold. As a result, we end up 
#with a small number of data points to average and obtain imprecise estimates
#local weighted regression (loess) permits us to consider larger window sizes
#The final result is a smoother fit than the bin smoother since we use larger sample sizes to estimate our local parameters:
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

