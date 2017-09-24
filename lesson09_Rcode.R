#' =======================================
#' Working with the HELP dataset
#' 
#' Introduction to Bootstrapping
#' 
#' Melinda Higgins, PhD
#' dated 09/25/2017
#' =======================================

library(dplyr)
library(tidyverse)

# The *.Rdata file can be downloaded from the SASR website
# https://nhorton.people.amherst.edu/sasr2/datasets.php 
#
# download the dataset and put it in your working directory

# load the dataset help.Rdata
load("help.Rdata")

# list variables in HELP dataset
names(helpdata)

# let's look i1 which is "Average number of drinks (standard units) 
# consumed per day (in past 30 days) - Baseline"
summary(helpdata$i1)
mean(helpdata$i1)
sd(helpdata$i1)
hist(helpdata$i1)

# Suppose we want to know if the average number of 
# drinks consumed per day (in the past 30 days)
# significantly exceeds 15.
# Test Null Hypothesis 
#      H0: avg<=15 versus
#      Ha: avg>15

# we could (naively) run a parametric one-sample t-test for 15
# which assumes that the underlying distribution
# is normal. The distribution of i1 is NOT normal - it is
# very right skewed because it is a count
t.test(helpdata$i1,mu=15)

# suppose we assume a Poisson distribution
# which is appropriate for count data
# NOTE: To be a true Poisson the
# mean should be equal to the standard deviation (SD)
# as we see above, the mean for these i1 data
# is smaller than the SD, so it is slightly overdispersed
# overdispersion is when SD > mean
# x is the total number of observed events
# T is the sample size
# r is the "rate" expected, i.e. the "mean"
poisson.test(x=sum(helpdata$i1),
             T=length(helpdata$i1), 
             r=15)

# here is a quick plot of these distributions overlaid
# on a histogram of our data
hist(helpdata$i1, freq=F, breaks=12,
     main="Histogram of Number of Drinks Per Day")
lines(density(helpdata$i1), col="red")
x <- seq(0, 150, by=1)
y <- dnorm(x=x,
           mean=mean(helpdata$i1), 
           sd=sd(helpdata$i1))
lines(x, y, col="blue")
x <- seq(0, 150, by=1)
y <- dpois(x=x, lambda=15)
lines(x, y, col="green")
legend("topright", 
       c("data", "density","normal","Poisson"), 
       fill=c("black","red","blue","green"))

# So, our i1 data is NOT normal and it is NOT exactly Poisson
# we need another way to make estimations and inferences
# the boostrap is a good non-parametric method to use
library(boot)

# let's get a better estimate of the mean
# and inference for the mean

# x = data
# d = indices used to track each of the
# 1000 bootstrapped samples
samplemean <- function(x, d) {
  return(mean(x[d]))
}
x <- helpdata$i1
b <- boot(x, samplemean, R=1000)

# look at the summary stats
# for the 1000 means
# "t" is what the boot() method uses for
# your statistic of interest - this 
# is NOT the t-test statistics, "t" just
# stands for the statistic.
min(b$t)
max(b$t)
mean(b$t)
median(b$t)
table(b$t)
barplot(table(b$t))

# histogram of the 1000 means
hist(b$t)

# view results
plot(b)

bci.mean <- boot.ci(b, type="bca")
bci.mean

# Suppose we want to know if the average number of 
# drinks consumed per day (in the past 30 days)
# significantly exceeds 15.
# Test Null Hypothesis 
#      H0: avg<=15 versus
#      Ha: avg>15
# we still are running a two-sided test here...

# We would reject this null hypothesis
# and conclude that the mean # of drinks is > 15.
# the 95% BCa CI is (16.17, 19.83).
bci.mean

# unadjusted t-test 95% confidence intervals
tt <- t.test(helpdata$i1, conf.level = 0.95)

# 2.5th, 97.5th percentiles
# of the bootstrapped means
# these are the unadjusted CIs
qbt <- quantile(b$t,
                probs=c(.025,.975))

# 4 types possible from boot.ci()
boot.ci(b, type="all")

# =========================================
# OPTIONAL - SKIP
# table 
bci.mean$bca[4:5]
tt$conf.int
qbt

df <- data.frame(bci.mean$bca[4:5],
                 tt$conf.int,
                 qbt)

# when in rmarkdown
library(knitr)
knitr::kable(df)
