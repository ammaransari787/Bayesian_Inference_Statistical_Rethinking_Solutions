# Load required libraries:
library(rethinking)
library(dagitty)
library(tidyverse)
library(glue)


##### Practice Problems #####

# 7E1: 
# State the three motivating criteria that define information entropy. 
# Try to express each in your own words. 

# 1) The measure should be continuous,so that small changes in any of the probabilities result in proportional/expected changes in uncertainty. 
# 2) The measure should increase as the number of possible events increases. 
# 3) The measure which is the combination of two measures should equal the sum of both of those measures - measure is additive. 


# 7E2: 
# Suppose a coin is weighted such that, 
# when it is tossed and lands on a table, 
# it comes up heads 70% of the time. 
# What is the entropy of this coin? 

-(.7*log(.7) + .3*log(.3))
# 0.6108643


# 7E3: 
# Suppose a four sided die is loaded such that, when tossed onto a table, it shows 1 20%, 2 25%, 3 25% and 4 30% of the time. What is the entropy of this die? 

p = c(.2, .25, .25, .3)
-sum(p*log(p)) # = 1.376227
# or
-(.2*log(.2) + .25*log(.25) + .25*log(.25) + .3*log(.3)) # 1.376227


# 7E4: 
# Suppose another four-sided die is loaded such that it never shows 4. 
# The other three sides show equally often. 
# What is the entropy of this die?

p = c((1/3),(1/3),(1/3)) # (we ignore p = 0 bc of L'Hopital's rule - see overthinking box on page 207)

-sum(p*log(p)) # 1.098612 


# 7M1: 
# Write down and compare the definitions of AIC and WAIC.
# Which of these criteria is most general? 
# Which assumptions are required to transform the more general criterion into a less general one? 

# AIC = -2lppd + 2p
# where p is the number of free paramters in the posterior distribution.
# AIC is only reliable when:
# 1) The priors are flat or overwhelmed by the likelihood
# 2) The posteior distribution is approximately mltivariagte Gaussian
# 3) The sample size N is much greater than the number of parameters k

# WAIC makes no assumptions about the shape of the posterior. 
# It provides an approximation of the out of sample deviance that converges to the cross-validation approximation in a large sample. 
# But in a finite sample, it can disgree. It can disagree because it has a different target - 
# it isn't trying to approximate the cross-validation score, but rather guess the out-of-sample KL divergence. 
# In the large-sample limit, these tend to be the same. 

# WAIC(y, theta) = -2(lppd - (summation with index i of variance_theta*logp(yi|theta) )
# where y is the observations and theta is the posterior distribution.
# - esentially scaled lppd minus the summation of penalty terms (variance in log-probabilities for each observation i)

# WAIC is more general with regards to allowing informative priors and non-Gaussian distributions, 
# but if you don't have independent observations that are exchangeable (as you won't in a time series), WAIC is not meaningful / general.


# 7M2: 
# Explain the difference between model selection and model comparison. What information is lost under model selection? 

# Model selection is choosing the model with the lowest criterion value and then discarding the others, 
# which you should NEVER do. 
# You discard the information bout relative model accuracy contained in the differences among the CV/PSIS/WAIC values.
# The difference are useful because they can be large or small,
# and they provide advice about how confident we might be about models (conditional on the set of models compared).
# Additionally, the predictive score has nothing to do with causal models - highly confounded models can have the best CV/PSIS/WAIC. 

# By contrast, 
# model comparison uses multiple models to understand both how different variables influence predictions and, 
# in combination with a causal model, 
# how implied conditional independencies among variables help us infer causal relationships.


# 7M3: 
# When comparing models with an information criterion, 
# why must all models be fit to exactly the same observations? 
# What would happen to the information criterion values, 
# if the models were fit to different numbers of observations? 
# Perform some experiments, if you are not sure. 

compare(m5.1, m5.2, m5.3)
#       WAIC    SE dWAIC   dSE pWAIC weight
# m5.1 127.2 14.31   0.0    NA   4.5   0.77
# m5.3 129.6 15.04   2.4  1.04   6.1   0.23
# m5.2 140.0 10.80  12.7 10.61   3.4   0.00

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$Divorce)
d$M = standardize(d$Marriage)
d$A = standardize(d$MedianAgeMarriage)

d$keep_1 = rep(0, nrow(d))
d$keep_2 = rep(0, nrow(d))
d$keep_3 = rep(0, nrow(d))

set.seed(5)

i <- 1 
while (i <= nrow(d))
{
  keep_1_or_nah = rbinom(1, size = 1, prob = 0.5)
  d$keep_1[i] = keep_1_or_nah
  
  keep_2_or_nah = rbinom(1, size = 1, prob = 0.5)
  d$keep_2[i] = keep_2_or_nah
  
  keep_3_or_nah = rbinom(1, size = 1, prob = 0.5)
  d$keep_3[i] = keep_3_or_nah
  
  i <- i + 1
}

nrow(d)
nrow(subset(d, subset = d$keep_1 == 1))
nrow(subset(d, subset = d$keep_2 == 1))
nrow(subset(d, subset = d$keep_3 == 1))


d1 <- subset(d, subset = d$keep_1 == 1)

d2 <- subset(d, subset = d$keep_2 == 1)

d3 <- subset(d, subset = d$keep_3 == 1)


m5.1_keep <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d1
)

m5.2_keep <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d2
)

m5.3_keep <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d3
)

# compare(m5.1_keep, m5.2_keep,m5.3_keep) # error 

WAIC(m5.1_keep)
#       WAIC      lppd  penalty  std_err
# 1 63.76623 -28.84874 3.034381 8.813224

WAIC(m5.2_keep)
#       WAIC      lppd  penalty  std_err
# 1 68.05687 -29.45627 4.572168 11.60852

WAIC(m5.3_keep)
#       WAIC      lppd  penalty  std_err
# 1 75.37998 -32.61994 5.070052 8.604598

# New weights:
# 1: 
exp(-.5*0) / (exp(-.5*0) + exp(-.5*(68.05687 - 63.76623)) + exp(-.5*(75.37998 - 63.76623)))
# 0.8928273

# 2: 

exp(-.5*(68.05687 - 63.76623)) / (exp(-.5*0) + exp(-.5*(68.05687 - 63.76623)) + exp(-.5*(75.37998 - 63.76623)))
# 0.1044881

# 3: 
exp(-.5*(75.37998 - 63.76623)) / (exp(-.5*0) + exp(-.5*(68.05687 - 63.76623)) + exp(-.5*(75.37998 - 63.76623)))
# 0.002684564

# as compared to original:
#       WAIC    SE dWAIC   dSE pWAIC weight
# m5.1 127.2 14.31   0.0    NA   4.5   0.77
# m5.3 129.6 15.04   2.4  1.04   6.1   0.23
# m5.2 140.0 10.80  12.7 10.61   3.4   0.00
# 

# Standard errors are much different, weights are much different, standard errors are not reliable due to different values.

# Explanation for 7M3:
# Information criteria utilizes the lppd - 
# AIC multiplies it by -2 and subtracts 2p (num of parameters), 
# while WAIC subracts the penalty term before multiplying by -2. 
# However, for both, lppd is a summation that increases in absolute magnitude as the number of observations increases, 
# and when this is multiplied by the -2 scale for information criteria is its again larger. 
# Thus, smaller number of observations will result in smaller AIC/WAIC values, 
# which normally indicate better out-of-sample prediction. 
# However, this is misleading, as it is not smaller due to actual better predicition, 
# but due to less observations making the lppd that is multiplied by -2 of a smaller absolute magnitude. 
# Thus, for AIC and lppd to have any meaning at all, 
# they must be comparing models with the same number (and same exact observations), 
# as the only meaningful information gleaned from it comes from 
# the comparison of the values of two models with the same number of observations and same observations.  


# with set.seed(5), got same amount for each: let's see
compare(m5.1_keep, m5.2_keep,m5.3_keep)
#           WAIC    SE dWAIC   dSE pWAIC weight
# m5.3_keep 61.3  8.35   0.0    NA   5.3   0.76
# m5.1_keep 64.1 14.74   2.7 18.13   6.3   0.19
# m5.2_keep 66.9  7.08   5.5 10.38   3.1   0.05

# as compared to original:
#       WAIC    SE dWAIC   dSE pWAIC weight
# m5.1 127.2 14.31   0.0    NA   4.5   0.77
# m5.3 129.6 15.04   2.4  1.04   6.1   0.23
# m5.2 140.0 10.80  12.7 10.61   3.4   0.00

# Even with the same number of observations,
# these are highly dependent on the sample to provide information. Each has different values,
# and we got much different outcomes as compared to when they all had the same values.

# Testing with the same 24 for each now, changing d1,d2,d3 definition to all use keep_1 for example. 
# still different than original / expected. So both number and having the same observations matter, and all having less observations also changes model comparisons (obviously, diff paramter estimates and sampled posterior probability distributions, diff lppds)


# 7M4: 
# What happens to the effective number of parameters, 
# as measured by PSIS or WAIC, as a prior becomes more concentrated? 
# Why? 
# Perform some experiments, if you are not sure. 

# I believe the penalty will get larger, as the log-probability gets smaller due to concentrated priors,
# and the ln graph approaches negative infinity rapidly as x approaches 0. 
# This would likely increase variance. Thus WAIC would be smaller overall. Let's test though to see. (WRONG - see below)

set.seed(71)
# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N, 10, 2)

# assign treatments and simulate fungus and growth
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment*0.4)
h1 <- h0 + rnorm(N, 5 - 3*fungus)

# compose a clean data frame
d<- data.frame(h0=h0, h1=h1, treatment = treatment, fungus = fungus)
precis(d)
#            mean   sd  5.5% 94.5%    histogram
# h0         9.96 2.10  6.57 13.08 ▁▂▂▂▇▃▂▃▁▁▁▁
# h1        14.40 2.69 10.62 17.93     ▁▁▃▇▇▇▁▁
# treatment  0.50 0.50  0.00  1.00   ▇▁▁▁▁▁▁▁▁▇
# fungus     0.23 0.42  0.00  1.00   ▇▁▁▁▁▁▁▁▁▂

# Now you shoudl have a data frame d with the simulated plant experiment data. 

# Prior:

# (See written notes or book (page 171-172) for explanation of model and p and how they relate to height)
# We have to ensure that p > 0, because it is a proportion.
# Back in chapter 4 (page 96), we used a Log-Normal distribution, because it is always positive. 
# Let's use one again. 
# If we use p ~ Log-Normal(0, 0.25) the prior distribution looks like:
sim_p <- rlnorm(1e4, 0, 0.25)
precis(data.frame(sim_p))
#       mean   sd 5.5% 94.5%    histogram
# sim_p 1.04 0.26 0.67   1.5 ▁▁▃▇▇▃▁▁▁▁▁▁

# So this prior expects anything from 40% shrinkage up to 50% growth. 
# Let's fit this model, so you can see how it just measures the average growth in the experiment. 
m6.6 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0*p,
    p <- dlnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6)
#       mean   sd 5.5% 94.5%
# p     1.43 0.02 1.40  1.45
# sigma 1.79 0.13 1.59  1.99
# About 40% growth, on average.

# Here's the code to approximate the posterior with all the variables included 
# (see written notes or book (page 172) for explanation of model):
m6.7 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm(0, 0.2),
    bt ~ dnorm(0, 0.5),
    bf ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.7)

m6.8 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0*p,
    p <- a + bt*treatment,
    a ~ dlnorm(0, 0.2),
    bt ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

compare(m6.6, m6.7,m6.8)

WAIC(m6.7)
#       WAIC      lppd  penalty  std_err
# 1 361.7222 -177.1519 3.709197 14.23518

# Now changing priors to be narrower - expect a larger penalty

m6.7 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm(0, 0.2),
    bt ~ dnorm(0, .1),
    bf ~ dnorm(0, .1),
    sigma ~ dexp(1)
  ), data = d
)

WAIC(m6.7)
#       WAIC      lppd  penalty  std_err
# 1 361.5132 -177.1761 3.580494 14.15464

set.seed(1)
logprob <- sim(m6.8, ll=TRUE, n = 1e4)
n <- ncol(logprob)
ns <- nrow(logprob)
f <- function(i) log_sum_exp(logprob[,i]) - log(ns)
lppd <- sapply(1:n, f)
sum(lppd)


# Slightly smaller penalty is being subtracted from lppd (which is already negative), 
# so it information criteria is a bit smaller, model scores better

observations_ = rnorm(10, mean = 0, sd =.5)

# view(observations_)

gaussian_prob = function(observation_, mu_, std_dev_) {
  return(
    ((1) / (sqrt(2*pi*std_dev_**2))) * exp(-((observation_ - mu_)**2 / (2*std_dev_**2)))
  )
}

print(gaussian_prob(1, 0, 1))
print(gaussian_prob(1, 0, .2))

probs_observations = rep(0, length(observations_))

i <- 1
while (i <= length(observations_))
{
  probs_observations[i] <- gaussian_prob(observations_[i], 0, .2)
  i <- i + 1
}


sum(var(probs_observations))

y <- rnorm(10) # execute just once, to get data
# repeat this, changing sigma each time
m <- quap(
  alist(
    y ~ dnorm(mu,1),
    mu ~ dnorm(0,sigma)
  ),
  data=list(y=y,sigma=.2) )

WAIC(m)


# 7M5: Provide an informal explanation of why informative priors reduce overfitting

# Informative priors reduce overfitting because they make the the model less sensitive to the data, less flexible.
# Because the model isn't as sensitive to the data, 
# it learns from the data more slowly, 
# which makes it less likely to quickly learn from irregular features of the train dataset.
# In this way, the model is less likely to overfit, 
# and is more likely to generalize to improve out-of-sample performance.


# 7M6: Provide an informal explanation of why overly informative priors result in underfitting.

# If priors are too strong, the model will be very insensitive to the data, 
# not even learning the regular features of the data set we want it to pick up.
# If this is the case, the model will underfit, 
# and will not provide good predictions either in sample or out of sample - it didn't learn enough from the sample. 


# 7H1: 
# 

data(Laffer)
d <- Laffer

plot(d$tax_revenue ~ d$tax_rate)

d$REV = standardize(d$tax_revenue)
d$RATE = standardize(d$tax_rate)

plot(d$REV ~ d$RATE)

# Model definition
straight_line <- quap(
  alist(
    REV ~ dnorm(mu, sigma),
    mu <- a + bRATE*RATE,
    a ~ dnorm(0, 0.2),
    bRATE ~ dnorm(0, .75),
    sigma ~ dexp(1)
  ), data = d
)

##### Prior Predictive Simulation #####
set.seed(10)
prior <- extract.prior(straight_line)
mu <- link(straight_line, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(straight_line)

WAIC(straight_line)
PSIS(straight_line) # Throws error with high penalty / k value - use Robust instead: 

# Do a robust regression: 
robust_straight_line <- quap(
  alist(
    REV ~ dstudent(2, mu, sigma),
    mu <- a + bRATE*RATE,
    a ~ dnorm(0, 0.2),
    bRATE ~ dnorm(0, .75),
    sigma ~ dexp(1)
  ), data = d
)

##### Prior Predictive Simulation #####
set.seed(10)
prior <- extract.prior(robust_straight_line)
mu <- link(robust_straight_line, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(robust_straight_line)

WAIC(robust_straight_line)
PSIS(robust_straight_line)
# No error with robust regression! 

compare(robust_straight_line, straight_line)
plot(compare(robust_straight_line, straight_line))

compare(robust_straight_line, straight_line, func = PSIS)
plot(compare(robust_straight_line, straight_line, func = PSIS))

compare(robust_straight_line, straight_line)@dSE

# 99% interval of differnce 
15.9 + c(-1,1)*16.02*2.6 # = -25.752  57.552

# 97% interval of differnce 
15.9 + c(-1,1)*16.02*1.881 # = -14.23362  46.03362

# 89% interval of differnce 
15.9 + c(-1,1)*16.02*1.227 # = -3.75654 35.55654

# Can't definitively say robust model is better than straight, but
# That SE on non robust is huuuuuge, 
# robust is a good improvement in at least that  
# also no pareto k error is big, we can't even trust the WAIC/PSIS for straight either way.
# SO robust it is

# Plotting robust 

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(robust_straight_line, data=data.frame(RATE=rate.seq))
str(mu)


##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

sim.revenue <- sim(robust_straight_line, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)


# Plotting straight: 



# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(straight_line, data=data.frame(RATE=rate.seq))
str(mu)



##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

sim.revenue <- sim(straight_line, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)


# NEXT, quadratic 

quadratic_curve <- quap(
  alist(
    REV ~ dstudent(2, mu, sigma),
    mu <- a + bRATE*RATE + bRATE_2*RATE^2,
    a ~ dnorm(0, 0.2),
    bRATE ~ dnorm(0, .5),
    bRATE_2 ~ dnorm(0, .25),
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(quadratic_curve)
mu <- link(quadratic_curve, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(quadratic_curve)

# Plotting quadratic

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(quadratic_curve, data=data.frame(RATE=rate.seq))
str(mu)


##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)


sim.revenue <- sim(quadratic_curve, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)



compare(robust_straight_line, quadratic_curve, func = PSIS)
plot(compare(robust_straight_line, quadratic_curve, func = PSIS))

# Definitely CANNOT say quadratic is better than robust straight,
# as the standard errors of diff include both means 

compare(robust_straight_line, quadratic_curve)
plot(compare(robust_straight_line, quadratic_curve))


# 99% interval of differnce 
4.4 + c(-1,1)*4.9*2.6 # = -8.34 17.14

# 97% interval of differnce 
4.4 + c(-1,1)*4.9*1.881 # = -4.8169 13.6169

# 89% interval of differnce 
4.4 + c(-1,1)*4.9*1.227 # = -1.6123 10.4123

# Cannot definitely say that quadratic curve is better than straight line.
# Straight line is simpler. 


WAIC(robust_straight_line)
WAIC(quadratic_curve)

PSIS(quadratic_curve)
PSIS(robust_straight_line)

# PSIS(quadratic_curve, pointwise = TRUE)

plot(PSIS(quadratic_curve, pointwise = TRUE)$k,  WAIC(quadratic_curve, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)

plot(PSIS(robust_straight_line, pointwise = TRUE)$k,  WAIC(quadratic_curve, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)

# TODO maybe another curve, maybe log scale ? 

plot(REV ~ RATE, data = d)

plot(REV ~ log_RATE, data = d)

d$unstd_log_rate <- log(d$tax_rate)
d$log_RATE <- standardize(d$unstd_log_rate)


d$unstd_rev_rate = log(d$tax_revenue)

d$log_REV = standardize(d$unstd_rev_rate)

plot(log_REV ~ RATE, data = d)

ggplot(aes(x = RATE, y = log_REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(aes(x = RATE, y = REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(aes(x = log_RATE, y = REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(aes(x = log_RATE, y = log_REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# Trying cubic 

# NEXT, plot cubic 

cubic_curve <- quap(
  alist(
    REV ~ dstudent(2, mu, sigma),
    mu <- a + bRATE*RATE + bRATE_2*RATE^2 + bRATE_3*RATE^3,
    a <- dnorm(0, 0.2),
    bRATE ~ dnorm(0, .5),
    bRATE_2 ~ dnorm(0, .25),
    bRATE_3 ~ dnorm(0, .1),
    sigma ~ dexp(1)
  ), data = d
)

##### Prior Predictive Simulation #####
set.seed(10)
prior <- extract.prior(cubic_curve)
mu <- link(cubic_curve, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(cubic_curve)

# Plotting cubic

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(cubic_curve, data=data.frame(RATE=rate.seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=.89)

##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

sim.revenue <- sim(cubic_curve, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)

compare(robust_straight_line, quadratic_curve, cubic_curve,func = PSIS)
plot(compare(robust_straight_line, quadratic_curve, cubic_curve, func = PSIS))

# Not better either.


# Splines

d = d[order(d$RATE),] # so, I can do this too instead of arrange from tidyverse below

num_knots = 30
knot_list = quantile(d$RATE, probs=seq(0,1,length.out=num_knots))

precis(knot_list)

# The next choice is polynomial degree 

# Here we  contruct necessary basis function for a degree 3 (cubic) spline
library(splines)
B <- bs(d$RATE,
        knots=knot_list[-c(1,num_knots)],
        degree=3, intercept = TRUE)

nrow(B)
ncol(B)
# Each row is a temp, corresponding to the rows in the d data frame.
# Each column is a basis function, one of our synthetic variables 
# defining a span of temps within which a corresponding parameter will influence prediction. 

# To display the basis functions, just plot each column against temp:
plot(NULL, xlim=range(d$RATE), ylim=c(0,1), xlab='temp', ylab='basis')
for (i in 1:ncol(B)) lines(d$RATE, B[,i])

# model
# needs matrix multiplication for summation in mu eqution
# as well as a start list for the weights to tell quap how many there are
m7s.7 <- quap(
  alist(
    RATE ~ dnorm(mu, sigma),
    # mu <- a + B %*% w, # matrix multiplication, same as below commented out line, both do the summation in mu equation the same way
    mu <- a + sapply(1:nrow(B), function(i) sum(B[i,]*w)),
    a ~ dnorm(0,1),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data = list(RATE = d$REV, B=B), 
  start = list(w=rep(0,ncol(B))))


# could look at posterior means if you want with precis(m7s.7, depth =2),
# but it won't reveal much - you should see 17 w parameters but you can't tell what the model thinks from the parameter summaries 

# Instead we need to plot the posterior predictions.

# First, here are the weighted basis functions:
post <- extract.samples(m7s.7)
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d$RATE), ylim = c(-6,6),
     xlab='temp', ylab='basis * weight')
for (i in 1:ncol(B)) lines(d$RATE, w[i]*B[,i])

# And finally, the 97% posterior interval for mu, at each temp:
mu <- link(m7s.7)
mu_PI_3 <- apply(mu,2,PI,0.97)
mu_mean = apply(mu, 2, mean)

plot(d$RATE, d$REV, col=col.alpha(rangi2,0.3), pch=16)
shade(mu_PI_3, d$RATE, col = col.alpha('black', 0.5))

# lines(d$RATE, mu_mean)

# How many knots is correct? We'll answer that question in a few more chapters. 

compare(robust_straight_line, quadratic_curve, cubic_curve, m7s.7, func = PSIS)

#                       PSIS    SE dPSIS   dSE pPSIS weight
# quadratic_curve       70.5 14.21   0.0    NA   3.8   0.59
# cubic_curve           71.6 14.17   1.1  0.46   4.3   0.34
# robust_straight_line  74.6 13.61   4.1  4.76   3.9   0.08
# m7s.7                117.4 34.91  46.9 34.90  25.4   0.00


plot(compare(robust_straight_line, quadratic_curve, cubic_curve, m7s.7, func = PSIS))

# Splines way overfit. Great example of overfitting. Horrible prediction/performance. 
# Even changing num knots / degree, still never does better than any others.


# Explantion for 7H1:
# Robust straight line, quadratic, and cubic were similar enough
# that you can't eliminate any of the models as better than the other.
# I would say a straight line would be fine to use,
# as it is easier to interpret and not significantly worse than quadratic.


# 7H2: 
# In the Laffer data, 
# there is one country with a high tax revenue that is an outlier. 
# Use PSIS and WAIC to measure the importance of this outlier in 
# the models you fit in the previous problem. 
# Then use robust regression with a 
# Student's t distribution to revisit the curve fitting problem. 
# How much does a curved relationship depend upon the outlier point? 

# Well, I already used robust regression in all my 7H1 models - 
# I'll go ahead and see how they look without robust regression here first
# so I can answer this question. 


data(Laffer)
d <- Laffer

plot(d$tax_revenue ~ d$tax_rate)

d$REV = standardize(d$tax_revenue)
d$RATE = standardize(d$tax_rate)

# d <- subset(d, subset = d$REV < 3)

plot(d$REV ~ d$RATE)

# Model definition
straight_line <- quap(
  alist(
    REV ~ dnorm(mu, sigma),
    mu <- a + bRATE*RATE,
    a ~ dnorm(0, 0.2),
    bRATE ~ dnorm(0, .75),
    sigma ~ dexp(1)
  ), data = d
)

##### Prior Predictive Simulation #####
set.seed(10)
prior <- extract.prior(straight_line)
mu <- link(straight_line, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(straight_line)

WAIC(straight_line)
PSIS(straight_line) # Throws error with high penalty / k value - use Robust instead: 

# Do a robust regression: 
straight_line <- quap(
  alist(
    REV ~ dnorm(mu, sigma),
    mu <- a + bRATE*RATE,
    a ~ dnorm(0, 0.2),
    bRATE ~ dnorm(0, .75),
    sigma ~ dexp(1)
  ), data = d
)

##### Prior Predictive Simulation #####
set.seed(10)
prior <- extract.prior(straight_line)
mu <- link(straight_line, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(straight_line)

WAIC(straight_line)
PSIS(straight_line)
# No error with robust regression! 
# 
# compare(straight_line, straight_line)
# plot(compare(straight_line, straight_line))
# 
# compare(straight_line, straight_line, func = PSIS)
# plot(compare(straight_line, straight_line, func = PSIS))
# 
# compare(straight_line, straight_line)@dSE

# 99% interval of differnce 
15.9 + c(-1,1)*16.02*2.6 # = -25.752  57.552

# 97% interval of differnce 
15.9 + c(-1,1)*16.02*1.881 # = -14.23362  46.03362

# 89% interval of differnce 
15.9 + c(-1,1)*16.02*1.227 # = -3.75654 35.55654

# Can't definitively say robust model is better than straight, but
# That SE on non robust is huuuuuge, 
# robust is a good improvement in at least that  
# also no pareto k error is big, we can't even trust the WAIC/PSIS for straight either way.
# SO robust it is

# Plotting robust 

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(straight_line, data=data.frame(RATE=rate.seq))
str(mu)


##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

sim.revenue <- sim(straight_line, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)


# Plotting straight: 



# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(straight_line, data=data.frame(RATE=rate.seq))
str(mu)



##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

sim.revenue <- sim(straight_line, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)


# NEXT, quadratic 

quadratic_curve <- quap(
  alist(
    REV ~ dnorm(mu, sigma),
    mu <- a + bRATE*RATE + bRATE_2*RATE^2,
    a ~ dnorm(0, 0.2),
    bRATE ~ dnorm(0, .5),
    bRATE_2 ~ dnorm(0, .25),
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(quadratic_curve)
mu <- link(quadratic_curve, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(quadratic_curve)

# Plotting quadratic

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(quadratic_curve, data=data.frame(RATE=rate.seq))
str(mu)


##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)


sim.revenue <- sim(quadratic_curve, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)



compare(straight_line, quadratic_curve, func = PSIS)
plot(compare(straight_line, quadratic_curve, func = PSIS))

# Definitely CANNOT say quadratic is better than robust straight,
# as the standard errors of diff include both means 

compare(straight_line, quadratic_curve)
plot(compare(straight_line, quadratic_curve))


# 99% interval of differnce 
4.4 + c(-1,1)*4.9*2.6 # = -8.34 17.14

# 97% interval of differnce 
4.4 + c(-1,1)*4.9*1.881 # = -4.8169 13.6169

# 89% interval of differnce 
4.4 + c(-1,1)*4.9*1.227 # = -1.6123 10.4123

# Cannot definitely say that quadratic curve is better than straight line.
# Straight line is simpler. 


WAIC(straight_line)
WAIC(quadratic_curve)

PSIS(quadratic_curve)
PSIS(straight_line)

# PSIS(quadratic_curve, pointwise = TRUE)

plot(PSIS(quadratic_curve, pointwise = TRUE)$k,  WAIC(quadratic_curve, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)

plot(PSIS(straight_line, pointwise = TRUE)$k,  WAIC(quadratic_curve, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)

# TODO maybe another curve, maybe log scale ? 

plot(REV ~ RATE, data = d)

plot(REV ~ log_RATE, data = d)

d$unstd_log_rate <- log(d$tax_rate)
d$log_RATE <- standardize(d$unstd_log_rate)


d$unstd_rev_rate = log(d$tax_revenue)

d$log_REV = standardize(d$unstd_rev_rate)

plot(log_REV ~ RATE, data = d)

ggplot(aes(x = RATE, y = log_REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(aes(x = RATE, y = REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(aes(x = log_RATE, y = REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(aes(x = log_RATE, y = log_REV), data = d) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# Trying cubic 

# NEXT, plot cubic 

cubic_curve <- quap(
  alist(
    REV ~ dnorm(mu, sigma),
    mu <- a + bRATE*RATE + bRATE_2*RATE^2 + bRATE_3*RATE^3,
    a <- dnorm(0, 0.2),
    bRATE ~ dnorm(0, .5),
    bRATE_2 ~ dnorm(0, .25),
    bRATE_3 ~ dnorm(0, .1),
    sigma ~ dexp(1)
  ), data = d
)

##### Prior Predictive Simulation #####
set.seed(10)
prior <- extract.prior(cubic_curve)
mu <- link(cubic_curve, post = prior, data = list(RATE=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "rate" , ylab = 'rev')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(cubic_curve)

# Plotting cubic

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
rate.seq <- seq(from=-3.5, to=1.5, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(cubic_curve, data=data.frame(RATE=rate.seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=.89)

##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

sim.revenue <- sim(cubic_curve, data=list(RATE=rate.seq))
# sim.revenue <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.revenue)

revenue.PI <- apply(sim.revenue, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(REV ~ RATE, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(rate.seq, mu.mean)

# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, rate.seq)

# draw PI region for line # either do above HPDI or this, not both
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
shade(mu.PI, rate.seq)

# draw PI region for simulated heights 
shade(revenue.PI, rate.seq)

compare(straight_line, quadratic_curve, cubic_curve,func = PSIS)
plot(compare(straight_line, quadratic_curve, cubic_curve, func = PSIS))

# Not better either.


# So just not using robust regression it's still the same results,
# ie very similar. 

# What about removing it from the graphs ? 
# (Not recommended but I'm comparing both models + reporting both, 
# and only one point, 
# and only for the sake of answering this practice question really - 
# see page 232 for full explanation - 
# it's recommended to just use robust regression, as I did originally!)

d <- subset(d, subset = d$REV < 3)

# Explanation for 7H2:
# Still didn't change much.
# Not really due to the outlier. Just the data in general. 
# Curved relationship cannot be confirmed, with or without outlier.

# Pareto K was at 2 - very high for the outlier though. 
# Robust regression is still definitely the right decision.


# 7H3: 
# Consider three fictional polynesian islands. 
# On each there is a Royal Ornithologist 
# charged by the king with surveying the bird population. 
# They have each found the following proportions of 
# 5 important bird species:

# Note that each row sums to 1, all the birds. 
# This problem has two parts. 
# It is not computationally complicated. 
# But it is conceptually tricky. 
# First, compute the entropy of each island's bird distribution.

p_island_1 <- c(.2, .2, .2, .2, .2)
island_1_entropy = -sum(p_island_1*log(p_island_1))
island_1_entropy # 1.609438

p_island_2 <- c(.8, .1, .05, .025, .025)
island_2_entropy = -sum(p_island_2*log(p_island_2))
island_2_entropy # 0.7430039

p_island_3 <- c(.05, .15, .7, .05, .05)
island_3_entropy = -sum(p_island_3*log(p_island_3))
island_3_entropy # 0.9836003

# Interpret these entropy values. 

# Island 1 has the highest entropy. 
# I predict it will be the best at predicting the other islands,
# similar to the earth -> mars vs mars -> earth example from the text. 

# Second, use each island's bird distribution to predict the other two. 
# This means to compute the KL divergence of each island from the others, 
# treating each island as if it were a statistical model of the other islands. 
# You should end up with 6 different KL divergence values. 
# Which island predicts the others best? Why? 

# divergence = summation pilog(pi/qi)

# 1 predicting 2 (1 as p, 2 as q:) # = 0.9704061
sum(p_island_1*(log(p_island_1/p_island_2)))

# 1 predicting 3 (1 as p, 3 as q) # = 0.6387604
sum(p_island_1*(log(p_island_1/p_island_3)))

mean_divergence_1 = (0.9704061 + 0.6387604) / 2
mean_divergence_1 # = 0.8045833


# 2 predicting 1 (2 as p, 1 as q:) # = 0.866434
sum(p_island_2*(log(p_island_2/p_island_1)))

# 2 predicting 3 (2 as p, 3 as q:) # = 2.010914
sum(p_island_2*(log(p_island_2/p_island_3)))

mean_divergence_2 = (0.866434 + 2.010914) / 2
mean_divergence_2 # = 1.438674


# 3 predicting 1 (3 as p, 1 as q:) # = 0.6258376
sum(p_island_3*(log(p_island_3/p_island_1)))

# 3 predicting 2 (3 as p, 2 as q:) # = 1.838845
sum(p_island_3*(log(p_island_3/p_island_2)))

mean_divergence_3 = (0.6258376 + 1.838845) / 2
mean_divergence_3 # = 1.232341


# Explanation for 7H3:
# Smaller divergence means better at predicting. 
# As expected, island 1, the island with the highest entropy, 
# was the best at predicting the other islands -
# it performed better than 2 at predicting 3, 
# and better than 3 at predicting 2,
# and had the lowest average deviance of predicting 
# the other 2 islands, 
# among all 3 islands.
# This mirrors what we saw in the rethinking on divergence 
# direction with mars -> earth vs earth -> mars. 


# 7H4:
# Recall the marriage, age, and happiness collider bias example from Chapter 6. 
# Run models m6.9 and m6.10 again (page 178). 
# Compare these two models using WAIC (or PSIS, they will produce identical results). 
# Which model is expected to make better predictions? 
# Which model provides the correct causal inference about the influence of age on happiness? 
# Can you explain why the answers to these two questions disagree? 


d <- sim_happiness(seed = 1977, N_years = 1000)

precis(d)
#           mean    sd  5.5% 94.5%     histogram
# age       33.0 18.77  4.00 62.00 ▇▇▇▇▇▇▇▇▇▇▇▇▇
# married    0.3  0.46  0.00  1.00    ▇▁▁▁▁▁▁▁▁▃
# happiness  0.0  1.21 -1.79  1.79      ▇▅▇▅▅▇▅▇

# Now we should do our duty and think about the priors.
# Let's consider the slope bA first, 
# because how we scale the predictor A will determine the meaning of the intercept. 
# We'll focus only on the adult sample, those 18 or over. 
# Imagine a very strong relationship between age and happiness, 
# such that happiness is at its maximum at age 18 and is minimum at age 65. 
# It'll be easier if we rescale age so that the range from 18 to 65 is one unit. 
# This will do it:
d2 <- d[d$age>17, ] # only adults
d2$A <- (d2$age - 18) / (65 - 18)
# Now this new variable A ranges from 0 to 1, 
# where 0 is age 18 and 1 is age 65. 
# Happiness is on an arbitrary scale, in these data, from -2 to +2. 
# So our imaginary strongest relationship, 
# taking happiness from maximum to minimum, 
# has a slope with rise over run of (2 - (-2)) / 1 = 4. 
# Remember that 95% of the mass of a normal distribution 
# is contained within 2 standard deviations. 
# So if we set the standard deviation of the prior to half of 4, 
# we are saying that we expect 95% of plausible slopes 
# to be less than maximally strong. 
# That isn't a very strong prior, but again, 
# it at least helps bound inference to realistic ranges. 

# Now for the intercepts. 
# Each a is the value of mu_i when Ai = 0. 
# In this case, that means at age 18. 
# So we need to allow a to cover the full range of happiness scores. 
# Normal(0,1) will put 95% of the mass in the -2 to +2 interval. 
# Finally, let's approximate the posterior. 
# We need to construct the marriage status index variable, as well. 
# We'll do that, and then immediately present the quap code. 
d2$mid <- d2$married + 1

m6.9 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = d2
)

precis(m6.9, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]  -0.23 0.06 -0.34 -0.13
# a[2]   1.26 0.08  1.12  1.40
# bA    -0.75 0.11 -0.93 -0.57
# sigma  0.99 0.02  0.95  1.03

# This model is quite sure that age is negatively associated with happiness.

# We'd like to compare the inferences from this model ^ to a model that 
# omits marriage status. 
# Here it is, followed by a comparison of the marginal posterior distributions:
m6.10 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a <- dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = d2
)

precis(m6.10)
#       mean   sd  5.5% 94.5%
# a     0.00 0.08 -0.12  0.12
# bA    0.00 0.13 -0.21  0.21
# sigma 1.21 0.03  1.17  1.26

# This model, in contrast, finds no association between age and happiness. 

# The pattern above is exactly what we should expect when we condition on a collider. 
# The collider is marriage status. 
# It is a common consequence of age and happiness. 
# As a reuslt, when we condition on it, 
# we induce a spurious association between the two causes. 
# So it looks like, to model m6.9, 
# that age is negatively associated with happiness. 
# But this is just a statistical association, not a causal association. 
# Once we know whether someone is married or not, then their age does provide information about how happy they are. 

compare(m6.9, m6.10)
plot(compare(m6.9, m6.10))

#  99% - very significant difference in these models - m6.9 way more predictive
387.9 + c(-1, 1)*35.4*2.6

# Explanation of 7H4:
# m6.9 is shown to be far better at making predictions. 
# However, we demonstrated in the simulation that in terms of causal inference,
# m6.10 is more accurate in showing that there is no causal relationship between age and happiness.
# (which we know to be true as per the initial simulation).
# m6.9 is a confounded model, due to the collider bias from including
# variable marriage, which is influenced by both age and happiness. 
# However, as we discussed in this chapter, a confounded model can still make more accurate predictions
# than the correct model - which is why we must never use predictive scores to choose our models!
# In this case, I believe knowing the combination of marital status and age 
# allows for a more precise prediction of the happiness of any one person, which makes total sense
# if you consider the simulation 
# (happier people are more likely to get married, 
# if a young person is married it's likely they are happier than normal unmarried people people of the same age)
# so the model can use this information for more precise prediction of happiness based on both age and marital status.


##### 7H5 ####
# Revisit the urban fox data, 
# data(foxes), 
# from the previous chapter's practice problems. 
# Use WAIC or PSIS based model comparison on five different models, 
# each using weight as the outcome, and containing these sets of predictor variables:
# 1) avgfood + groupsize + area - check 
# 2) avgfood + groupsize - check 
# 3) groupsize + area
# 4) avgfood - check 
# 5) area - check
# Can you explain the relative differences in WAIC scores, 
# using the fox DAG from the previous chapter? 
# Be sure to pay attention to the standard error of the score differences (dSE). 


provided_fox_DAG = dagitty("dag {
                           area -> avgfood; 
                           avgfood -> groupsize; 
                           avgfood -> weight; 
                           groupsize -> weight
                           }")


drawdag(provided_fox_DAG)

adjustmentSets(provided_fox_DAG, exposure="area", outcome="weight")
# {}


# load data
data(foxes)
d <- foxes

# standardize variables 
d$Ar = standardize(d$area)
d$AF = standardize(d$avgfood)
d$W = standardize(d$weight)
d$G = standardize(d$groupsize)


# Just area:

m6.6H3 = quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bAr*Ar,
    a ~ dnorm(0, 0.2),
    bAr ~ dnorm(0, .75),
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H3)
mu <- link(m6.6H3, post = prior, data = list(Ar=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "area" , ylab = 'weight')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


plot(d$W ~ d$Ar)

# Posterior predictions 

precis(m6.6H3)
#       mean   sd  5.5% 94.5%
# a     0.00 0.08 -0.13  0.13
# bAr   0.02 0.09 -0.13  0.17
# sigma 0.99 0.06  0.89  1.09


##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
PROB_VALUE = 0.67 # he just picked all of these bc they are prime
PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
Ar_seq <- seq(from=-2.5, to=2.5, length.out = 30)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m6.6H3, data=data.frame(Ar=Ar_seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.weight <- sim(m6.6H3, data=list(Ar=Ar_seq))
# sim.weight <- sim(m6.6H3, data=list(weight=Ar_seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.weight)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.weight, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(W ~ Ar, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(Ar_seq, mu.mean)

# draw HPDI region for line
mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
shade(mu.HPDI, Ar_seq)

# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, Ar_seq)

# draw PI region for simulated heights 
shade(height.PI, Ar_seq)

PSIS(m6.6H3)
# PSIS      lppd penalty  std_err
# 1 333.9336 -166.9668 2.74112 13.92967


# just avgfood 

m6.6H37 = quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bAF*AF,
    a ~ dnorm(0, 0.2),
    bAF ~ dnorm(0, .75),
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H37)
mu <- link(m6.6H37, post = prior, data = list(AF=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "avgfood" , ylab = 'weight')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


plot(d$W ~ d$AF)

# Posterior predictions 

precis(m6.6H37)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAF   -0.02 0.09 -0.17  0.12
# sigma  0.99 0.06  0.89  1.09


##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime
range(d$AF)
# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
AF_seq <- seq(from=-2, to=2.5, length.out = 30)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m6.6H37, data=data.frame(AF=AF_seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.weight <- sim(m6.6H37, data=list(AF=AF_seq))
# sim.weight <- sim(m6.6H3, data=list(weight=Ar_seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.weight)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.weight, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(W ~ AF, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(AF_seq, mu.mean)

# draw HPDI region for line
mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
shade(mu.HPDI, AF_seq)

# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, AF_seq)

# draw PI region for simulated heights 
shade(height.PI, AF_seq)

PSIS(m6.6H37)
#       PSIS      lppd  penalty  std_err
# 1 333.4776 -166.7388 2.429206 13.89517




# Avgfood + groupsize 

m6.6H5 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bAF*AF + bG*G,
    a ~ dnorm(0, 0.2),
    bAF ~ dnorm(0, .5),
    bG ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = d
)


# # Prior predictive - use caution - unsure if this extrapolates to multiple predictors
# # To simulate from the priors, we can use extract.prior and link as in the previous chapter
# # We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# # That'll cover most of the possible range of both variables
# set.seed(10)
# prior <- extract.prior(m6.6H5)
# mu <- link(m6.6H5, post = prior, data = list(AF=c(-2,2), G = c(-2,2)))
# plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "area" , ylab = 'weight')
# for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))

# Posterior:

precis(m6.6H5)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAF    0.48 0.18  0.19  0.76
# bG    -0.57 0.18 -0.86 -0.29
# sigma  0.94 0.06  0.84  1.04


# PSIS

PSIS(m6.6H5)
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
# PSIS      lppd penalty  std_err
# 1 323.9452 -161.9726 3.74108 16.17736

set.seed(24071847)
PSIS_m6.6H5 = PSIS(m6.6H5, pointwise = TRUE)
set.seed(24071847)
WAIC_m6.6H5 = WAIC(m6.6H5, pointwise = TRUE)
plot(PSIS_m6.6H5$k, WAIC_m6.6H5$penalty, xlab = "PSIS Pareto k", ylab = "WAIC penalty", col = rangi2, lwd=2)
# Nothing crazy high - above .5 maybe but below .7 

compare(m6.6H3, m6.6H5, func = PSIS)
plot(compare(m6.6H3, m6.6H5, func = PSIS))

# 99% Credible interval of difference between models
10.1 + c(-1,1)*6.94*2.6 # = -7.944 28.144

# 97% interval of difference 
10.1 + c(-1,1)*6.94*1.881 # = -2.95414 23.15414

# 89% interval of difference 
10.1 + c(-1,1)*6.94*1.227 # = 1.58462 18.61538


# area + groupsize 

m6.6H5a7 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bAr*Ar + bG*G,
    a ~ dnorm(0, 0.2),
    bAr ~ dnorm(0, .5),
    bG ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = d
)

# Posterior:

precis(m6.6H5a7)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAr    0.41 0.15  0.17  0.64
# bG    -0.48 0.15 -0.71 -0.25
# sigma  0.94 0.06  0.84  1.04


# PSIS

PSIS(m6.6H5a7)
#      PSIS      lppd  penalty  std_err
# 1 324.097 -162.0485 3.818952 15.90554

set.seed(24071847)
PSIS_m6.6H5 = PSIS(m6.6H5, pointwise = TRUE)
set.seed(24071847)
WAIC_m6.6H5 = WAIC(m6.6H5, pointwise = TRUE)
plot(PSIS_m6.6H5$k, WAIC_m6.6H5$penalty, xlab = "PSIS Pareto k", ylab = "WAIC penalty", col = rangi2, lwd=2)
# Nothing crazy high - above .5 maybe but below .7 

compare(m6.6H3, m6.6H5, func = PSIS)
plot(compare(m6.6H3, m6.6H5, func = PSIS))

# 99% Credible interval of difference between models
10.1 + c(-1,1)*6.94*2.6 # = -7.944 28.144

# 97% interval of difference 
10.1 + c(-1,1)*6.94*1.881 # = -2.95414 23.15414

# 89% interval of difference 
10.1 + c(-1,1)*6.94*1.227 # = 1.58462 18.61538






# Avgfood + groupsize + area 

m6.6H57 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bAF*AF + bG*G + bAr*Ar,
    a ~ dnorm(0, 0.2),
    bAF ~ dnorm(0, .5),
    bG ~ dnorm(0, .5),
    bAr ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = d
)

# # Prior predictive - use caution - unsure if this extrapolates to multiple predictors
# # To simulate from the priors, we can use extract.prior and link as in the previous chapter
# # We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# # That'll cover most of the possible range of both variables
# set.seed(10)
# prior <- extract.prior(m6.6H57)
# mu <- link(m6.6H57, post = prior, data = list(AF=c(-2,2), G = c(-2,2), Ar = c(-2,2)))
# plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "predictors" , ylab = 'weight')
# for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(m6.6H57)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAF    0.30 0.21 -0.04  0.63
# bG    -0.64 0.18 -0.93 -0.35
# bAr    0.28 0.17  0.01  0.55
# sigma  0.93 0.06  0.83  1.03

plot(precis(m6.6H57))

drawdag(provided_fox_DAG)

adjustmentSets(provided_fox_DAG, exposure = 'groupsize', outcome = 'weight')

PSIS(m6.6H57)
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
# PSIS      lppd penalty  std_err
# 1 323.3262 -161.6631 4.82245 16.38937

set.seed(24071847)
PSIS_m6.6H57 = PSIS(m6.6H57, pointwise = TRUE)
set.seed(24071847)
WAIC_m6.6H57 = WAIC(m6.6H57, pointwise = TRUE)
plot(PSIS_m6.6H57$k, WAIC_m6.6H57$penalty, xlab = "PSIS Pareto k", ylab = "WAIC penalty", col = rangi2, lwd=2)
# again, nothing crazy high. while greater than .5, less than .7 for sure 


compare(m6.6H3, m6.6H5,m6.6H57,m6.6H37,m6.6H5a7, func = PSIS)
plot(compare(m6.6H3, m6.6H5,m6.6H57,m6.6H37,m6.6H5a7, func = PSIS))
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
#           PSIS    SE dPSIS  dSE pPSIS weight
# m6.6H57  323.2 16.49   0.0   NA   4.8   0.43
# m6.6H5a7 324.0 15.88   0.8 2.99   3.8   0.29
# m6.6H5   324.0 16.19   0.8 3.55   3.8   0.28
# m6.6H37  333.6 13.90  10.4 7.28   2.5   0.00
# m6.6H3   334.0 13.99  10.8 7.33   2.8   0.00

# 99% credible interval of diff between area_groupsize_avgfood vs groupsize_area
0.8 + c(-1, 1)*2.99*2.6 # = -6.974  8.574

# 97% credible interval of diff between area_groupsize_avgfood vs groupsize_avgfood
3.52 + c(-1, 1)*3.6*1.881 # = -3.2516 10.2916

# 89% credible interval of diff between area_groupsize_avgfood vs groupsize_avgfood
3.52 + c(-1, 1)*3.6*1.227 # = -0.8972  7.9372

# First of all, pretty interesting that we can't see a predictive difference
# between area_groupsize_avgfood model vs groupsize_avgfood model/area_groupsize model,
# when we consider the causal DAG - avgfood is a descendent of area in the pipe 
# area -> avgfood -> weight so by that logic learning area after we 
# already have avgfod shouldn't tell us anything more, so it's 
# interesting this reflects that. 
# Similarly, avgfood is a descendent of area so the area_groupsize model also
# containing the same information as the other two makes sense as well. 
# This fact ^ also explains why the just area and just average food 
# models have practically the exact same prediction - they're giving 
# the same information about weight essentially, as far as the predicitive power goes.
# This highlights how the predictive model doesn't care about causal inference at all,
# and just looks at statistical associations. Dangerous if misused -
# we must always remember that we shouldn't use these tools for discovering causal models. 
# We would have completely missed the need to include area at all, or potentially
# one of the two of avgfood/area. These tools are for prediction, NOT causal inference/models selection. 




