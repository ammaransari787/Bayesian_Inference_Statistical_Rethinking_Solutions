##### Practice Problems #####

# 4M1: For the model definition below, simulate observed y values from the prior (not the posterior)
# y_i ~ Normal(mu, sigma)
# mu ~ Normal(0,10)
# sigma ~ Exponential(1)

# Prior predictive simulation 
sample_mu = rnorm(1e4, 0, 10)
sample_sigma = rexp(1e4, 1)
prior_h = rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)


# 4M2: Translate the model above into a quap formula 

# alist of formulas for likelihood and priors for mu/sigma
flist = alist(
  y_ ~ dnorm(mu, sigma), # likelihood
  mu ~ dnorm(0, 10), # prior for mu
  sigma ~ dexp(1) # prior for sigma 
)

# Fit the model to the data in the dataframe d2:
# m4M2.1 = quap(flist, data = theoretical)


# 4M7: Refit model m4.3 from the chapter, but omit the mean weight xbar this time. 
# Compare the new model's posterior to that of the original model. 
# In particular, look at the covariance among the pararameters.  
# What is different? Then compare the posterior predictions of both models.


data(Howell1); d = Howell1; d2 = d[d$age >= 18,]

# define the average weight, x-bar
xbar = mean(d2$weight)

# fit model 
m4.3 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*( weight - xbar), 
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)
  ), data = d2
)

# Tables of marginal distributions 
precis(m4.3)
#          mean  sd    5.5%  94.5%
# a     154.60 0.27 154.17 155.03
# b     0.90   0.04   0.84   0.97
# sigma 5.07   0.19   4.77   5.38

# Remember, the numbers in the default precis output aren't sufficient to describe the quadratic posterior completely.  
# For that, we also require the variance-covariance matrix. 
# You can see the covariances among the parameters with vcov:
round(vcov(m4.3), 3)
#           a     b sigma
# a     0.073 0.000 0.000
# b     0.000 0.002 0.000
# sigma 0.000 0.000 0.037

# He didn't do this this time, but I'm going to - correlation of covariances 
cov2cor(vcov(m4.3))
#                   a             b        sigma
# a      1.000000e+00 -3.745595e-06  0.001191773
# b     -3.745595e-06  1.000000e+00 -0.003142877
# sigma  1.191773e-03 -3.142877e-03  1.000000000
# ^ Very small, very very close to zero, meaning very little covariation among parameters in this case. 

# Using pairs(m4.3 shows both the marginal posteriors and the covariance)
pairs(m4.3)
# In the practice problems at the end of the chpater, you'll see that the lack of covariance among the parameters results from centering. 


data(Howell1); d = Howell1; d2 = d[d$age >= 18,]

# define the average weight, x-bar
xbar = mean(d2$weight)

# fit model 
m4.3_no_xbar <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*( weight), 
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)
  ), data = d2
)

# Tables of marginal distributions 
precis(m4.3_no_xbar)
#         mean   sd   5.5%  94.5%
# a     114.53 1.90 111.50 117.56
# b       0.89 0.04   0.82   0.96
# sigma   5.07 0.19   4.77   5.38

# Remember, the numbers in the default precis output aren't sufficient to describe the quadratic posterior completely.  
# For that, we also require the variance-covariance matrix. 
# You can see the covariances among the parameters with vcov:
round(vcov(m4.3_no_xbar), 3)
#            a      b sigma
# a      3.602 -0.078 0.009
# b     -0.078  0.002 0.000
# sigma  0.009  0.000 0.037

# He didn't do this this time, but I'm going to - correlation of covariances 
cov2cor(vcov(m4.3_no_xbar))
#                 a           b       sigma
# a      1.00000000 -0.98980046  0.02567021
# b     -0.98980046  1.00000000 -0.02545425
# sigma  0.02567021 -0.02545425  1.00000000
# HUGE almost perfect negative correlation between a and b! Just from not centering, wow 

# Using pairs(m4.3_no_xbar shows both the marginal posteriors and the covariance)
pairs(m4.3_no_xbar)
# In the practice problems at the end of the chpater, you'll see that the lack of covariance among the parameters results from centering. 


# posterior predictions for m4.3:

##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
PROB_VALUE = 0.67 # he just picked all of these bc they are prime
PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
weight.seq <- seq(from=25, to=70, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m4.3, data=data.frame(weight=weight.seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=.89)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.height <- sim(m4.3, data=list(weight=weight.seq))
# sim.height <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.height)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(height ~ weight, d2, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw HPDI region for line
mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
shade(mu.HPDI, weight.seq)

# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, weight.seq)

# draw PI region for simulated heights 
shade(height.PI, weight.seq)


# Posterior predictions for m4.2_no_xbar:

##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
PROB_VALUE = 0.67 # he just picked all of these bc they are prime
PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
weight.seq <- seq(from=25, to=70, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m4.3_no_xbar, data=data.frame(weight=weight.seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=.89)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.height <- sim(m4.3_no_xbar, data=list(weight=weight.seq))
# sim.height <- sim(m4.3_no_xbar, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.height)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(height ~ weight, d2, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw HPDI region for line
mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
shade(mu.HPDI, weight.seq)

# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, weight.seq)

# draw PI region for simulated heights 
shade(height.PI, weight.seq)


# 4M8: In the chapter, we used 15 knots with the cherry blossom spline.
# Increase the number of knots and observe what happens to the resulting spline. 
# Then adjust also the width of the prior on the weights - change the standard deviation of the prior and watch what happens. 
# What do you think the combination of knot number and the prior on the weights controls?

# Splines 

# loading data
data(cherry_blossoms)
d = cherry_blossoms
precis(d)

# plot doy of cherry blossom vs year 
plot(doy ~ year, data = d)

# First, we choose the knots, both amount and where they go

# creating knots (here we create 15 and at evenly spaced quantiles of predictor variables) 
d2 = d[complete.cases(d$doy), ]
num_knots = 15
knot_list = quantile(d2$year, probs=seq(0,1,length.out=num_knots))

precis(knot_list)

# The next choice is polynomial degree 

# Here we  contruct necessary basis function for a degree 3 (cubic) spline
library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)],
        degree=3, intercept = TRUE)

nrow(B)
ncol(B)
# Each row is a year, corresponding to the rows in the d2 data frame.
# Each column is a basis function, one of our synthetic variables 
# defining a span of years within which a corresponding parameter will influence prediction. 

# To display the basis functions, just plot each column against year:
plot(NULL, xlim=range(d2$year), ylim=c(0,1), xlab='year', ylab='basis')
for (i in 1:ncol(B)) lines(d2$year, B[,i])

# model
# needs matrix multiplication for summation in mu eqution
# as well as a start list for the weights to tell quap how many there are
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w, # matrix multiplication, same as below commented out line, both do the summation in mu equation the same way
    # mu <- a + sapply(1:nrow(B), function(i) sum(B[i,]*w)),
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data = list(D = d2$doy, B=B), 
  start = list(w=rep(0,ncol(B))))


# could look at posterior means if you want with precis(m4.7, depth =2),
# but it won't reveal much - you should see 17 w parameters but you can't tell what the model thinks from the parameter summaries 

# Instead we need to plot the posterior predictions.

# First, here are the weighted basis functions:
post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d2$year), ylim = c(-6,6),
     xlab='year', ylab='basis * weight')
for (i in 1:ncol(B)) lines(d2$year, w[i]*B[,i])

# And finally, the 97% posterior interval for mu, at each year:
mu <- link(m4.7)
mu_PI <- apply(mu,2,PI,0.97)
plot(d2$year, d2$doy, col=col.alpha(rangi2,0.3), pch=16)
shade(mu_PI, d2$year, col = col.alpha('black', 0.5))

# The number of knots and sigma of the weight affect the number and slope of the combination of b-splines, respectively. 
# The number of knots changes the amount of changes in general shape of the combination in spline, 
# and the sigma of the weight effects the actual weight value the basis is multiplied by, which affects slope in that zone,
# smaller sigma leads to flatter slopes, it appears. 


# 4H1: 

data(Howell1); d = Howell1; d2 = d[d$age >= 18,]

# define the average weight, x-bar
xbar = mean(d2$weight)

# fit model 
m4.3 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*( weight - xbar), 
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)
  ), data = d2
)

post = extract.samples(m4.3)

NEW_VALUE = 54.63

mu_at_new_value = rnorm(1e4, post$a + post$b*(NEW_VALUE-xbar), post$sigma)

mean(mu_at_new_value)

PI(mu_at_new_value, .89)


# 4H2: Select out all the rows in the Howell1 data with ages below 18 years of age.
# If you do it right, you should end up with a new df with 192 rows. 

data(Howell1); d = Howell1; d2 = d[d$age < 18,]

nrow(d2)

# A) 
# Fit a linear regression to these data, using quap. 
# Present and interpret the estimates. 
# For every 10 units of increase in weight, how much taller does the model predict  a child gets?

# define the average weight, x-bar
xbar = mean(d2$weight)

# fit model 
m4H2.1 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*( weight - xbar), 
    a ~ dnorm(100, 40),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)
  ), data = d2
)

# Simulating heights from the model, using only the priors #
set.seed(2971)
N = 100 # 100 lines
a = rnorm(N, 100, 40)
b = rlnorm(N, 0, 1)
# Now we have 100 pairs of a and B values. Now to plot the lines:
plot(NULL, xlim = range(d2$weight), ylim = c(-100, 400), xlab = "weight", ylab = "height")
abline(h=0, lty=2)
abline(h=272, lty=1, lwd = 0.5) # tallest person flat line
xbar = mean(d2$weight)
for(i in 1:N) curve(a[i] + b[i]*(x - xbar),
                    from = min(d2$weight), to=max(d2$weight), add = TRUE, col=col.alpha('black', 0.2))

# Tables of marginal distributions 
precis(m4H2.1)

# Remember, the numbers in the default precis output aren't sufficient to describe the quadratic posterior completely.  
# For that, we also require the variance-covariance matrix. 
# You can see the covariances among the parameters with vcov:
round(vcov(m4H2.1), 3)

# He didn't do this this time, but I'm going to - correlation of covariances 
cov2cor(vcov(m4H2.1))

# Using pairs(m4.3 shows both the marginal posteriors and the covariance)
pairs(m4H2.1)

# I think it would increase by 27.2 cm (10*slope), but let's test:

post = extract.samples(m4H2.1)

# NEW_VALUE = 14.63
NEW_VALUE = 24.63

mu_at_new_value = rnorm(1e4, post$a + post$b*(NEW_VALUE-xbar), post$sigma)

mean(mu_at_new_value)

PI(mu_at_new_value, .89)

125.1997 - 98.01688 # = 27.18282, so yes, v close to 27.2

# B) 
# Plot the raw data, with height on the vertical axis and weight on the horizontal axis. 
# Super impose the MAP regerssion line and the 89% interval for the mean. 
# Also superimpose the 89% interval for predicted heights.

# posterior predictions for m4H2.1:

PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
weight.seq <- seq(from=0, to=45, by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m4H2.1, data=data.frame(weight=weight.seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=.89)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.height <- sim(m4H2.1, data=list(weight=weight.seq))
# sim.height <- sim(m4H2.1, data=list(weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.height)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(height ~ weight, d2, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw HPDI region for line
mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
shade(mu.HPDI, weight.seq)

# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, weight.seq)

# draw PI region for simulated heights 
shade(height.PI, weight.seq)

# C) 
# What aspects of the model fit concern you? 
# Describe the kinds of assumption you would change, if any, to improve the model. 
# You don't have to write any new code. 
# Just explain what the model appears to be doing a bad job of, and what you hypothesize would be a better model.

# One thing that concerns me is the fact that data appears to have a curve, yet I used a straight linear model. 
# I would change the assumtpion that a line is the best fit of this data - instead, a polynomial function may work better.
# The model is doing a bad job at curving to fit this apparent curvilinear relationship.
# A quadratic or cubic polynomial linear model might fit these data better. 

# 4H3: Suppose a colleague of yours, who works on allometry, glances at the practice problems just above. 
# Your colleague exclaims, "That's silly. Everyone knows that it's only hte logarithm of body weight that scales with height!"
# Let's take your colleague's advice and see what happens. 

# A) 
# Model the relationship between height (cm) and the natural logarithm of weight (log-kg). 
# Use the entire Howell1 data frame, all 544 rows, adults and non-adults. 
# Can you interpret the resulting estimates?

data(Howell1)
d = Howell1

d$log_weight = log(d$weight)

# plot(height ~ log_weight, data = d)

# define the average weight, x-bar
xbar = mean(d$log_weight)

##### Prior predictive simulation #####
# Simulating heights from the model, using only the priors #
set.seed(2971)
N = 100 # 100 lines
a = rnorm(N, 150, 50)
b = rlnorm(N, 3, .5)
# Now we have 100 pairs of a and B values. Now to plot the lines:
plot(NULL, xlim = range(d$log_weight), ylim = c(-100, 400), xlab = "weight", ylab = "height")
abline(h=0, lty=2)
abline(h=272, lty=1, lwd = 0.5) # tallest person flat line
xbar = mean(d$log_weight)
for(i in 1:N) curve(a[i] + b[i]*(x - xbar),
                    from = min(d$log_weight), to=max(d$log_weight), add = TRUE, col=col.alpha('black', 0.2))


# fit model 
m4H3.1 <- quap(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*(log_weight - xbar), 
    a ~ dnorm(150, 50),
    b ~ dlnorm(3, .5),
    sigma ~ dunif(0,75)
  ), data = d
)

# Tables of marginal distributions 
precis(m4H3.1)
#         mean   sd   5.5%  94.5%
# a     138.26 0.22 137.91 138.62
# b      47.07 0.38  46.46  47.68
# sigma   5.13 0.16   4.89   5.38

# So, for each increase in 1 of log-weight (kg), 
# we expect a 47.07 cm increase in height. 


# B)
# Begin with this plot:
# plot(height ~ weight, data = Howell1). 
# Then use samples from the quadratic approximate posterior of the model in (A) to superimpose on the plot:
# 1) the predicted mean height as a function of weight
# 2) the 97 % interval for the mean, and 
# 3) the 97% interval for predicted heights

##### CORRECT WAY FOR B (SECOND ATTEMPT) #####

# Actually, below this portion was my first attempt,
# but I don't actually think it's what they want. 
# Trying again 
plot(height ~ weight, data = Howell1)

PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
weight.seq <- seq(from=round(min(d$weight)-1), to=round(max(d$weight)+1), by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m4H3.1, data=data.frame(log_weight=log(weight.seq)))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.height <- sim(m4H3.1, data=list(log_weight=log(weight.seq)))
# sim.height <- sim(m4H3.1, data=list(log_weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.height)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(height ~ weight, d, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw HPDI region for line
mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
shade(mu.HPDI, weight.seq)

# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, weight.seq)

# draw PI region for simulated heights 
shade(height.PI, weight.seq)

##### END CORRECT WAY FOR B (SECOND ATTEMPT) #####

##### INCORRECT WAY FOR B (first attempt, but still useful to know how to do so keeping it here) ######
# The below gets the does the plot + shades for the 
# line of log-weight vs height, and uses the overthinking
# box on page 114 technique to get corrected weights ( aka exp(log_weight)) as the labels, pretty cool

# PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
weight.seq <- seq(from=1.0, to=4.5, by=.5)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m4H3.1, data=data.frame(log_weight=weight.seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.height <- sim(m4H3.1, data=list(log_weight=weight.seq))
# sim.height <- sim(m4H3.1, data=list(log_weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.height)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# # plot raw data
# plot(height ~ log_weight, d, col = col.alpha(rangi2,0.5))
# 
# # draw MAP line
# lines(weight.seq, mu.mean)
# 
# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, weight.seq)
# 
# # # draw PI region for line # either do above HPDI or this, not both
# # mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# # shade(mu.PI, weight.seq)
# 
# # draw PI region for simulated heights
# shade(height.PI, weight.seq)

# Overthinking - changing to original scale for above graph instead:
# First, turn of the horizontal axis when plotting raw data with xaxt = 'n'
plot(height ~ log_weight, d, col=col.alpha(rangi2,0.5), xaxt='n', xlab = 'weight')
# he didn't, but can also add all the extra lines/shades
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)
# 
# Then, explicitly construct the axis, using the axis function
at = c(1.5, 2, 2.5, 3, 3.5, 4)
labels = exp(at) # convert standardized units back to original scale
axis(side = 1, at=at, labels = round(labels, 1))

##### END INCORRECT WAY FOR B (first attempt, but still useful to know how to do so keeping it here) #####


# 4H4: Plot the prior predictive distribution for the 
# parabolic polynomial regression model in the chapter. 
# You can modify the code that plots the 
# linear regression prior predictive distribution. 
# Can you modify the prior distributions of a, B1 and B2 
# so that the prior predictions stay within 
# the biologically reasonable outcome space? 
# That is to say: Do not try to fit the data by hand. 
# But do try to keep the curves consistent with 
# what you know about height and weight, 
# before seeing these exact data. 


data(Howell1)
d = Howell1

d$weight_s = (d$weight - mean(d$weight)) / sd(d$weight) # z score


# Prior predictive simulation
# Simulating heights from the model, using only the priors #
set.seed(2971)
N = 100 # 100 lines
a = rnorm(N, 150, 50)
b1 = rlnorm(N, 3, .5)
b2 = rnorm(N, -1, 10)
# Now we have 100 pairs of a and B values. Now to plot the lines:
plot(NULL, xlim = range(d$weight_s), ylim = c(-100, 400), xlab = "standardized weight", ylab = "height")
abline(h=0, lty=2)
abline(h=272, lty=1, lwd = 0.5) # tallest person flat line
# xbar = mean(d$log_weight)
for(i in 1:N) curve(a[i] + b1[i]*(x) + b2[i]*(x^2),
                    from = min(d$weight_s), to=max(d$weight_s), add = TRUE, col=col.alpha('black', 0.2))


# 4H5: Return to data(cherry_blossoms) and model the 
# association between blossom date(doy) and march temperature (temp). 
# Note that there are many missing values in both variables.  
# You may consider a linear model, a polynomial, or a spline on temperature 
# How well does temperature trend predict the blossom trend? 

data(cherry_blossoms)
cherry_blossoms_df = cherry_blossoms

nrow(cherry_blossoms_df)
cherry_blossoms_df = subset(cherry_blossoms_df, subset = !is.na(cherry_blossoms_df$doy))
nrow(cherry_blossoms_df)
cherry_blossoms_df = subset(cherry_blossoms_df, subset = !is.na(cherry_blossoms_df$temp))
nrow(cherry_blossoms_df)

colSums(is.na(cherry_blossoms_df))

plot(cherry_blossoms_df$doy ~ cherry_blossoms_df$temp, data = cherry_blossoms_df)
plot(cherry_blossoms_df$temp ~ cherry_blossoms_df$doy, data = cherry_blossoms_df)

plot(log(cherry_blossoms_df$doy) ~ cherry_blossoms_df$temp, data = cherry_blossoms_df)
plot(cherry_blossoms_df$doy ~ log(cherry_blossoms_df$temp), data = cherry_blossoms_df)

precis(cherry_blossoms_df)

range(cherry_blossoms_df$year)
plot(cherry_blossoms_df$doy ~ cherry_blossoms_df$year, data = cherry_blossoms_df)

cherry_blossoms_df_pre_1900 = subset(cherry_blossoms_df, subset = cherry_blossoms_df$year <= 1900)
cherry_blossoms_df_post_1900 = subset(cherry_blossoms_df, subset = cherry_blossoms_df$year > 1900)

precis(cherry_blossoms_df_pre_1900)
precis(cherry_blossoms_df_post_1900)

# I think this log transformation is the most linear: 
# plot(cherry_blossoms_df$doy ~ log(cherry_blossoms_df$temp), data = cherry_blossoms_df)

# Starting that log linear model now: 

data(cherry_blossoms)
cherry_blossoms_df = cherry_blossoms

nrow(cherry_blossoms_df)
cherry_blossoms_df = subset(cherry_blossoms_df, subset = !is.na(cherry_blossoms_df$doy))
nrow(cherry_blossoms_df)
cherry_blossoms_df = subset(cherry_blossoms_df, subset = !is.na(cherry_blossoms_df$temp))
nrow(cherry_blossoms_df)

cherry_blossoms_df$log_temp = log(cherry_blossoms_df$temp)

# plot(cherry_blossoms_df$doy ~ log(cherry_blossoms_df$temp), data = cherry_blossoms_df)

# define the average weight, x-bar
xbar = mean(cherry_blossoms_df$log_temp)

# Prior predictive simulation
# Simulating heights from the model, using only the priors #
set.seed(2971)
N = 100 # 100 lines
a = rnorm(N, 100, 30)
b = rnorm(N, 0, 10)
# Now we have 100 pairs of a and B values. Now to plot the lines:
plot(NULL, xlim = range(cherry_blossoms_df$log_temp), ylim = c(0, 365), xlab = "log-temp", ylab = "doy")
xbar = mean(cherry_blossoms_df$log_temp)
for(i in 1:N) curve(a[i] + b[i]*(x - xbar),
                    from = min(cherry_blossoms_df$log_temp), to=max(cherry_blossoms_df$log_temp), add = TRUE, col=col.alpha('black', 0.2))

# fit model 
cherry_blossom_log_temp_doy <- quap(
  alist(
    doy ~ dnorm(mu,sigma),
    mu <- a + b*(log_temp - xbar), 
    a ~ dnorm(100, 30),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0,30)
  ), data = cherry_blossoms_df
)

# Tables of marginal distributions 
precis(cherry_blossom_log_temp_doy)
#         mean   sd   5.5%  94.5%
# a     104.92 0.21 104.58 105.26
# b     -17.88 1.88 -20.89 -14.87
# sigma   5.91 0.15   5.67   6.15

# Start correct B version 

# Actually, below this portion was my first attempt,
# but I don't actually think it's what they want. 
# Trying again 
plot(doy ~ temp, data = cherry_blossoms_df)

PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
weight.seq <- seq(from=round(min(cherry_blossoms_df$temp)-1), to=round(max(cherry_blossoms_df$temp)+1), by=1)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(cherry_blossom_log_temp_doy, data=data.frame(log_temp=log(weight.seq)))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.height <- sim(cherry_blossom_log_temp_doy, data=list(log_temp=log(weight.seq)))
# sim.height <- sim(m4H3.1, data=list(log_weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.height)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# plot raw data
plot(doy ~ temp, cherry_blossoms_df, col = col.alpha(rangi2,0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw HPDI region for line
mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
shade(mu.HPDI, weight.seq)

# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, weight.seq)

# draw PI region for simulated heights 
shade(height.PI, weight.seq)

# End correct B version 

# ggplot tests:
ggplot(aes(x = temp, y= doy), data = cherry_blossoms_df) + geom_smooth(method = 'lm') + geom_point()

ggplot(aes(x = log_temp, y= doy), data = cherry_blossoms_df) + geom_smooth(method = 'lm') + geom_point()

# Start incorrect (but interesting) B version

# The below gets the does the plot + shades for the 
# line of log-weight vs height, and uses the overthinking
# box on page 114 technique to get corrected weights ( aka exp(log_weight)) as the labels, pretty cool

# PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# PROB_VALUE = 0.67 # he just picked all of these bc they are prime
PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
weight.seq <- seq(from=round(min(cherry_blossoms_df$log_temp) - 1), to=round(max(cherry_blossoms_df$log_temp) + 1), by=.5)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(cherry_blossom_log_temp_doy, data=data.frame(log_temp=weight.seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.height <- sim(cherry_blossom_log_temp_doy, data=list(log_temp=weight.seq))
# sim.height <- sim(m4H3.1, data=list(log_weight=weight.seq), n=1e4)
# ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
str(sim.height)

# We can summarize these3 simulated heights in the same way 
# we summarized the distributions of m, by using apply:
height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)

# Let's plot everything we've built up:
# 1) The average line,
# 2) The shaded region of 89% plausible mu
# 3) the boundaries of the simulated heights the model expects 

# # plot raw data
# plot(height ~ log_weight, d, col = col.alpha(rangi2,0.5))
# 
# # draw MAP line
# lines(weight.seq, mu.mean)
# 
# # draw HPDI region for line
# mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# shade(mu.HPDI, weight.seq)
# 
# # # draw PI region for line # either do above HPDI or this, not both
# # mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# # shade(mu.PI, weight.seq)
# 
# # draw PI region for simulated heights
# shade(height.PI, weight.seq)

# Overthinking - changing to original scale for above graph instead:
# First, turn of the horizontal axis when plotting raw data with xaxt = 'n'
plot(doy ~ log_temp, cherry_blossoms_df, col=col.alpha(rangi2,0.5), xaxt='n', xlab = 'temp')
# he didn't, but can also add all the extra lines/shades
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)
# 
# Then, explicitly construct the axis, using the axis function
at = c(1, 1.5, 2, 2.5, 3)
labels = exp(at) # convert standardized units back to original scale
axis(side = 1, at=at, labels = round(labels, 1))

# End incorrect (but interesting) B version

# So this doesn't seem terribly accurate... 
# .. I'm leaning towards splines over polynomials to try next 



##### Splines #####

# loading data
data(cherry_blossoms)
d = cherry_blossoms
precis(d)

# nrow(d)
# d = subset(d, subset = !is.na(d$doy))
# nrow(d)
# d = subset(d, subset = !is.na(d$temp))
# nrow(d)

# plot doy of cherry blossom vs temp 
plot(doy ~ temp, data = d)

# First, we choose the knots, both amount and where they go

# creating knots (here we create 15 and at evenly spaced quantiles of predictor variables) 
d = d[complete.cases(d$doy), ]
d = d[complete.cases(d$temp), ]

d = d[order(d$temp),] # so, I can do this too instead of arrange from tidyverse below

##### THIS LINE BELOW WAS THE KEY -- specifically, arrange(temp) #####

# d <- d[complete.cases(d[,c('doy', 'temp')]), ] %>%
#   arrange(temp)

##### END THIS LINE BELOW WAS THE KEY -- specifically, arrange(temp) #####

num_knots = 30
knot_list = quantile(d$temp, probs=seq(0,1,length.out=num_knots))

precis(knot_list)

# The next choice is polynomial degree 

# Here we  contruct necessary basis function for a degree 3 (cubic) spline
library(splines)
B <- bs(d$temp,
        knots=knot_list[-c(1,num_knots)],
        degree=3, intercept = TRUE)

nrow(B)
ncol(B)
# Each row is a temp, corresponding to the rows in the d data frame.
# Each column is a basis function, one of our synthetic variables 
# defining a span of temps within which a corresponding parameter will influence prediction. 

# To display the basis functions, just plot each column against temp:
plot(NULL, xlim=range(d$temp), ylim=c(0,1), xlab='temp', ylab='basis')
for (i in 1:ncol(B)) lines(d$temp, B[,i])

# model
# needs matrix multiplication for summation in mu eqution
# as well as a start list for the weights to tell quap how many there are
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    # mu <- a + B %*% w, # matrix multiplication, same as below commented out line, both do the summation in mu equation the same way
    mu <- a + sapply(1:nrow(B), function(i) sum(B[i,]*w)),
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data = list(D = d$doy, B=B), 
  start = list(w=rep(0,ncol(B))))


# could look at posterior means if you want with precis(m4.7, depth =2),
# but it won't reveal much - you should see 17 w parameters but you can't tell what the model thinks from the parameter summaries 

# Instead we need to plot the posterior predictions.

# First, here are the weighted basis functions:
post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d$temp), ylim = c(-6,6),
     xlab='temp', ylab='basis * weight')
for (i in 1:ncol(B)) lines(d$temp, w[i]*B[,i])

# And finally, the 97% posterior interval for mu, at each temp:
mu <- link(m4.7)
mu_PI_3 <- apply(mu,2,PI,0.97)
mu_mean = apply(mu, 2, mean)

plot(d$temp, d$doy, col=col.alpha(rangi2,0.3), pch=16)
shade(mu_PI_3, d$temp, col = col.alpha('black', 0.5))

# lines(d$temp, mu_mean)

# How many knots is correct? We'll answer that question in a few more chapters. 

# # PROB_VALUE = 0.89 # he just picked all of these bc they are prime
# # PROB_VALUE = 0.67 # he just picked all of these bc they are prime
# PROB_VALUE = 0.97 # he just picked all of these bc they are prime
# 
# # Define a sequence of weights to compute predictions for
# # these values will be on the horizontal axis 
# weight.seq <- seq(from=4.5, to=8.5, by=.5)
# 
# #use link to compute mu for each sample from posterior
# # and for each weight in weight.sq
# mu <- link(m4.7)
# # post <- extract.samples(m4.7)
# # mu.link <- function(weight) post$a + post$b()
# str(mu)
# 
# # summarize the distribution of mu
# mu.mean <- apply(mu, 2, mean)
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# 
# # simulated heights that embody the 
# # uncertainty in the posterior as well as the 
# # uncertainty in the Gaussian distribution of heights, using sim tool
# sim.height <- sim(m4.7, data=list(temp=d$temp))
# # sim.height <- sim(m4H3.1, data=list(log_weight=weight.seq), n=1e4)
# # ^ optional n value controls how many samples are used, smooths out edges, but edge smoothness hardly matters except for aesthetics - it's all approximate either way 
# str(sim.height)
# 
# # We can summarize these3 simulated heights in the same way 
# # we summarized the distributions of m, by using apply:
# height.PI <- apply(sim.height, 2, PI, prob=PROB_VALUE)
# 
# # Let's plot everything we've built up:
# # 1) The average line,
# # 2) The shaded region of 89% plausible mu
# # 3) the boundaries of the simulated heights the model expects
# 
# # plot raw data
# plot(doy ~ temp, d, col = col.alpha(rangi2,0.5))
# 
# # draw MAP line
# lines(d$temp, mu.mean)
# 
# # # draw HPDI region for line
# # mu.HPDI = apply(mu, 2, HPDI, prob=PROB_VALUE)
# # shade(mu.HPDI, d$temp)
# 
# # draw PI region for line # either do above HPDI or this, not both
# mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)
# shade(mu.PI, d$temp)
# 
# # draw PI region for simulated heights
# shade(height.PI,d$temp)


# 4H6: Simulate the prior predictive distribution for the 
# cherry blossom spline in the chapter. 
# Adjust the prior on the weights and observe what happens. 
# What do you think the prior on the weights is doing? 


# Prior predictive simulation #
# Simulating heights from the model, using only the priors #
set.seed(42)
N = 10 # 100 lines
a = rnorm(N, 100, 10)
w = rnorm(N*ncol(B), 0, 5)
w = matrix(w, nrow=ncol(B))
plot(NULL, xlim=range(d2$year) , ylim = c(-30,30), xlab = 'year', ylab = 'basis * weigh')
j = 1
while (j <= N)
{
  for (i in 1:ncol(B)) lines(d2$year, w[i, j]* B[,i])
  j = j + 1
}

# The prior on the weights is being multiplied with basis function, 
# and amplifying the magnitude of this product. 
# More constrained priors lead to much smaller range of the 
# curves of the basis * weight curves, which makes sense,
# and also reflects what we saw in problem number 4M8. 


# 4H8: The cherry blossom spline in the chapter used an intercept a, but technically it doesn't require one. 
# The first basis functins could substitute for the intercept. 
# Try refitting the cherry blosom spline without the intercept. 
# What else about the model do you need to change to make this work?  


# Splines #

# loading data
data(cherry_blossoms)
d = cherry_blossoms
precis(d)

# plot doy of cherry blossom vs year 
plot(doy ~ year, data = d)

# First, we choose the knots, both amount and where they go

# creating knots (here we create 15 and at evenly spaced quantiles of predictor variables) 
d2 = d[complete.cases(d$doy), ]

d2$doy_standardized = (d2$doy - mean(d2$doy)) / sd(d2$doy)

num_knots = 15
knot_list = quantile(d2$year, probs=seq(0,1,length.out=num_knots))

precis(knot_list)

# The next choice is polynomial degree 

# Here we  contruct necessary basis function for a degree 3 (cubic) spline
library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)],
        degree=3, intercept = TRUE)

nrow(B)
ncol(B)
# Each row is a year, corresponding to the rows in the d2 data frame.
# Each column is a basis function, one of our synthetic variables 
# defining a span of years within which a corresponding parameter will influence prediction. 

# To display the basis functions, just plot each column against year:
plot(NULL, xlim=range(d2$year), ylim=c(0,1), xlab='year', ylab='basis')
for (i in 1:ncol(B)) lines(d2$year, B[,i])

# model
# needs matrix multiplication for summation in mu eqution
# as well as a start list for the weights to tell quap how many there are
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- B %*% w, # matrix multiplication, same as below commented out line, both do the summation in mu equation the same way
    # mu <- a + sapply(1:nrow(B), function(i) sum(B[i,]*w)),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data = list(D = d2$doy_standardized, B=B), 
  start = list(w=rep(0,ncol(B))))


# could look at posterior means if you want with precis(m4.7, depth =2),
# but it won't reveal much - you should see 17 w parameters but you can't tell what the model thinks from the parameter summaries 

# Instead we need to plot the posterior predictions.

# First, here are the weighted basis functions:
post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d2$year), ylim = c(-6,6),
     xlab='year', ylab='basis * weight')
for (i in 1:ncol(B)) lines(d2$year, w[i]*B[,i])

# And finally, the 97% posterior interval for mu, at each year:
mu <- link(m4.7)
mu_PI <- apply(mu,2,PI,0.97)
plot(d2$year, d2$doy_standardized, col=col.alpha(rangi2,0.3), pch=16)
shade(mu_PI, d2$year, col = col.alpha('black', 0.5))

# How many knots is correct? We'll answer that question in a few more chapters. 


# 4H8 explanation: you would need to standardize days of year and use that instead of doy

# Or, you could change w's prior to what a's prior was (and you would also need to change the exponential prior for sigma to remove error, so from 1 to 10. Then it works. See below for proof.)

## Splines ##

# loading data
data(cherry_blossoms)
d = cherry_blossoms
precis(d)

# plot doy of cherry blossom vs year 
plot(doy ~ year, data = d)

# First, we choose the knots, both amount and where they go

# creating knots (here we create 15 and at evenly spaced quantiles of predictor variables) 
d2 = d[complete.cases(d$doy), ]
num_knots = 15
knot_list = quantile(d2$year, probs=seq(0,1,length.out=num_knots))

precis(knot_list)

# The next choice is polynomial degree 

# Here we  contruct necessary basis function for a degree 3 (cubic) spline
library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)],
        degree=3, intercept = TRUE)

nrow(B)
ncol(B)
# Each row is a year, corresponding to the rows in the d2 data frame.
# Each column is a basis function, one of our synthetic variables 
# defining a span of years within which a corresponding parameter will influence prediction. 

# To display the basis functions, just plot each column against year:
plot(NULL, xlim=range(d2$year), ylim=c(0,1), xlab='year', ylab='basis')
for (i in 1:ncol(B)) lines(d2$year, B[,i])

# model
# needs matrix multiplication for summation in mu eqution
# as well as a start list for the weights to tell quap how many there are
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- B %*% w, # matrix multiplication, same as below commented out line, both do the summation in mu equation the same way
    # mu <- a + sapply(1:nrow(B), function(i) sum(B[i,]*w)),
    w ~ dnorm(100,10),
    sigma ~ dexp(10)
  ), data = list(D = d2$doy, B=B), 
  start = list(w=rep(0,ncol(B))))


# could look at posterior means if you want with precis(m4.7, depth =2),
# but it won't reveal much - you should see 17 w parameters but you can't tell what the model thinks from the parameter summaries 

# Instead we need to plot the posterior predictions.

# First, here are the weighted basis functions:
post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d2$year), ylim = c(-6,6),
     xlab='year', ylab='basis * weight')
for (i in 1:ncol(B)) lines(d2$year, w[i]*B[,i])

# And finally, the 97% posterior interval for mu, at each year:
mu <- link(m4.7)
mu_PI <- apply(mu,2,PI,0.97)
plot(d2$year, d2$doy, col=col.alpha(rangi2,0.3), pch=16)
shade(mu_PI, d2$year, col = col.alpha('black', 0.5))

# How many knots is correct? We'll answer that question in a few more chapters. 



