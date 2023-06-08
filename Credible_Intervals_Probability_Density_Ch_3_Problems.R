##### Practice Problems ##### 

# 3E1: How much posterior probability lies below p = 0.2?
sum(samples < 0.2) / 1e4 # = 4e-04

# 3E2: How much posterior probability greater than p = 0.8?
sum(samples > 0.8) / 1e4 # = 0.1116

# 3E3: How much posterior probability lies between p = 0.2 and p = 0.8?
sum(samples > 0.2 & samples < 0.8) / 1e4 # = 0.888

# 3E4: 20% of the posterior probability lies below which value of p? 
quantile(samples, 0.2) # 0.5185185

# 3E5: 20% of the posterior probability lies above which value of p? 
quantile(samples, 0.8) # 0.7557558 

# 3E6: Which values of p contain the narrowest interval equal to 66% of the posterior probablity?
HPDI(samples, prob = 0.66) # 0.5085085 0.7737738 

# 3E7: Which values of p contain 66% of the posterior probability,
# assuming equal posterior probability below and above the interval 
PI(samples, prob = 0.66)

# Or, same as above ^ :
quantile(samples, c(0.17, 0.83))

# 3M1: Suppose the globe tossing data had turned out to be 8 water in 15 tosses. 
# Construct the posterior distribution, using grid approximation. 
# Use the same flat prior as before. 

# Define number of points to use (5 vs 20 in example)
NUM_POINTS = 1000 

# define grid
p_grid = seq(from = 0, to = 1, length.out = NUM_POINTS)

# define prior
prior = rep(1, NUM_POINTS) # flat prior 

# compute likelihood at each value in grid 
likelihood = dbinom(8, size = 15, prob = p_grid)

# compute product of likelihood and prior
unstd_posterior = likelihood * prior

# standardize the posterior, so it sums to 1
posterior = unstd_posterior / sum(unstd_posterior)

# to plot:
plot(p_grid, posterior, type = 'b',
     xlab = 'probability of water', ylab = 'posterior probability')
mtext(glue("{NUM_POINTS} points"))

# 3M2: Draw 10,000 samples from the grid approximation from above.
# Then use the samples to calcule the 90% HPDI for p 
set.seed(100)
samples = sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, prob = 0.9) # 0.3343343 0.7217217

# 3M3: Construct a posterior predictive check for this model and data.
# This means simulate the distribution of samples, averaging over the posterior uncertainty in p. 
# What is the probability of observing 8 water in 15 tosses? 
w = rbinom(1e4, size = 15, prob = samples)
simplehist(w)
table(w)
1499 / length(w) # = 0.1499
# or 
1499 / 1e4 # = 0.1499
# or 
sum(w == 8) / length(w) # = 0.1499
# or 
sum(w == 8) / 1e4 # = 0.1499
# or 
length(subset(w, subset = w == 8)) / length(w) # = 0.1499

# 3M4: Using the posterior distribution constructed from the new (8/15) data, 
# now calculate the probability of observing 6 water in 9 tosses. 
w = rbinom(1e4, size = 9, prob = samples)

sum(w == 6) / length(w) # = 0.1714
# or 
sum(w == 6) / 1e4 # = 0.1714

# 3M5: 
# Start over at 3M1, but now use a prior that is zero below p = 0.5, and a constant above p = 0.5.
# This corresponds to prior information that majority of the Earth's surface is water. 
# Repeat each problem above and compare the inferences. 
# What difference does the better prior make? 
# If it helps, compare inferences (using both priors) to the true value p = 0.7.

# Define number of points to use (5 vs 20 in example)
NUM_POINTS = 1000 

# define grid
p_grid = seq(from = 0, to = 1, length.out = NUM_POINTS)

# define prior
# prior = rep(1, NUM_POINTS) # flat prior
prior = ifelse(p_grid < 0.5, 0, 1) # step prior, 2nd on page 38 figure 2.6

# compute likelihood at each value in grid 
likelihood = dbinom(8, size = 15, prob = p_grid)

# compute product of likelihood and prior
unstd_posterior = likelihood * prior

# standardize the posterior, so it sums to 1
posterior = unstd_posterior / sum(unstd_posterior)

# to plot:
plot(p_grid, posterior, type = 'b',
     xlab = 'probability of water', ylab = 'posterior probability')
mtext(glue("{NUM_POINTS} points"))


# 3M2: Draw 10,000 samples from the grid approximation from above.
# Then use the samples to calcule the 90% HPDI for p 
set.seed(100)
samples = sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
# Flat prior from before:
# HPDI(samples, prob = 0.9) # 0.3343343 0.7217217 
# New prior (step prior):
HPDI(samples, prob = 0.9) # 0.5005005 0.7097097 


# 3M3: Construct a posterior predictive check for this model and data.
# This means simulate the distribution of samples, averaging over the posterior uncertainty in p. 
# What is the probability of observing 8 water in 15 tosses? 
w = rbinom(1e4, size = 15, prob = samples)
simplehist(w)
# Flat prior from before: 
# sum(w == 8) / 1e4 # = 0.1499
# New prior (step prior):
sum(w == 8) / 1e4 # = 0.163
# vs true prob 
w = rbinom(1e4, size = 15, prob = 0.7)
sum(w == 8) / 1e4 # = 0.0783
# or 
dbinom(8, size = 15, prob = 0.7) # 0.08113003


# 3M4: Using the posterior distribution constructed from the new (8/15) data, 
# now calculate the probability of observing 6 water in 9 tosses. 
w = rbinom(1e4, size = 9, prob = samples)
simplehist(w)

# Flat prior from before: 
# sum(w == 6) / 1e4 # = 0.1714
# New prior (step prior):
sum(w == 6) / 1e4 # = 0.2353
# vs true prob 
w = rbinom(1e4, size = 9, prob = 0.7)
sum(w == 6) / 1e4 # = 0.2662
# or 
dbinom(6, size = 9, prob = 0.7) # 0.2668279

# Explanation for 3M5:
# The new step prior shifted the HPDI to the left, made it more similar to true values.
# This is evident when looking at histogram of posterior predictive distributions for all three (flat prior, step prior, and true prob of 0.7).
# For 6 out of 9, all probabilities were more accurate along with histograms of posterior predictive distribution when using step prior vs flat prior (accurate meaning closer to values from true prob of 0.7).
# For 8 out of 15, while the probability of observing 8 was lower for the true prob of 0.7, and thus closer to the lower flat prior probability,
# examining histograms of the posterior predictive distribution for all three (flat prior, step prior, and true prob of 0.7) shows the step prior histogram is far closer - for the true prob of 0.7 histogram MAP is to the left of 8, similar to with the step prior, and less like with the flat prior. 
# Overall, the informed step prior appears to improve predictions for these reasons. 


# 3M6: Suppose you want to estimate the Earth's proportion of water very precisely. 
# Specifically, you want the 99% percentile interval of the posterior distribution of p to be only 0.05 wide. 
# This means the distance between the upper and lower bound of the interval should be 0.05. 
# How many times will you have to toss the globe to do this? 

PI_ = PI(samples, prob = 0.99)

PI_[1][1]
PI_[2]

unname(PI_[2]) - unname(PI_[1])


i = 1
w = 0
PI_range = 1
while (PI_range >= .05)
{
  value = rbinom(1, size = 1, prob = 0.7)
  if (value == 1)
  {
    w = w + 1
  }
  p_grid = seq(from = 0, to = 1, length.out = 1000)
  prior = rep(1, 1000) # flat prior 
  likelihood = dbinom(w, size = i, prob = p_grid)
  unstd_posterior = likelihood * prior
  posterior = unstd_posterior / sum(unstd_posterior)
  samples = sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
  PI_ = PI(samples, prob = 0.99)
  PI_range = unname(PI_[2]) - unname(PI_[1])
  print(glue("PI_range is {PI_range}, size is {i}"))
  i = i + 1
}

print(i) # 2095

data(homeworkch3)

# 3H1: Using a grid approximation compute the posterior distribution for the probability of a birth being a boy.  
# Assume uniform prior probability. Which parameter value maximizes the posterior probabilility? 

sum(birth1) # 51 boys in birth 1 
length(birth1) # out of 100 births 

sum(birth2) # 60 boys in birth 1 
length(birth2) # out of 100 births 

# Pr(boy) = .555
# Pr(girl) = 0.445


# Define number of points to use (5 vs 20 in example)
NUM_POINTS = 1000 

# define grid
p_grid = seq(from = 0, to = 1, length.out = NUM_POINTS)

# define prior
prior = rep(1, NUM_POINTS) # flat prior 

# different priors
# prior = ifelse(p_grid < 0.5, 0, 1) # step prior, 2nd on page 38 figure 2.6
# prior = exp(-5*abs(p_grid - 0.5)) # peaked prior, 3rd on page 38 figure 2.6

# compute likelihood at each value in grid 
likelihood = dbinom(111, size = 200, prob = p_grid)

# compute product of likelihood and prior
unstd_posterior = likelihood * prior

# standardize the posterior, so it sums to 1
posterior = unstd_posterior / sum(unstd_posterior)

# to plot:
plot(p_grid, posterior, type = 'b',
     xlab = 'probability of water', ylab = 'posterior probability')
mtext(glue("{NUM_POINTS} points"))

p_grid[which.max(posterior)] # 0.5545546

# 3H2: Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above.  
# Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.

set.seed(100)
samples = sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

# 50% HPDI
HPDI(samples, 0.5) # = 0.5265265 0.5725726 

# 89% HPDI
HPDI(samples, 0.89) # = 0.4994995 0.6076076 

# 97% HPDI
HPDI(samples, 0.97) # = 0.4824825 0.6296296 

# 3H3: Use rbinom to simulate 10,000 replicates of 200 births . 
# You should end up with 10,000 numbers, each one a count of boys out of 200 births.  
# Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births).  
# There are many good ways to visualize the simulations, but the dens command 
# (part of the rethinking package) is probably the easiest way in this case. 
# Does it look like thte model fits the data well? 
# That is, does the distribution of predictions include the actual observation as a central, likely outcome? 

simulations_ = rbinom(1e4, size = 200, prob = samples)
dens(simulations_)

max(table(simulations_)) # 446

table(simulations_) # 111 has 446

# Yes, 111 is actually the maximum value in the frequency table.  

# 3H4: Now compare 10,000 counts of boys from 100 simulated first borns only to the number in the first births , birth1. how does the model look in this light?
simulations_ = rbinom(1e4, size = 100, prob = samples)
dens(simulations_)

max(table(simulations_)) # 681

table(simulations_) # 54 has the max now

quantile(simulations_, c(.2, .8)) # actual value of 51 is in the middle 60

# or, same as ^ above :
PI(simulations_, .6) # actual value of 51 is in the middle 60

quantile(simulations_, .22) # 51

# Performs less accurately in this case, however, actual observed value is still in middle 60% of distribution.

mean(simulations_) # 55.3125
median(simulations_) # 55
# and 54 was max value, shown w table and max(table) above 

# 3H5: The model assumes that sex of first and second births are independent. 
# To check this assumption, focus now on second births that followed female firstborns. 
# Compare 10,000 simulated counts of boys to only those second births that followed girls. 
# To do this correctly, you need to count the number of firstborns who were girls, 
# and simulate that many births, 10,000 times. 
# Compare the counts of boys in your simulations to the actual observed count of boys following girls. 
# How does the model look in this light? Any guesses what is going on in these data? 


# calculating counts of boys following girls: 

boys_following_girls_count = 0

i = 1
while (i <= 100)
{
  if (birth1[i] == 0 && birth2[i] == 1)
  {
    boys_following_girls_count = boys_following_girls_count + 1
  }
  i = i + 1
}

print(boys_following_girls_count) # 39

# Counts of firstborn girls
print(length(subset(birth1, subset = birth1 == 0))) # 49

# or just 100 - 51 (which was birth 1 boys)

# simulating 
simulations_ = rbinom(1e4, size = 49, prob = samples)

dens(simulations_)

max(table(simulations_)) # 681

table(simulations_) # 54 has the max now

quantile(simulations_, c(.01, .99))

quantile(simulations_, .999) # observed value is at the .999 percentile, far in the extreme tail 

# Explanation for 3H5:
# Appears they are not independent. 
# A few things could be happening here.
# First, this data could simply represent an extreme case,
# and this 'small world' is very non-representative of the 'large world' of real life. 
# An alternative slightly informed guess is related to the idea that parents having a second child may be older. 
# Older pregnancies have higher risk of complications,
# and also, it appears a higher proportion of female fetuses are miscarried during all pregnancies than male fetusus,
# according to this article:
# https://www.npr.org/sections/health-shots/2015/03/30/396384911/why-are-more-baby-boys-born-than-girls#:~:text=Scientists%20have%20found%20some%20unexpected,birth%20since%20the%2017th%20century
# - perhaps this is related.


