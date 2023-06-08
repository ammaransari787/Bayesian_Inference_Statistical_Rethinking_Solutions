# Loading required libraries:
library(rethinking)
library(glue)

##### Practice Problems #####

# Question 2M1:

# Recall the globe tossing model form the cahpter. 
# Compute and plot the grid approximate posterior distribution 
# for each of the following sets of observations. 
# In each case, assume a uniform prior for p.
# 1) W, W, W
# 2) W, W, W, L
# 3) L, W, W, L, W, W, W

# So only thing that is different from example (and 1/2/3) is likelihood

# Define number of points to use (5 vs 20 in example)
NUM_POINTS = 20 

# define grid
p_grid = seq(from = 0, to = 1, length.out = NUM_POINTS)

# define prior
prior = rep(1, NUM_POINTS) # flat prior 

# different priors
# prior = ifelse(p_grid < 0.5, 0, 1) # step prior, 2nd on page 38 figure 2.6
# prior = exp(-5*abs(p_grid - 0.5)) # peaked prior, 3rd on page 38 figure 2.6

# compute likelihood at each value in grid 
# likelihood = dbinom(3, size = 3, prob = p_grid) # For number 1
# likelihood = dbinom(3, size = 4, prob = p_grid) # For number 2
likelihood = dbinom(5, size = 7, prob = p_grid) # For number 3

# compute product of likelihood and prior
unstd_posterior = likelihood * prior

# standardize the posterior, so it sums to 1
posterior = unstd_posterior / sum(unstd_posterior)

# to plot:
plot(p_grid, posterior, type = 'b',
     xlab = 'probability of water', ylab = 'posterior probability')
mtext(glue("{NUM_POINTS} points"))


##### Question 2M2: ##### 

# Now assume a prior for p that is equal to zero when p < 0.5 
# and is a positive constant when p >= 5. 
# Again compute and plot the grid approximate posterior distribution 
# for each of the sets of observations in the problem just above. 

# That is same as the step prior from the text, copied below 
# prior = ifelse(p_grid < 0.5, 0, 1) # step prior, 2nd on page 38 figure 2.6

# Thus, 

# Define number of points to use (5 vs 20 in example)
NUM_POINTS = 20 

# define grid
p_grid = seq(from = 0, to = 1, length.out = NUM_POINTS)

# define prior
# prior = rep(1, NUM_POINTS) # flat prior 

# different priors
prior = ifelse(p_grid < 0.5, 0, 1) # step prior, 2nd on page 38 figure 2.6
# prior = exp(-5*abs(p_grid - 0.5)) # peaked prior, 3rd on page 38 figure 2.6

# compute likelihood at each value in grid 
# likelihood = dbinom(3, size = 3, prob = p_grid) # For number 1
# likelihood = dbinom(3, size = 4, prob = p_grid) # For number 2
likelihood = dbinom(5, size = 7, prob = p_grid) # For number 3

# compute product of likelihood and prior
unstd_posterior = likelihood * prior

# standardize the posterior, so it sums to 1
posterior = unstd_posterior / sum(unstd_posterior)

# to plot:
plot(p_grid, posterior, type = 'b',
     xlab = 'probability of water', ylab = 'posterior probability')
mtext(glue("{NUM_POINTS} points"))



