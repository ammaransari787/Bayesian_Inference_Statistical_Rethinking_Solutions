# Load required libraries:
library(rethinking)
library(dagitty)
library(tidyverse)
library(glue)


##### Practice problems #####

# 9E1:
# Which of the following is a requirement of the simple Metropolis algorithm?
# 3 - the proposal distribution must be symmetric. 


# 9E2:
# Gibbs sampling is more efficient than the metropolis algorithm.
# How does it achieve this extra efficiency? 
# Are there any limitations to the Gibbs sampling strategy?

# Gibbs sampling is not restricted by the requirement in 9E1 of symmetric
# proposal distributions, 
# and thus it can use adaptive proposals where the distribution of proposed parameter values adjusts itself, 
# depending on the parameter values at the moment, 
# using conjugate pairs of prior distributions and likelihoods
# that have analytical solutions for the posterior distribution of an individual parameter.
# The solutions allow Gibbs sampling to make smarter jumps around the joint posterior distribution of all parameters.

# Limitations include the fact that gibbs sampling can get "stuck" 
# in high dimensional cases due to inevitable high correlation 
# which results in a narrow ridge of probabilities (narrow shell in high dimensional cases).
# Gibbs sampling makes many rejected proposals in these instances, and gets 'stuck', slowing to a crawl,
# to the point that extremely complex models with thousands / ten of thousands 
# of parameters are literally impossible to use Gibbs sampling methods for. 

# Also, if you don't want to use conjugate priors (as we'll discuss why not when we go over multilevel models),
# gibbs sampling requires that you use them, so this is another limitation. 


# 9E3: 
# Which sort of parameters can Hamiltonian Monte Carlo not handle? Can you explain why?

# Hamiltonian Monte Carlo cannot handle discrete parameters. 
# It requires continuous parameters,
# because it cannot do the Hamiltonian physics simulation,
# where it takes a random 'velocity' and random direction and glides in the probability distribution,
# as discrete values aren't continuous by definition, 
# and the particle wouldn't be able to stop at any point,
# move to any point, change direction at any point, etc in the discrete case. 


# 9E4:
# Explain the difference between the effective number of samples, 
# n_eff as calculated by Stan, 
# and the actual number of samples.

# Because Markov chains are typically autocorelated, 
# i.e., adjacent samples have some nonzero correlation, so they're not entirely independent, 
# Stan provdes the effective number of samples, 
# which is an estimate of the number of independent samples from the posterior distribution. 
# Bascially, n_eff is the length of a Markov chain with no autocorrelation that would provide 
# the same quality of estimate as your chain.


# 9E5:
# Which value should Rhat approach, 
# when a chain is sampling the posterior distribution correctly? 

# 1.00 from above. 


# 9E6:
# Sketch a good trace plot for a Markov chain, 
# one that is effectively sampling from the posterior distribution. 
# What is good about its shape? 
# Then sketch a trace plot for a malfuctioning Markov chain. 
# What about its shpae indicates malfunction?

# Good one needs stationarity, good mixing, and convergence of all the chains.
# See written notes for sketch. 
# Similar to m9.4 vs m9.5 traceplots.
# Also top half vs bottom half of page 295, figure 9.11


# 9E7: 
# Repeat the problem above, 
# but now for a trace rank plot.

# see written notes.
# Good one has histograms that overlap and 
# stay within the same range.
# Malfunctioning one has thick sections without overlap.

# trankplot(m9.4) # Bad/malfunctioning
# vs 
# trankplot(m9.5) # or trankplot(m9.1) # Both good
# shows this as well.


# 9M1:
# Re-estimate the terrain ruggedness model from the chapter, 
# but now using a uniform prior for the standard deviation, sigma. 
# The uniform prior should be dunif(0,1). 
# Use ulam to estimate the posterior.
# Does the different prior have any detectible influence 
# on the posterior distribution of sigma?
# Why or why not?

# The model from the chapter again:

# One chain to debug - check for errors that only show up with just 1 chain
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dat_slim
)

# Then 4 chains for verification + inference
set.seed(123)
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dat_slim, chains = 4, cores = 4
)

traceplot(m9.1)
trankplot(m9.1)

precis(m9.1, depth = 2)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  3165     1
# a[2]   1.05 0.01  1.03  1.07  3503     1
# b[1]   0.13 0.07  0.02  0.24  2908     1
# b[2]  -0.14 0.05 -0.22 -0.05  2233     1
# sigma  0.11 0.01  0.10  0.12  2976     1


# Now doing it with uniform standard deviation prior 

# One chain to debug - check for errors that only show up with just 1 chain
m9M1_1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dunif(0,1)
  ), data=dat_slim
)

set.seed(123)
# Then 4 chains for verification + inference
m9M1_2 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dunif(0,1)
  ), data=dat_slim, chains = 4, cores = 4
)

traceplot(m9M1_2)

tranplot(m9M1_2)

precis(m9M1_2, depth = 2)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  2520     1
# a[2]   1.05 0.01  1.03  1.07  2996     1
# b[1]   0.13 0.07  0.01  0.25  2521     1
# b[2]  -0.14 0.06 -0.23 -0.05  2746     1
# sigma  0.11 0.01  0.10  0.12  2731     1

# vs what it was bfore with exponential prior:

#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  3165     1
# a[2]   1.05 0.01  1.03  1.07  3503     1
# b[1]   0.13 0.07  0.02  0.24  2908     1
# b[2]  -0.14 0.05 -0.22 -0.05  2233     1
# sigma  0.11 0.01  0.10  0.12  2976     1

# Virtually identical parameter posterior distributions, 
# with the only difference 
# being in the n_eff samples. 
# Why would n_eff be different for a uniform prior for sigma?
# Perhaps when the prior is more vague,
# the Hamiltonian engine isn't gliding in as focused of an area,
# and the leapfrog steps/step size found cannot be as optimal,
# resulting in slightly more autocorrelation in the Markov Chain.
# Thus, it's probably better to have better priors for n_eff/your markov chain. 
# It could also just be due to random seed. 
# Let's try with set.seed the same to double check. 

# Exponential prior set.seed(123)
# mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  2592     1
# a[2]   1.05 0.01  1.03  1.07  3086     1
# b[1]   0.13 0.08  0.01  0.26  2075     1
# b[2]  -0.14 0.05 -0.23 -0.05  2682     1
# sigma  0.11 0.01  0.10  0.12  2334     1

# mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  2832     1
# a[2]   1.05 0.01  1.03  1.07  3162     1
# b[1]   0.13 0.08  0.01  0.25  2392     1
# b[2]  -0.14 0.06 -0.23 -0.05  2416     1
# sigma  0.11 0.01  0.10  0.12  2259     1

# Ok, it's possible that was simply moreso due to random.seed variation,
# as it disappears when they have the same set.seed

# His solution mentioned plotting the priors first, like so:
par(mfrow=c(1,1))
curve( dunif(x,0,1) , from=0 , to=10 ,
       xlab="sigma" , ylab="Density" , ylim=c(0,1), col = 'red')
curve( dexp(x,1) , add=TRUE , col="blue" )

# Then he plotted the posterior distributions of the two models, like so:
sigma_unif <- extract.samples(m9.1, pars="sigma")
sigma_exp <- extract.samples(m9M1_2, pars="sigma")
par(mfrow=c(1,1))
dens(sigma_unif[[1]], col = 'red')
dens(sigma_exp[[1]], add = TRUE, col = 'blue')

# As we can see, they are very similar.
# His explanation mentioned how this is because there is a lot of data,
# relative to the difference in priors. 

# 9M2:
# Modify the terrain ruggedness model again. 
# This time, change the prior for b[cid] to dexp(0.3)
# What does this do to the posterior distribution? 
# Can you explain it? 

# First, let's plot this new prior:
curve( dexp(x,0.3) , from= -10 , to=10 ,
       xlab="sigma" , ylab="Density" , xlim = c(-10,10),ylim=c(0,1), col = 'red')

# And let's add the previous prior to compare: 
# curve( dnorm(x,0.3) , from= -10 , to=10 ,
#        xlab="sigma" , ylab="Density" , xlim = c(-5,5),ylim=c(0,1), col = 'blue')

curve( dnorm(x,0.3) , add=TRUE , col="blue" )

# So the new (red) prior restricts it ot positive values,
# and leaves more probability open to more positive values
# beyond 2/3 the normal one does, up till around 10.

# I predict the posterior parameter will shift to the right. 

# Previously, it was .13 and -.14.
# I don't suspect the -14 will be possible. 
# .13 might increase.
# -.14 might go up all the way to 0.
# let's see:

# One chain to debug - check for errors that only show up with just 1 chain
m9M2_1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dexp(0.3),
    sigma ~ dexp(1)
  ), data=dat_slim
)

# Then 4 chains for verification + inference
set.seed(123)
m9M2_1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dexp(0.3),
    sigma ~ dexp(1)
  ), data=dat_slim, chains = 4, cores = 4
)

traceplot(m9M2_1)
trankplot(m9M2_1)

precis(m9M2_1, depth = 2)
#       mean   sd 5.5% 94.5% n_eff Rhat4
# a[1]  0.89 0.02 0.86  0.91  1554  1.00
# a[2]  1.05 0.01 1.03  1.07  1862  1.00
# b[1]  0.14 0.08 0.03  0.27   907  1.01
# b[2]  0.02 0.02 0.00  0.05  1702  1.00
# sigma 0.11 0.01 0.11  0.12  1240  1.00

# vs what it was before:

#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  2592     1
# a[2]   1.05 0.01  1.03  1.07  3086     1
# b[1]   0.13 0.08  0.01  0.26  2075     1
# b[2]  -0.14 0.05 -0.23 -0.05  2682     1
# sigma  0.11 0.01  0.10  0.12  2334     1

# Nice, supports what I suspected - 
# b[2] is no longer -.14, it's all the way up positive .02,
# and b[1] is a bit stronger to the right. 

# This makes a lot of sense,
# as the new exponential prior gave no probability 
# the the value being less than 0. 

# The effect of the new exp prior (red) restricting b[2] 
# to positive values, as compare to the previous posterior (blue):
test_extract_b_2 = extract.samples(m9M2_1, pars = "b[2]")
prev_extract_b_2 = extract.samples(m9.1, pars = 'b[2]')
par(mfrow=c(1,1))
dens(test_extract_b_2[[1]], col = 'red', xlim = c(-.5, .5))
dens(prev_extract_b_2[[1]], add = TRUE, col = 'blue')

# And b[1] is mostly the same, perhaps a tad to the right ? 
test_extract_b_1 = extract.samples(m9M2_1, pars = "b[1]")
prev_extract_b_2 = extract.samples(m9.1, pars = 'b[1]')
par(mfrow=c(1,1))
dens(test_extract_b_1[[1]], col = 'red', xlim = c(-.5, .5))
dens(prev_extract_b_2[[1]], add = TRUE, col = 'blue')


# 9M3:
# Re-estimate one of the Stan models from the chapter, 
# but at different numbers of warmup iterations. 
# Be sure to use the same number of sampling iterations in each case. 
# Compare the n_eff values. 
# How much warmup is enough? 

# So the OG one is 1000 for iteration and 1000/2 = 500 for warmup (defaults)

# Here's the precis summary with set.seed(123) for the default ^ 
# mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  2592     1
# a[2]   1.05 0.01  1.03  1.07  3086     1
# b[1]   0.13 0.08  0.01  0.26  2075     1
# b[2]  -0.14 0.05 -0.23 -0.05  2682     1
# sigma  0.11 0.01  0.10  0.12  2334     1


# 750 warmup:
set.seed(123)
m9.750 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dat_slim, chains = 4, cores = 4, iter = 1000, warmup = 750
)

precis(m9.750, depth = 2)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  1281     1
# a[2]   1.05 0.01  1.03  1.07  1524     1
# b[1]   0.13 0.07  0.03  0.24  1082     1
# b[2]  -0.14 0.06 -0.23 -0.06  1262     1
# sigma  0.11 0.01  0.10  0.12  1432     1

# Whoah, n_eff dropped wayyyy down compared to the default of 500.

set.seed(123)
m9.250 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dat_slim, chains = 4, cores = 4, iter = 1000, warmup = 250
)

precis(m9.250, depth = 2)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  3849     1
# a[2]   1.05 0.01  1.03  1.07  4942     1
# b[1]   0.13 0.08  0.01  0.26  3377     1
# b[2]  -0.14 0.06 -0.23 -0.05  4554     1
# sigma  0.11 0.01  0.10  0.12  3692     1

# Wow, 250 is way better! 

# vs original w defaults: 
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  2592     1
# a[2]   1.05 0.01  1.03  1.07  3086     1
# b[1]   0.13 0.08  0.01  0.26  2075     1
# b[2]  -0.14 0.05 -0.23 -0.05  2682     1
# sigma  0.11 0.01  0.10  0.12  2334     1

# Testing 100:
set.seed(123)
m9.100 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dat_slim, chains = 4, cores = 4, iter = 1000, warmup = 100
)

precis(m9.100, depth = 2)
# mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  4162     1
# a[2]   1.05 0.01  1.03  1.07  4098     1
# b[1]   0.13 0.08  0.01  0.25  1606     1
# b[2]  -0.14 0.06 -0.23 -0.05  2041     1
# sigma  0.11 0.01  0.10  0.12  2362     1

test_precis_100 = precis(m9.100, depth = 2)

mean(test_precis_100$n_eff)

test_precis_250 = precis(m9.250, depth = 2)

mean(test_precis_250$n_eff)


# Testing 10 
set.seed(123)
m9.10 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data=dat_slim, chains = 4, cores = 4, iter = 1000, warmup = 10
)

precis(m9.10, depth = 2)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a[1]   0.89 0.02  0.86  0.91  1991     1
# a[2]   1.05 0.01  1.03  1.07  2279     1
# b[1]   0.14 0.08  0.01  0.27   262     1
# b[2]  -0.15 0.05 -0.23 -0.06   576     1
# sigma  0.11 0.01  0.10  0.12   277     1


# 1000

1000 / 50

test_values = rep(0, 19)

curr_ = 0
i = 1
while (i <= length(test_values))
{
  curr_ = curr_ + 50
  test_values[i] = curr_
  i = i + 1
}

outcome_eff_means <- rep(0, 19)

i = 1
while(i <= length(test_values))
{
  set.seed(123)
  test_ulam <- ulam(
    alist(
      log_gdp_std ~ dnorm(mu, sigma),
      mu <- a[cid] + b[cid]*(rugged_std - 0.215),
      a[cid] ~ dnorm(1, 0.1),
      b[cid] ~ dnorm(0, 0.3),
      sigma ~ dexp(1)
    ), data=dat_slim, chains = 4, cores = 4, iter = 1000 + test_values[i], warmup = test_values[i]
  )
  
  test_precis__ = precis(test_ulam, depth = 2)
  outcome_eff_means[i] = mean(test_precis__$n_eff)
  print(glue("value: {test_values[i]}, mean n_eff:{outcome_eff_means[i]}"))
  i = i + 1
}

my_df_ = data.frame(cbind(test_values, outcome_eff_means))

plot(my_df_$outcome_eff_means ~ my_df_$test_values, data = my_df_)
# Got a weird error that broke the loop after warmup of 500, 
# but by then it had already started declining.

# Peak was at 250 warmup, interestingly. 

# Saved the plot in the figures folder as "ch_9_mean_n_eff_vs_warmup_iters_plot.png"

# After checking his solution, I believe my results are not using iter correctly -
# I need to do iter = 1000 + w and warmup = 2 to make them all have 1000 post warmup samples.

get_precis_output_w <- function(w) {
  set.seed(123)
  func_ulam <- ulam(
    alist(
      log_gdp_std ~ dnorm(mu, sigma),
      mu <- a[cid] + b[cid]*(rugged_std - 0.215),
      a[cid] ~ dnorm(1, 0.1),
      b[cid] ~ dnorm(0, 0.3),
      sigma ~ dexp(1)
    ), data=dat_slim, chains = 4, cores = 4, iter = 1000 + w, warmup = w
  )
  
  print(precis(func_ulam, depth = 2))
}

get_precis_output_w(10)
#             mean          sd        5.5%       94.5%     n_eff     Rhat4
# a[1]   0.8871757 0.016001427  0.86140200  0.91212109 2023.6730 0.9995815
# a[2]   1.0503294 0.010232890  1.03464725  1.06700055 2288.4267 1.0031807
# b[1]   0.1379839 0.081016578  0.00855996  0.26654775  262.9425 1.0035083
# b[2]  -0.1466260 0.053873291 -0.23126011 -0.05950744  574.6646 1.0044484
# sigma  0.1113491 0.006252939  0.10228600  0.12201400  281.7172 1.0013762

get_precis_output_w(100)
#             mean          sd        5.5%       94.5%    n_eff     Rhat4
# a[1]   0.8866851 0.015767576  0.86174771  0.91259830 4572.602 0.9995233
# a[2]   1.0506125 0.010214480  1.03428000  1.06702110 4528.242 1.0002026
# b[1]   0.1320299 0.075384689  0.01461324  0.24932329 1748.836 0.9999985
# b[2]  -0.1433977 0.055623680 -0.23290517 -0.05451845 2370.220 1.0002097
# sigma  0.1116287 0.006024662  0.10250539  0.12131616 2608.825 0.9998356

get_precis_output_w(250)
#             mean          sd        5.5%       94.5%    n_eff     Rhat4
# a[1]   0.8867894 0.016072303  0.86101551  0.91249958 5348.302 1.0000061
# a[2]   1.0504845 0.009882469  1.03481890  1.06586055 6427.688 0.9994197
# b[1]   0.1328791 0.075218623  0.01228197  0.25688917 4839.314 0.9998118
# b[2]  -0.1415999 0.054667571 -0.22829330 -0.05336529 6243.576 0.9997178
# sigma  0.1113546 0.006065999  0.10204194  0.12117897 4419.010 1.0008512

get_precis_output_w(500)
#             mean         sd        5.5%       94.5%    n_eff     Rhat4
# a[1]   0.8867069 0.01647579  0.86060380  0.91366428 5204.762 0.9996825
# a[2]   1.0505662 0.01001391  1.03447945  1.06650055 6832.306 0.9993997
# b[1]   0.1340615 0.07686967  0.01374244  0.25617339 4436.061 1.0003004
# b[2]  -0.1420913 0.05574916 -0.23264014 -0.05156564 5098.901 0.9995652
# sigma  0.1115987 0.00616685  0.10197545  0.12164017 4744.482 0.9998648

get_precis_output_w(750)
#             mean          sd        5.5%       94.5%    n_eff     Rhat4
# a[1]   0.8866647 0.016068995  0.86172734  0.91273688 5576.578 0.9991181
# a[2]   1.0503781 0.010082289  1.03408000  1.06646055 6364.634 0.9997876
# b[1]   0.1330099 0.073739210  0.01501823  0.25088843 4548.644 0.9995251
# b[2]  -0.1428027 0.053847585 -0.23062138 -0.05705142 4866.543 0.9993779
# sigma  0.1116012 0.006288045  0.10200083  0.12196811 4800.538 0.9996656

# Once again, it looks like 250 is enough! 


# 9H1:
# Run the model below and then inspect the posterior distribution 
# and explain what it is accomplishing:

mp <- ulam(
  alist(
    a ~ dnorm(0,1),
    b ~ dcauchy(0, 1)
  ), data = list(y=1), chains = 1
)

# Compare the samples for the parameters a and b. 
# Can you explain the different trace plots? 
# If you are unfamilar with the cauchy distribution, 
# you should look it up.
# The key feature to attend to is that it has no expected value. 
# Can you connect this fact to the trace plot? 

precis(mp)
#    mean   sd  5.5% 94.5% n_eff Rhat4
# a -0.08 1.03 -1.76  1.76    97  1.00
# b  0.29 5.20 -4.21  6.43   268  1.01

traceplot(mp)

par(mfrow=c(1,1))
a_dens <- extract.samples(mp, pars = 'a')
b_dens <- extract.samples(mp, pars = 'b')
dens(b_dens[[1]], col = 'red', xlim = c(-40, 40))
dens(a_dens[[1]], add = TRUE, col = 'blue')

# B traceplot has no stationarity. It cannot, because there isn't 
# really a sure high probability area in the posterior probability distribution -
# there is no mean value expected in the Cauchy distribution, 
# as seen in the simulations below. The extremely fat tails allow for 
# any random draw to overwhelm previous draws. 
# See below simulations for proof.

curve( dcauchy(x,0,1) , from=-100 , to=100 ,
       xlab="sigma" , ylab="Density" , ylim=c(0,1), col = 'red')
curve( dnorm(x,0,1) , add=TRUE , col="blue" )
curve( dstudent(x,1,0,1) , add=TRUE , col="green" )

# dcauchy just looks like the most fat tailed student t 
# with a v of 1 - is that it? 

# Cursory google search confirms that is the same thing.

set.seed(123)
y <- rcauchy(1e4, 0, 5)
mu <- sapply(1:length(y), function(i) sum(y[1:i])/i)
plot(mu, type = 'l')

set.seed(1)
y <- rcauchy(1e4, 0, 5)
mu <- sapply(1:length(y), function(i) sum(y[1:i])/i)
plot(mu, type = 'l')

set.seed(2)
y <- rcauchy(1e4, 0, 5)
mu <- sapply(1:length(y), function(i) sum(y[1:i])/i)
plot(mu, type = 'l')

# Same exact thing, using quotient of 2 random Gaussian draws:
set.seed(1)
y <- rep(0, 1e4)
for (i in 1:1e4)
{
  y[i] = rnorm(1, 0, 5) / rnorm(1, 0, 5)
}
mu <- sapply(1:length(y), function(i) sum(y[1:i])/i)
plot(mu, type = 'l')

# Now student t with v = 1
set.seed(5)
y <- rstudent(1e4, 1, 0, 5)
mu <- sapply(1:length(y), function(i) sum(y[1:i])/i)
plot(mu, type = 'l')

# Seems like it is same thing 


# 9H2:
# Recall the divorce rate example from Chapter 5. 
# Repeat that analysis, using ulam this time, 
# fitting models m5.1, m5.2, and m5.3. 
# Use compare to compare the models on the basis of WAIC or PSIS.
# TO use WAIC or PSIS with ulam, 
# you need to add the argument log_log=TRUE.
# Explain the model comparison results. 

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$Divorce)
d$M = standardize(d$Marriage)
d$A = standardize(d$MedianAgeMarriage)

# To compute the approximate posterior:

# For ulam - only have a list with the variables of interset! 

dlist_1 <-list(
  A = d$A,
  M = d$M,
  D = d$D
)

# First with one chain to see bugs
m5.1 <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dlist_1
)

# No errors! 

# So now, with 4 chains 
m5.1 <- ulam(
  alist(
    # D ~ dnorm(mu, sigma),
    D ~ dstudent(2, mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    # sigma ~ dexp(1)
    sigma ~ dunif(0,3) # eliminates warnings 
  ), data = dlist_1, chains = 4, cores = 4, log_lik = TRUE
)

traceplot(m5.1) # Looks good 
trankplot(m5.1) # Looks good 

precis(m5.1) # everything looks good 
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a      0.00 0.10 -0.16  0.16  1732     1
# bA    -0.57 0.11 -0.75 -0.38  1828     1
# sigma  0.83 0.09  0.70  0.97  1570     1

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
par(mfrow=c(1,1))
set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. 
# The procedure is exactly like the examples from the previous chapter: 
# link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-3, to = 3.2, length.out=30)
mu <- link(m5.1, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = dlist_1, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m5.1)
#        mean   sd  5.5% 94.5%
# a      0.00 0.10 -0.16  0.16
# bA    -0.57 0.11 -0.74 -0.39
# sigma  0.79 0.08  0.66  0.91

# before (above) vs ulam/stan (below)

#        mean   sd  5.5% 94.5% n_eff Rhat4
# a      0.00 0.10 -0.16  0.16  1732     1
# bA    -0.57 0.11 -0.75 -0.38  1828     1
# sigma  0.83 0.09  0.70  0.97  1570     1

# Very very similar results, slight slight difference in sigma 

# You can fit a similar regression for the relationship in the left-hand plot in figure 5.2

# First with one chain to check for bugs:
m5.2 <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dlist_1
)

# No bugs, so on to 4 chains for verification / inference:
m5.2 <- ulam(
  alist(
    # D ~ dnorm(mu, sigma),
    D ~ dstudent(2, mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    # sigma ~ dexp(1)
    sigma ~ dunif(0,3) # eliminates warnings 
  ), data = dlist_1, chains = 4, cores = 4, log_lik = TRUE
)

traceplot(m5.2) # looks good 
trankplot(m5.2) # looks good 

precis(m5.2)
#       mean   sd  5.5% 94.5%
# a     0.00 0.11 -0.17  0.17
# bM    0.35 0.13  0.15  0.55
# sigma 0.91 0.09  0.77  1.05

# before (above) vs ulam/stan (below)

#       mean   sd  5.5% 94.5% n_eff Rhat4
# a     0.00 0.11 -0.17  0.18  1853     1
# bM    0.34 0.13  0.13  0.55  1742     1
# sigma 0.94 0.09  0.81  1.10  1678     1

# Again, very very similar.

# As you can see in figure 5.2, (and my precis above), 
# this relationship isn't as strong as the previous one.

# But merely comparing parameter means between different bivariate regressions is no way to decide which predictor is better. 
# Both of these predictors could provide independent value, or they could be redundant, or one could eliminate the value of the other. 

# Approximating the posterior of a multiple regression model

# First one chain to check for bugs:
m5.3 <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = dlist_1
)

# sigma ~ dunif(0,3) # error is caused by dexp(1) vs dunif(0,3) - 
# highly constrained. 
# But didn't occur second time - so sporadic, and for highly contrained -
# prob fine 
par(mfrow=c(1,1))
curve( dunif(x,0,3) , from=0 , to=10 ,
       xlab="sigma" , ylab="Density" , ylim=c(0,1), col = 'red')
curve( dexp(x,1) , add=TRUE , col="blue" )

sigma_prior <- extract.prior(m5.3, pars = 'sigma')
dens(sigma_prior[[4]], col = 'black', xlim = c(-4, 4))

# prior looks as it should too. ok cool.

# No errors, so onto 4 chains for verification + inference:
set.seed(1234) # had to set seed to this to get rid of warning...
m5.3 <- ulam(
  alist(
    # D ~ dnorm(mu, sigma),
    D ~ dstudent(2, mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    # sigma ~ dexp(1)
    sigma ~ dunif(0, 3) # had more n_eff and perfect rhat, (and no warnings ever) but basically same results
  ), data = dlist_1, chains = 4, cores = 4, log_lik = TRUE
)

traceplot(m5.3)
trankplot(m5.3)

precis(m5.3)
#        mean   sd  5.5% 94.5%
# a      0.00 0.10 -0.16  0.16
# bM    -0.07 0.15 -0.31  0.18
# bA    -0.61 0.15 -0.85 -0.37
# sigma  0.79 0.08  0.66  0.91

# ulam with sigma ~ dexp(1)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a      0.00 0.10 -0.16  0.16  1629  1.00
# bM    -0.06 0.16 -0.31  0.18  1039  1.01
# bA    -0.61 0.16 -0.86 -0.35  1092  1.01
# sigma  0.83 0.09  0.70  0.98  1129  1.00

# ulam with sigma ~ dunif(0,3)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a      0.00 0.10 -0.17  0.16  1634     1
# bM    -0.06 0.16 -0.30  0.20  1209     1
# bA    -0.60 0.16 -0.85 -0.35  1229     1
# sigma  0.83 0.09  0.70  0.99  1256     1

# Both ulam's are v similar, don't think warning is problem
# Bc it says it isn't if it's sporadic, which it is. 
# See this mc-stan discourse thread for reference as well: 
# https://discourse.mc-stan.org/t/scale-parameter-is-0-but-must-be-0-can-i-do-anything-to-deal-with-this/19453/4
# They also mentioned that it could be bc of exponential distribution,
# that didn't seem to be a problem,
# and that changing the seed could solve it.

# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m5.1, m5.2, m5.3), par=c('bA', 'bM'))
# Looks exactly as it did in chapter 5.

coeftab(m5.1, m5.2, m5.3)

compare(m5.1, m5.2, m5.3,func = PSIS)
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
#       PSIS    SE dPSIS  dSE pPSIS weight
# m5.1 126.0 12.85   0.0   NA   3.8   0.71
# m5.3 127.9 12.98   1.8 0.72   4.9   0.29
# m5.2 139.7 10.05  13.7 9.35   3.2   0.00

plot(compare(m5.1, m5.2, m5.3,func = PSIS))

plot(PSIS(m5.1, pointwise = TRUE)$k, ylim = c(0, 0.7)) #  max value 0.5658975119
plot(PSIS(m5.2, pointwise = TRUE)$k, ylim = c(0, 0.7))
plot(PSIS(m5.3, pointwise = TRUE)$k, ylim = c(0, 0.7))

# Some values are decently high, but none above .6 so nothing crazy. 
# Perhaps candiaates for robust regression however.

# Difference between m5.1 and m5.3

# 99% credibility interval of difference:
1.8 + c(-1,1)*0.72*2.6 # -0.072  3.672

# 97% credibility interval of difference:
1.8 + c(-1,1)*0.72*1.881 # 0.44568 3.15432

# 89% credibility interval of difference:
1.8 + c(-1,1)*0.72*1.227 # 0.91656 2.68344

# You could say m5.1 is better at predicting than m5.3, 
# but not at the 99% probability level. 
# But you could at the 97% probability level.


# Difference between m5.1 and m5.2:

# 99% credibility interval of difference:
13.7 + c(-1, 1)*9.35*2.6 # = -10.61  38.01

# 97% credibility interval of difference:
13.7 + c(-1, 1)*9.35*1.881 # -3.88735 31.28735

# 89% credibility interval of difference:
13.7 + c(-1, 1)*9.35*1.227 # 2.22755 25.17245

# The 89% credibility interval of the difference 
# between m5.1 and m5.2 also doesn't include 0. 

# The comparisons of PSIS values is saying that 
# the model that just includes 
# age at marriage (m5.1) is slightly better at predicting 
# divorce rate than the one that also includes marriage rate (m5.3),
# both of which make better prediction than 
# the one that only uses marriage rate (m5.2).
# This makes sense, 
# since we already knew that once we know age at marriage,
# knowing marriage rate doesn't give us any additional information
# regarding divorce rate, based on our posteriors for the parameters,
# as well as the DAGs we came up with. 

# However, there are decently high pareto K values for 
# both m5.1 and m5.3, which make these PSIS values 
# potentially unreliable. 
# I believe we should redo with a robust regression
# and examine after that.

# After using robust regression (and dunif(0,3) sigma prior to eliminate warnings)
# > compare(m5.1, m5.2, m5.3,func = PSIS)
#       PSIS    SE dPSIS  dSE pPSIS weight
# m5.1 129.4 11.03   0.0   NA   3.5   0.84
# m5.3 132.7 11.33   3.3 1.45   5.9   0.16
# m5.2 149.5 10.42  20.1 8.04   4.7   0.00

plot(compare(m5.1, m5.2, m5.3,func = PSIS))

# Again, m5.1 vs m5.3:

# 99% credibility interval of difference:
3.3 + c(-1, 1)*1.45*2.6 # -0.47  7.07

# 97% credibility interval of difference:
3.3 + c(-1, 1)*1.45*1.881 # 0.57255 6.02745

# 89% credibility interval of difference:
3.3 + c(-1, 1)*1.45*1.227 # 1.52085 5.07915

# Exact same results as before:
# You could say m5.1 is better at predicting than m5.3, 
# but not at the 99% probability level. 
# But you could at the 97% probability level.
# So the pareto k weren't high enough to be big issue.
# (Makes sense since they were less than .7).

# And again m5.1 vs m5.2:

# 99% credibility interval of difference:
20.1 + c(-1, 1)*8.04*2.6 # -0.804 41.004

# 97% credibility interval of difference:
20.1 + c(-1, 1)*8.04*1.881 # 4.97676 35.22324

# 89% credibility interval of difference:
20.1 + c(-1, 1)*8.04*1.227 # 10.23492 29.96508

# Now you can say m5.1 is better than m5.2 at the 97% credibility interval,
# not just at the 89%! 

# So robust regression gives the same results:
# The comparisons of PSIS values is saying that 
# the model that just includes 
# age at marriage (m5.1) is slightly better at predicting 
# divorce rate than the one that also includes marriage rate (m5.3),
# both of which make better predictions than 
# the one that only uses marriage rate (m5.2).
# This makes sense, 
# since we already knew that once we know age at marriage,
# knowing marriage rate doesn't give us any additional information
# regarding divorce rate, based on our posteriors for the parameters,
# as well as the DAGs we came up with. 


# 9H3: 
# Sometimes changing a prior for one parameter has 
# unanticipated effects on other parameters. 
# This is because when a parameter is highly correlated with 
# another parameter in the posterior, 
# the prior influences both parameters. 
# Here's an example to work and think through. 
# Go back to the leg length example in Chapter 6 and 
# use the code there to simulate height and 
# leg lengths for 100 imagined individuals.

N <- 100 # number of individuals 
set.seed(909)
height <- rnorm(N, 10, 2) # sim total height of each
leg_prop <- runif(N, 0.4, 0.5) # leg as proportion of height
leg_left <- leg_prop*height + rnorm(N, 0, 0.02) # sim left leg as proportion + error
leg_right <- leg_prop*height + rnorm(N, 0, 0.02) # sim right leg as proportion + error
d <- data.frame(height, leg_left, leg_right)

# Below is the model you fit before, 
# resulting in a highly correlated posterior for 
# the two beta parameters. 
# This time, fit the model using ulam:
m5.8s <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10,100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ), data = d, chains = 4,  cores = 4, log_lik = TRUE,
  start=list(a=10, bl=0, br=0.1, sigma=1)
)

traceplot(m5.8s)

precis(m5.8s)
#       mean   sd  5.5% 94.5% n_eff Rhat4
# a     0.98 0.30  0.50  1.46  1021  1.00
# bl    0.20 2.61 -3.77  4.51   530  1.01
# br    1.80 2.62 -2.53  5.82   527  1.01
# sigma 0.64 0.05  0.57  0.71  1059  1.00

# Compare the posterior distribution produced by 
# the code above to the posterior distribution produced 
# when you change the prior for br so that it is strictly positive: 

m5.8s2 <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ), data = d, chains = 4, cores = 4, log_lik = TRUE,
  constraints = list(br='lower=0'),
  start = list(a=10, bl=0, br=0.1, sigma=1)
)

# Note the constraints list. 
# What this does is constrain the prior distribution of br so that 
# it has positive probability only above zero. 
# In other words, 
# that prior ensures that the posterior distribution for br will 
# have no probability mass below zero. 
# Compare the two posterior distributions for m5.8s and m5.8s2. 
# What has changed in the posterior distribution of both beta parameters? 
# Can you explain the change induced by the change in prior?

# I suspect that the sd of both will change. I'll plot the two to compare.

traceplot(m5.8s2) # woah looks way worse
trankplot(m5.8s2)

precis(m5.8s2)
#        mean   sd  5.5% 94.5% n_eff Rhat4
# a      0.96 0.29  0.50  1.42   949  1.00
# bl    -0.91 1.81 -4.01  1.59   603  1.01
# br     2.91 1.81  0.40  6.07   602  1.01
# sigma  0.63 0.05  0.57  0.72   920  1.00

# compared to what it was before: 
#       mean   sd  5.5% 94.5% n_eff Rhat4
# a     0.98 0.30  0.50  1.46  1021  1.00
# bl    0.20 2.61 -3.77  4.51   530  1.01
# br    1.80 2.62 -2.53  5.82   527  1.01
# sigma 0.64 0.05  0.57  0.71  1059  1.00

# Posterior distributions of bl (red) and br (blue) in m5.8s
bl_58s <- extract.samples(m5.8s, pars="bl")
br_58s <- extract.samples(m5.8s, pars="br")
par(mfrow=c(1,1))
dens(bl_58s[[1]], col = 'red', xlim = c(-10,10))
dens(br_58s[[1]], add = TRUE, col = 'blue')
mtext('m5.8s')

# Posterior distributions of bl (red) and br (blue) in m5.8s2
bl_58s2 <- extract.samples(m5.8s2, pars="bl")
br_58s2 <- extract.samples(m5.8s2, pars="br")
par(mfrow=c(1,1))
dens(bl_58s2[[1]], col = 'red', xlim = c(-10,10))
dens(br_58s2[[1]], add = TRUE, col = 'blue')
mtext('m5.8s2')

# model 1
# right
chainmode(br_58s[[1]], adj = 0.01) # 1.843197
mean(br_58s[[1]]) # 1.796881
median(br_58s[[1]]) # 1.842085
PI(br_58s[[1]])
#         5%       94% 
#   -2.531840  5.819837
# left
chainmode(bl_58s[[1]], adj = 0.01) # 0.1562942
mean(bl_58s[[1]]) # 0.1989058
median(bl_58s[[1]]) # 0.1474205
PI(bl_58s[[1]])
#          5%       94% 
#   -3.767442  4.509518 

# model 2
# right
chainmode(br_58s2[[1]], adj = 0.01) # 1.160771
mean(br_58s2[[1]]) # 2.906292
median(br_58s2[[1]]) # 2.72196
PI(br_58s2[[1]])
#         5%       94% 
#   0.3965946 6.0686355 
# left
chainmode(bl_58s2[[1]], adj = 0.01) # 0.3224245
mean(bl_58s2[[1]]) # 0.9061485
median(bl_58s2[[1]]) # 0.7295025
PI(bl_58s2[[1]])
#         5%       94% 
#   -4.007396  1.590155 

#        mean   sd  5.5% 94.5% n_eff Rhat4
# a      0.96 0.29  0.50  1.42   949  1.00
# bl    -0.91 1.81 -4.01  1.59   603  1.01
# br     2.91 1.81  0.40  6.07   602  1.01
# sigma  0.63 0.05  0.57  0.72   920  1.00

# compared to what it was before: 
#       mean   sd  5.5% 94.5% n_eff Rhat4
# a     0.98 0.30  0.50  1.46  1021  1.00
# bl    0.20 2.61 -3.77  4.51   530  1.01
# br    1.80 2.62 -2.53  5.82   527  1.01
# sigma 0.64 0.05  0.57  0.71  1059  1.00

# So first of all, the distribution no longer looks normal really - 
# much more skew to opposite direction in both distributions in model 2. 
# What's happening is the distribution for right leg wants to extend into the negative,
# but cannot bc it is constrained to positive directions.
# Thus, you have almost half of the distribution, which was normal previouslym,
# lopped off, which makes it look a bit positively skewed. 
# Bc these two paramters are so multicollinear,
# and values of left depend on values of right,
# the distribution for left does the same, but mirored.
# This way they can adhere to that narrow ridge of pluasible values, 
# and have the sum of their distributions be the same as they were in the initial model,
# as shown below. 

bl_model_2 <- bl_58s2[[1]]
br_model_2 <- br_58s2[[1]]
model_2_legs <- data.frame(cbind(bl_model_2, br_model_2))
plot(bl_model_2 ~ br_model_2,data = model_2_legs)

sum_blbr <- bl_58s2[[1]] + br_58s2[[1]]
dens(sum_blbr, col = rangi2, lwd = 2, xlab = 'sum of bl and br model 2')


# 9H4:
# For the two models fit in the previous problem, 
# use WAIC or PSIS to compare the 
# effective numbers of parameters for each model. 
# You will need to use log_lik=TRUE to instruct ulam to 
# compute the terms that both WAIC and PSIS need. 
# Which model ahs more effective parameters? 
# Why? 


compare(m5.8s, m5.8s2, func = PSIS)
#         PSIS    SE dPSIS  dSE pPSIS weight
# m5.8s2 194.5 11.37   0.0   NA   2.9   0.52
# m5.8s  194.6 11.20   0.1 0.67   2.9   0.48

# 99% credibility interval of difference:
0.1 + c(-1, 1)*0.67*2.6 # -1.642  1.842
# 97% credibility interval of difference:
0.1 + c(-1, 1)*0.67*1.881 # -1.16027  1.36027
# 89% credibility interval of difference:
0.1 + c(-1, 1)*0.67*1.227 # -0.72209  0.92209

plot(compare(m5.8s, m5.8s2, func = PSIS))

# These two models are virtually the exact same
# in terms of predictions.
# This makes sense, as they still are summing up to the same values as before,
# and thus are still conveying the exact same information as before,
# together in the model. 


# 9H5: 
# Modify the Metropolis algorithm code from the chapter to 
# handle the case that the island populations have a 
# different distribution than the island labels. 
# This means the island's number will not be the same as its population. 

population_vector <- c( # each island name is the index, i.e.,
  5, # island 1
  7, # island 2
  4, # island 3
  9, # island 4
  3, # island 5
  10, # island 6
  2, # island 7
  8, # island 8
  6, # island 9
  1 # island 10
)

# order should be: 10, 7, 5, 3, 1, 9, 2, 8, 4, 6

# Note: His solution just used sample:
pop_size <- sample(1:10)
# but otherwise it was exactly the same as mine

# Here's a short piece of code to do this, 
# storing the history of the king's journey in the vector positions:
num_weeks <- 1e5
positions <- rep(0, num_weeks)
current <- 10
for (i in 1:num_weeks) {
  ## record current position
  positions[i] <- current
  ## flip coin to generate proposal 
  proposal <- current + sample(c(-1, 1), size = 1)
  ## now make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  ## move? 
  prob_move <- population_vector[proposal] / population_vector[current] # Just change this line to Pr("proposal island" parameter value) in the posterior / Pr('current island' parameter value) in the posterior (which is proportional to the likelihood (which we have from the data) * the prior we specified)
  current <- ifelse(runif(1) < prob_move, proposal, current)
  # runif(1) is just a random number between 0 and 1 (bc of defaults min = 0 and max = 1)
}

# The king's location across the first 100 weeks of his simulated travels:
plot(1:100, positions[1:100])
# As you move from the left to the right in this plot, 
# the points show the king's location through time. 
# The king travels among islands, 
# or sometimes stays in place for a few weeks. 
# This plot demonstrates the seemingly pointless path 
# the Metropolis algorithm sends the king on. 
# The right-hand plot shows that the path is far from pointless, however.
plot(table(positions))



samples = br_58s[[1]]
# n=10000 = default amount of samples of extract.samples

chainmode(samples, adj = .1)
sum(samples > 5 & samples < 10) / 10000 

sum(samples < 1.8 & samples > 1.75)/ 10000 # 0.004

sum(samples > (chainmode(samples,adj=.1)-.1) & samples < chainmode(samples,adj=.1)+.1 ) / 10000

PI(samples, prob = .04)

PI(samples, prob = .02)

dens(samples)
mean(samples)
median(samples)


ecdf(samples)(1.78)

quantile(samples, 0.484)

HPDI(samples, .04)


# Explanation 9H5:
# prob_move <- proposal / current 
# just change this ^ line to 
# Pr("proposal island" parameter value) in the posterior / Pr('current island' parameter value) in the posterior (which is proportional to the likelihood (which we have from the data) * the prior we specified)


# 9H6:
# Modify the metropolis algorithm code from the chapter to 
# write your own simple MCMC estimator for globe tossing data and model from chapter 2. 

# 0 = water, 1 = land

# 6 water draws and 3 land draws: 
# Flat prior:

starting = rbinom(1, 1, prob = .5)
starting

probability_vector <- c(6/9, 3/9)

num_weeks <- 1e5
positions <- rep(0, num_weeks)
current <- starting
for (i in 1:num_weeks) {
  ## record current position
  positions[i] <- current
  # ## flip coin to generate proposal 
  proposal = rbinom(1, 1, prob = .5)
  # ## move? 
  prob_move <- probability_vector[proposal + 1] / probability_vector[current + 1]
  current <- ifelse(runif(1) < prob_move, proposal, current)
  # runif(1) is just a random number between 0 and 1 (bc of defaults min = 0 and max = 1)
}

# The king's location across the first 100 weeks of his simulated travels:
plot(1:100, positions[1:100])
# As you move from the left to the right in this plot, 
# the points show the king's location through time. 
# The king travels among islands, 
# or sometimes stays in place for a few weeks. 
# This plot demonstrates the seemingly pointless path 
# the Metropolis algorithm sends the king on. 
# The right-hand plot shows that the path is far from pointless, however.
plot(table(positions))


# That works. - NOT
# Mine was wrong bc the proposal are supposed to parameter values of p, the target of inference,
# not oberavations (water/land)
# I was basically just giving two paramter values, .67 and .33, which is WRONG
# We use continuous values, around the current one (symmetric)

# Book manual solution:

# Here's the raw data from the globe tossing example:
# W L W W W L W L W

# That's 6 waters in 9 tosses. 
# The likelihood is binomial, 
# and we'll use a uniform prior, as in Chapter 2. 
# So this is the model:

# w ~ Binomial(n, p) 
# p ~ Uniform(0, 1)

# where w is the observed number of water and n is the number of globe tosses. 
# The parameter p is the target of inference. 
# We need a posterior distribution for it. 
# The prior distribution is the given uniform density from zero to one. 
# To get a working Metropolis algorithm for this model, 
# think of the different values of p as the island indexes and the product of the likelihood and prior as 
# the population sizes. 
# What is tricky here is that there's an infinite number of islands: 
# every continuous value that p can take from zero to one. 
# That's hardly a problem though. 
# We just need a different way of proposing moves, 
# so that we can land on a range of islands. 
# It'll make more sense, 
# once you see the code 

num_samples <- 1e4
p_samples <- rep(NA, num_samples)
p <- 0.5 # initialize chain with p = 0.5
for (i in 1:num_samples) {
  # record current parameter value
  p_samples[i] <- p
  
  # generate a uniform proposal from -.1 to .1
  proposal <- p + runif(1, -0.1, 0.1)
  # Now reflect off boundaries at 0 and 1
  # this is needed to proposals are symmetric
  if (proposal < 0) proposal <- abs(proposal)
  if (proposal > 1) proposal <- 1-(proposal-1)
  
  # compute posterior prob of current and proposal 
  prob_current <- dbinom(6, size = 9, prob = p) * dunif(p, 0, 1)
  prob_proposal <- dbinom(6, size = 9, prob = proposal) * dunif(proposal, 0, 1)
  
  # move? 
  prob_move <- prob_proposal / prob_current
  p <- ifelse(runif(1) < prob_move, proposal, p)
}

# That's really all there is to it. Once the loop finishes, take a look at the trace plot:
plot(p_samples, type = 'l', ylab = 'probability of water')

# This chain is fine, 
# but it is more autocorrelated than a typical Stan chain. 
# That's why it tends to wander around a bit. 
# It is stationary, 
# but it isn't mixing very well. 
# This is common of such a simple Metropolis chain. 
# But it is working. 
# Now look at the posterior distribution implied by these samples. 
# I'll also compare it to the analytically derived posterior for this model:
dens(p_samples, xlab = "probability of water")
curve(dbeta(x, 7, 4), add = TRUE, col = "red")

# Not bad.

# So what would you do if the model had more than one parameter? 
# You just add another proposal for each parameter, 
# accepting or rejecting each proposal independent of the others. 
# Suppose for example we want to do a 
# simple linear regression with a Metropolis chain. 
# Let's simulate some simple Gaussian data and then 
# compute the posterior distribution of the mean and 
# standard deviation using a custom Metropolis algorithm. 
# The model is:

# y_i ~ Normal(mu, sigma)
# mu ~ Normal(0, 10)
# sigma ~ Uniform(0, 10)

# This is the code:

# Simulate some data
# 100 observations with mean 5 and sd 3
y <- rnorm(100, 5, 3)

# now chain to sample from posterior
num_samples <- 1e4
mu_samples <- rep(NA, num_samples)
sigma_samples <- rep(NA,num_samples)
mu <- 0
sigma <- 1
for (i in 1:num_samples) {
  # record current paramter values
  mu_samples[i] <- mu
  sigma_samples[i] <- sigma
  
  # proposal for mu
  mu_prop <- mu + runif(1, -0.1, 0.1)
  
  # compute posterior prob of mu and mu_prop
  # this is done treating sigma like a constant
  # will do calculation on log scale, as we should
  # so log priors get added to log likelihood
  log_prob_current <- sum(dnorm(y, mu, sigma, TRUE)) + dnorm(mu, 0, 10, TRUE) + dunif(sigma, 0, 10, TRUE)
  log_prob_proposal <- sum(dnorm(y, mu_prop, sigma, TRUE)) + dnorm(mu_prop, 0, 10, TRUE) + dunif(sigma, 0, 10, TRUE)
  
  # move?
  prob_move <- exp(log_prob_proposal - log_prob_current)
  mu <- ifelse(runif(1) < prob_move, mu_prop, mu)
  
  # proposal for sigma
  sigma_prop <- sigma + runif(1, -0.1, 0.1)
  # reflect off boundary at zero
  if (sigma_prop < 0) sigma_prop <- abs(sigma_prop)
  
  # compute posterior probabilities
  log_prob_current <- sum(dnorm(y, mu, sigma, TRUE)) + dnorm(mu, 0, 10, TRUE) + dunif(sigma, 0, 10, TRUE)
  log_prob_proposal <- sum(dnorm(y, mu, sigma_prop, TRUE)) + dnorm(mu, 0, 10, TRUE) + dunif(sigma_prop, 0, 10, TRUE)
  
  # move?
  prob_move <- exp(log_prob_proposal - log_prob_current)
  sigma <- ifelse(runif(1) < prob_move, sigma_prop, sigma)
}

# This code is more complex, 
# but it is really the same strategy, 
# just with two internal steps, 
# one for each parameter. 
# You can process mu_samples, and sigma_samples as usual. 
# I'll show the sequential samples plotted together now, 
# so you can see how the chain wanders into the 
# high probability region of the posterior as the chain evolves:
plot(mu_samples, sigma_samples, type = "l")

# We initialized the chain at (0, 1). 
# It then quickly wandered up and to the right, 
# eventually falling into the orbit of the high density region of the posterior. 
# That long trail into the high density region is often called the "burn in" 
# and usually trimmed off before analysis. 
# You don't hvae to do such trimming with Stan chains, 
# because Stan's warm-up phase takes care of a similar task, 
# and Stan only returns post-warmup samples to you. 


##### 9H7: #####
# Can you write your own Hamiltonian Monte Carlo algorithm 
# for the globe tossing data, 
# using the R code in the chapter? 
# You will have to write your own functions for the likelihood and gradient, 
# but you can use the HMC2 function.

# So first, the equation of the model is:

# Here's the raw data from the globe tossing example:
# W L W W W L W L W

# That's 6 waters in 9 tosses. 
# The likelihood is binomial, 
# and we'll use a uniform prior, as in Chapter 2. 
# So this is the model:

# w ~ Binomial(n, p) 
# p ~ Uniform(0, 1)

# where w is the observed number of water and n is the number of globe tosses. 
# The parameter p is the target of inference. 
# We need a posterior distribution for it. 

U_func <- function(pr) 
{
  r = 6
  n = 9
  
  p = pr 
  
  # This loop is no longer needed since I fixed it a better way in HMC2 function.
  # while (p < 0 || p > 1)
  # {
  #   if (p < 0) p <- abs(p)
  #   if (p > 1) p <- 1-(p-1)
  # }
  
  #print(glue("running U_func, p = {p}"))
  
  U = dbinom(r, size = n, prob = p, log = TRUE) + dunif(p, 0, 1, log = TRUE)
  return(-U)
}

# binomial calculation: dbinom(6, size = 9, .5)

(factorial(9) / (factorial(3) * factorial(6))) * .5**6 * .5**3
# 0.1640625

dbinom(6, size = 9, .5)
# 0.1640625

# Log likelihood of binomial:
log((factorial(9) / (factorial(3) * factorial(6))) * .5**6 * .5**3)

dbinom(6, size = 9, .5, log = TRUE)



test_gradient_func_1st_way <- function(pr) 
{
  r = 6 
  n = 9
  
  p = pr 
  
  # This loop is no longer needed since I fixed it a better way in HMC2 function.
  # while (p < 0 || p > 1)
  # {
  #   if (p < 0) p <- abs(p)
  #   if (p > 1) p <- 1-(p-1)
  # }
  
  #print(glue("running test_gradient_func_1st_way, p = {p}"))
  U_grad = (n*p-r)/ ((p-1)*p) # + 0 bc derivative of uniform dist = 0
  return(- U_grad)
}

test_gradient_func_1st_way(6,9,.5)

test_gradient_func_2nd_way <- function(pr)
{
  r = 6 
  n = 9
  
  # p = pr
  
  p = pr 
  
  U_grad = (r/p) - ((n-r) / (1-p)) # + 0 bc derivative of uniform dist = 0
  return(-1* U_grad)
} # These two gradients are the exact same - will just use first one

dunif(.5, 0, 1)

# First, need to slightly modify HMC2 function:

HMC2 <- function(U, grad_U, epsilon, L, current_q) {
  q = current_q
  p = rnorm(length(q), 0, 1) # random flick - p is momentum 
  current_p = p
  # Make a half step for momentum at the bginning
  p = p - epsilon * grad_U(q) / 2
  # initialize bookkeeping - saves trajectory
  qtraj <- matrix(NA, nrow=L+1, ncol = length(q))
  ptraj <- qtraj
  qtraj[1,] <- current_q
  ptraj[1,] <- p
  
  # Then the action comes in a loop over leapfrog steps. 
  # L steps are taken, 
  # using the gradient to compute a 
  # linear approximation of the log-posterior surface at each point.
  
  # Alternate full steps for position and momentum
  for (i in 1:L) {
    # prev_q = q
    q = q + epsilon * p # full step for the position
    
    # if (q < 0) q <- abs(q) # added this here to keep in in 0 to 1 range!
    # if (q > 1) q <- 1-(q-1)
    
    if (q > 1) { # OK, this works better in the rare case it goes outside of -1, 1, and works the same inside that range
      # print(glue("q is {q}, prev_q is {prev_q}, step size is {epsilon}, momentum is {p}"))
      q = 1 - (q - floor(q))
    }
    
    if (q < 0) {
      q = abs(q) - abs(ceiling(q))
    }
    
    # Make a full step for the momentum, except at end of trajectory
    if (i!=L) {
      p = p - epsilon * grad_U(q)
      ptraj[i+1,] <- p
    }
    qtraj[i+1,] <- q
  } 
  
  # Notice how the step size epsilon is added to the position and momentum vectors. 
  # It is in this way that the path is only an approximation, 
  # because it is a series of linear jumps, not an actual smooth curve. 
  # This can have important consequences, 
  # if the log-posterior bends sharply and the simulation jumps over a bend. 
  # All that remains is clean up: 
  # ensure the proposal is symmetric so 
  # the Markov chain is valid and 
  # decide whether to accept or reject the proposal.
  
  # Make a half step for momentum at the end
  p = p - epsilon * grad_U(q) / 2
  ptraj[L+1,] <- p
  
  # Negate momentum at end of trajectory to make the proposal symmetric
  p = -p
  
  # Evaluate potential and kinetic energies at start and end of trajectory
  current_U = U(current_q)
  current_K  = sum(current_p^2) / 2 
  proposed_U = U(q)
  proposed_K = sum(p^2) / 2 
  H0 <- current_U + current_K
  H1 <- proposed_U + proposed_K
  
  # Accept or reject the state at end of trajectory, returning either 
  # the position at the end of the trajectory or the initial position
  accept <- 0
  # print(glue("current_U = {current_U}"))
  # print(glue("proposed_U = {proposed_U}"))
  # print(glue("current_K = {current_K}"))
  # print(glue("proposed_K = {proposed_K}"))
  
  
  rando_ = runif(1)
  # print(glue('rando_ is {rando_}'))
  
  # 
  
  if (rando_ < exp(current_U - proposed_U + current_K - proposed_K)) {
    new_q <- q # accept
    accept <- 1#; print(glue('accepted, {rando_}<{exp(current_U - proposed_U + current_K - proposed_K)}'))
  } else new_q <- current_q#; print('rejected') # reject 
  return(list(q = new_q, traj = qtraj, ptraj = ptraj, accept = accept, dH = H1 - H0))
}


num_samples <- 1e4
# num_samples <- 200
p_samples <- rep(NA, num_samples)
p <- 0.5 # initialize chain with p = 0.5
for (i in 1:num_samples) {
  # record current parameter value
  # print(i)
  p_samples[i] <- p
  
  # print(glue("right before ret_list, p = {p}"))
  
  ret_list = HMC2(U_func, test_gradient_func_1st_way, .03, 11, p)
  
  p <- ret_list$traj[length(ret_list$traj)]
}

# That's really all there is to it. Once the loop finishes, take a look at the trace plot:
plot(p_samples, type = 'l', ylab = 'probability of water')

dens(p_samples, xlab = "probability of water", xlim = c(0,1))
curve(dbeta(x, 7, 4), add = TRUE, col = "red")

# YESSSS IT WORKEDDDDDD FINALLLLYYYYYY !!!!
