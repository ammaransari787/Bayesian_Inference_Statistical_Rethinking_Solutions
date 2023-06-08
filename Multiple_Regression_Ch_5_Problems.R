##### Spurious association #####

# import required libraries
library(rethinking)
library(dagitty)


##### Practice problems #####

# 5M1: Invent your own example of a spurious correlation. 
# An outcome variable should be correlated with both predictor variables. 
# But wehen both predictors are entered in the same model, 
# the correlations between the outcome and one of the predictors should mostly vanish 
# (or at least be greatly reduced.)
life_expectancy_df <- read.csv("../Data/for_rethinking_ch_5_problem_5M1.csv") # data source: world population review

colnames(life_expectancy_df)
colnames(life_expectancy_df)[3] = 'life_expectancy_overall'
colnames(life_expectancy_df)[9] = 'gun_ownership_rate'
colnames(life_expectancy_df)[11] = 'gun_death_rate'

cor(life_expectancy_df$life_expectancy_overall, life_expectancy_df$gun_ownership_rate) # -0.6205446
cor(life_expectancy_df$life_expectancy_overall, life_expectancy_df$gun_death_rate) # -0.80405

life_expectancy_df$L = standardize(life_expectancy_df$life_expectancy_overall)
life_expectancy_df$D = standardize(life_expectancy_df$gun_death_rate)
life_expectancy_df$O = standardize(life_expectancy_df$gun_ownership_rate)

cor(life_expectancy_df$L, life_expectancy_df$O) # -0.6205446
cor(life_expectancy_df$L, life_expectancy_df$D) # -0.80405

m5._5M1_bivariate_O <- quap(
  alist(
    L ~ dnorm(mu, sigma),
    mu <- a + bO*O,
    a ~ dnorm(0, 0.2),
    bO ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = life_expectancy_df
)

precis(m5._5M1_bivariate_O)
#        mean   sd  5.5% 94.5%
# a      0.00 0.10 -0.15  0.15
# bO    -0.59 0.11 -0.76 -0.42
# sigma  0.77 0.08  0.65  0.89

m5._5M1_bivariate_D <- quap(
  alist(
    L ~ dnorm(mu, sigma),
    mu <- a + bD*D,
    a ~ dnorm(0, 0.2),
    bD ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = life_expectancy_df
)

precis(m5._5M1_bivariate_D)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.12  0.12
# bD    -0.78 0.08 -0.91 -0.65
# sigma  0.59 0.06  0.49  0.68

m5._5M1_multiple_regression <- quap(
  alist(
    L ~ dnorm(mu, sigma),
    mu <- a + bD*D + bO*O,
    a ~ dnorm(0, 0.2),
    bD ~ dnorm(0, 0.5),
    bO ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = life_expectancy_df
)

precis(m5._5M1_multiple_regression)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.12  0.12
# bD    -0.83 0.14 -1.05 -0.61
# bO     0.06 0.14 -0.16  0.28
# sigma  0.58 0.06  0.49  0.68

plot(coeftab(m5._5M1_bivariate_D, m5._5M1_bivariate_O, m5._5M1_multiple_regression), par=c('bD', 'bO'))


# 5M3: It is sometimes observed that the best predictor of fire risk 
# is the presence of firefighters - States and localities with many firefighters also have more fires. 
# Presumably, firefighters do not cause fires. 
# Nevertheless, this is not a spurious correlation. Instead fires cause firefighters. 
# Consider he same reversal of causal inference in the context of the divorce and marriage data. 
# How might a high divorce rate cause a higher marriage rate? 
# Can you think of a way to evaluate this relationship using multiple regression? 

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables

# hist(d2$doy_standardized) # Z score variable I made last q in Chapter 4 (4H8)
# hist(standardize(d2$doy)) # using standardize from rethinking library
# # Proof they are exactly the same ^ 

d$D = standardize(d$Divorce)
d$M = standardize(d$Marriage)
d$A = standardize(d$MedianAgeMarriage)

# To compute the approximate posterior:
m5.5m3.1 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-3, to = 3.2, length.out=30)
mu <- link(m5.5m3.1, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(M ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m5.5m3.1)
#        mean   sd  5.5% 94.5%
# a      0.00 0.09 -0.14  0.14
# bA    -0.69 0.10 -0.85 -0.54
# sigma  0.68 0.07  0.57  0.79


# You can fit a similar regression for the relationship in the left-hand plot in figure 5.2

m5.5m3.2 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bD*D,
    a ~ dnorm(0, 0.2),
    bD ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m5.5m3.2)
#       mean   sd  5.5% 94.5%
# a     0.00 0.11 -0.17  0.17
# bD    0.35 0.13  0.15  0.55
# sigma 0.91 0.09  0.77  1.05

##### Approximating the posterior of a multiple regression model #####
m5.5m3.3 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bD*D + bA*A,
    a ~ dnorm(0, 0.2),
    bD ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m5.5m3.3)
#        mean   sd  5.5% 94.5%
# a      0.00 0.09 -0.14  0.14
# bD    -0.06 0.12 -0.25  0.13
# bA    -0.73 0.12 -0.92 -0.54
# sigma  0.68 0.07  0.57  0.79

# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m5.5m3.1, m5.5m3.2, m5.5m3.3), par=c('bA', 'bD'))


# 5M4: In the divorce data, States with high numbers of members of the Church of LDS 
# have much lower divorce rates than the regression models expected. 
# Find a list of LDS population by State and use those numbers as a predictor variable, 
# predicting divorce rate using marriage rate, median age at marriage, 
# and percent LDS population (possibly standardized). 
# You may want to consider transformations of the raw percent LDS variable. 

mormon_df = read.csv("../Data/mormon_population.csv")

# view(mormon_df)

colnames(mormon_df)
colnames(mormon_df)[1] = "Location"
colnames(mormon_df)[3] = "percent_mormon"

mormon_df = subset(mormon_df, select = c(Location, percent_mormon))

colnames(d)

view(d)

d = merge(d, mormon_df, on = "Location")

d = data.frame(sapply(d, function(x) gsub('%', "", x)))

d$PM = standardize(as.numeric(d$percent_mormon))

d$percent_mormon = as.numeric(d$percent_mormon)

d$log_percent_mormon = log(as.numeric(d$percent_mormon))

d$log_PM = standardize(d$log_percent_mormon)

d$D = as.numeric(d$D)
d$A = as.numeric(d$A)
d$M = as.numeric(d$M)


# Let's use d$log_PM now, looks better 

plot(d$D ~ d$log_PM, xlim = c(-3, 3), ylim = c(-3,3)) # Yeah, way better 

plot(d$D ~ d$PM, xlim = c(-3, 3), ylim = c(-3,3))


##### Approximating the posterior of a multiple regression model #####
m5.5M4 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A + blog_PM*log_PM,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    blog_PM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m5.5M4)
#          mean   sd  5.5% 94.5%
# a        0.00 0.10 -0.16  0.15
# bM       0.08 0.17 -0.19  0.35
# bA      -0.69 0.17 -0.97 -0.41
# blog_PM -0.29 0.15 -0.53 -0.05
# sigma    0.76 0.08  0.64  0.88


##### Posterior prediction plots #####

# How could we produce a simple posterior predictive check in the divorce example? 
# Let's begin by simulating predictions, average over the posterior:

# call link without specifying new data so it uses original data
mu <- link(m5.5M4)

# summaraize samples across cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# simulate observations
# again no new data, so uses original data
D_sim <- sim(m5.5M4, n=1e4)
D_PI <- apply(D_sim, 2, PI)

# Plot predictions against observed, with a line to show perfect prediction 
# and line segments for the confidence interval of each prediction 
plot(mu_mean ~ d$D, col = rangi2, ylim=range(mu_PI), xlab="Observed divorce", ylab = "Predicted divorce")
abline(a=0, b=1, lty=2)
for (i in 1:nrow(d)) lines(rep(d$D[i],2), mu_PI[,i], col = rangi2)

# the easies way to label a few select points is to use identify 
identify(x=d$D, y=mu_mean, labels = d$Loc)
# After executing the above line of code, 
# R will wait for you to click near a point in the active plot window. 
# It'll then place a label near that point, on the side you choose.
# When you are done labeling points press your right mouse button 
# (or press ESC, on some platforms)


# 5M6:

DMA_dag_5M6 = dagitty('dag {M -> A -> D}')
impliedConditionalIndependencies(DMA_dag_5M6)

# 5H2: Assuming that the DAG for the divorce example is indeed M -> A -> D, 
# fit a new model and use it to estimate 
# the counterfactual effect of halving a State's marriage rate M. 
# Use the counterfactual example from the chapter (starting on page 140) as a template. 

# Running two regressions at the same time, to also estimtate the influence of A on M
data(WaffleDivorce)
d <- list()
d$A = standardize(WaffleDivorce$MedianAgeMarriage)
d$D = standardize(WaffleDivorce$Divorce)
d$M = standardize(WaffleDivorce$Marriage)

m5.5H2 <- quap(
  alist(
    ## M -> A 
    A ~ dnorm(mu_M, sigma_M),
    mu_M <- aM + bMA*M,
    aM ~ dnorm(0, 0.2),
    bMA ~ dnorm(0, 0.5),
    sigma_M ~ dexp(1),
    ## A -> D
    D ~ dnorm(mu_D, sigma_D),
    mu_D <- aD + bAD*A,
    aD ~ dnorm(0,0.2),
    bAD ~ dnorm(0, 0.5),
    sigma_D ~ dexp(1)
  ), data = d
)

precis(m5.5H2)
#          mean   sd  5.5% 94.5%
# aM       0.00 0.09 -0.14  0.14
# bMA     -0.69 0.10 -0.85 -0.54
# sigma_M  0.68 0.07  0.57  0.79
# aD       0.00 0.10 -0.16  0.16
# bAD     -0.57 0.11 -0.74 -0.39
# sigma_D  0.79 0.08  0.66  0.91

# Look at the precis(m5.5H2) summary. You'll see that M and A are strongly negatively associated.
# If we interpret this causally, it indicates that manipulating M reduces A. (increasing marriage rate reduces age at marriage ? )
# Look at the precis(m5.5H2) summary. You'll see that A and D are strongly negatively associated.
# If we interpret this causally, it indicates that manipulating A reduces D (increasing age at marriage reduces divorce rate). 

# The goal is to simulate what would happen, if we manipulate M.
# So, next we define a range of values for M.
M_seq <- seq(from = -2, to = 2, length.out=30)
# this defines a list of 30 imaginary interventions, 
# ranging from 2 standard deviations below and 2 above the mean

# Now we can use sim to simulate observations from model m5.5H2. 
# But this time we'll tell it to simulate both M and D, in that order. 
# Why in that order? 
# Because we have to simulate the influence of A on M before we 
# simulate the joint influence of A and M on D. 
# The vars argument to sim tells it both which observables to simulate 
# and in which order: 

# prep data
sim_dat <- data.frame(M = M_seq)

# simulate A and then D, using M_seq
s <- sim(m5.5H2, data = sim_dat, vars=c('A', "D"))
# That's all there is to it - but do look at the overthinking box on page 144 
# to see the individual steps, so you can perform this kind of
# counterfactual simulation for any model fit with any software

# Now to plot the predictions:
plot(sim_dat$M, colMeans(s$D), ylim=c(-2,2), type = 'l', xlab = 'manipulated M', ylab = "counterfactual D")
shade(apply(s$D, 2, PI), sim_dat$M)
mtext("Total counterfactual effect of M on D")

plot(sim_dat$M, colMeans(s$A), ylim=c(-2,2), type = 'l', xlab = 'manipulated M', ylab = "counterfactual A")
shade(apply(s$A, 2, PI), sim_dat$M)
mtext("Counterfactual effect M -> A")

mean(d$M)
median(d$M)

mean(d$A)

(30-26.1) / 1.24

mean(WaffleDivorce$MedianAgeMarriage)
sd(WaffleDivorce$MedianAgeMarriage)

range(WaffleDivorce$MedianAgeMarriage)

mean(WaffleDivorce$Marriage)
sd(WaffleDivorce$Marriage)

range(WaffleDivorce$Marriage)

# Of course these calculation also permit numerical summaries. 
# For example, the expected causal effect of increasing median age at marriage from 20 to 30 is:
# new data frame, standardized to mean 26.1 and std dev 1.24
# sim2_dat <- data.frame(A=(c(20,30)-26.1)/1.24)
sim2_dat <- data.frame(M=(c(30,15)-20.114)/3.797905)
s2 <- sim(m5.5H2, data=sim2_dat, vars =c("A","D"))
mean(s2$D[,2] - s2$D[,1])
# -1.574405
# This is an effect of over one and one half standard deviations. 
#  Divorce rate would decrease by 1.57 standard deviations.  

mean(s2$A[,2] - s2$A[,1])
# 2.751772
# This is a very large effect of over 2 and one half standard deviations.
# Age of marriage would increase by 2.75 standard deviations. 


# 5H3: Return to the milk energy model, m5.7. 
# Suppose that the true causal relationship among the variables is:
# {M -> N; M -> K; N -> K}
# Now compute the counterfactual effect on K of doubling M.
# You will need to account for both 
# the direct and indirect paths of causation.
# Use the counterfactual example from the chapter 
# (starting on page 140) as a template. 

# Running two regressions at the same time, to also estimtate the influence of A on M
data(milk)
d <- milk

d$K = standardize(d$kcal.per.g)
d$N = standardize(d$neocortex.perc)
d$M = standardize(log(d$mass))

dcc <- d[complete.cases(d$K, d$N, d$M), ]

m5.5H3 <- quap(
  alist(
    ## M -> K <- N
    K ~ dnorm(mu, sigma),
    mu <- a + bN*N + bM*M,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1),
    ## M -> N
    N ~ dnorm(mu_N, sigma_N),
    mu_N <- aN + bMN*M,
    aN ~ dnorm(0,0.2),
    bMN ~ dnorm(0, 0.5),
    sigma_N ~ dexp(1)
  ), data = dcc
)

precis(m5.5H3)
#          mean   sd  5.5% 94.5%
# a        0.00 0.10 -0.16  0.16
# bM      -0.07 0.15 -0.31  0.18
# bA      -0.61 0.15 -0.85 -0.37
# sigma    0.79 0.08  0.66  0.91
# aM       0.00 0.09 -0.14  0.14
# bAM     -0.69 0.10 -0.85 -0.54
# sigma_M  0.68 0.07  0.57  0.79

# Real:
#          mean   sd  5.5% 94.5%
# a        0.07 0.13 -0.15  0.28
# bN       0.68 0.25  0.28  1.07
# bM      -0.70 0.22 -1.06 -0.35
# sigma    0.74 0.13  0.53  0.95
# aN      -0.01 0.12 -0.21  0.18
# bMN      0.61 0.13  0.40  0.83
# sigma_N  0.63 0.11  0.46  0.80

# Look at the precis(m5.5H3) summary. You'll see that N and M are strongly positively associated.
# If we interpret this causally, it indicates that manipulating M increases N. 

# The goal is to simulate what would happen, if we manipulate M.
# So, next we define a range of values for M.
M_seq <- seq(from = -2, to = 2, length.out=30)
# this defines a list of 30 imaginary interventions, 
# ranging from 2 standard deviations below and 2 above the mean

# Now we can use sim to simulate observations from model m5.5H4. 
# But this time we'll tell it to simulate both N and K, in that order. 
# Why in that order? 
# Because we have to simulate the influence of M on N before we 
# simulate the joint influence of M and N on K. 
# The vars argument to sim tells it both which observables to simulate 
# and in which order: 

# prep data
sim_dat <- data.frame(M = M_seq)

# simulate M and then D, using A_seq
s <- sim(m5.5H3, data = sim_dat, vars=c('N', "K"))
# That's all there is to it - but do look at the overthinking box on page 144 
# to see the individual steps, so you can perform this kind of
# counterfactual simulation for any model fit with any software

# Now to plot the predictions:
plot(sim_dat$M, colMeans(s$K), ylim=c(-2,2), type = 'l', xlab = 'manipulated M', ylab = "counterfactual K")
shade(apply(s$K, 2, PI), sim_dat$M)
mtext("Total counterfactual effect of M on K")

plot(sim_dat$M, colMeans(s$N), ylim=c(-2,2), type = 'l', xlab = 'manipulated M', ylab = "counterfactual N")
shade(apply(s$N, 2, PI), sim_dat$M)
mtext("Counterfactual effect M -> N")

# Of course these calculation also permit numerical summaries. 
# For example, the expected causal effect of doubling mass is:
# new data frame, standardized to mean 26.1 and std dev 1.24

# d$K = standardize(d$kcal.per.g)
# d$N = standardize(d$neocortex.perc)
# d$M = standardize(log(d$mass))

mean(log(d$mass))
sd(log(d$mass))
median(log(d$mass))

range(log(d$mass))


sim2_dat <- data.frame(M=(c(mean(log(d$mass)),mean(log(d$mass)) * 2)-mean(log(d$mass)))/sd(log(d$mass)))
s2 <- sim(m5.5H3, data=sim2_dat, vars =c("N","K"))
mean(s2$K[,2] - s2$K[,1])
# -0.2151489
# You would expect a decrease of about .215 standard deviations overall. 


# 5H4: Here is an open practice problem to engage your imagination. 
# In the divorce data, 
# States in the southern United States have many of the 
# highest divorce rates. Add the South indicator variable to the analysis. 
# First, draw one or more DAGs that represent your ideas for how 
# Southern American culture might influence any of the other three variables 
# (D, M, or A). Then list the testable implications of your DAGs, 
# if there are any, and fit one or more models to evaluate the implications. 
# What do you think the influence of "Southerness" is? 

dag_5H5 = dagitty('dag {S -> A; A -> D; A -> M}')
impliedConditionalIndependencies(dag_5H5)
# D _||_ M | A
# D _||_ S | A
# M _||_ S | A

drawdag(dag_5H5)

# MElist <- equivalentDAGs(dag_5H5)
# drawdag(MElist)


# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables

# hist(d2$doy_standardized) # Z score variable I made last q in Chapter 4 (4H8)
# hist(standardize(d2$doy)) # using standardize from rethinking library
# # Proof they are exactly the same ^ 

d$D = standardize(d$Divorce)
d$M = standardize(d$Marriage)
d$A = standardize(d$MedianAgeMarriage)
d$S = ifelse(d$South==1, 2, 1) # 2 = south, 1 = not south

### Just southerness on divorce 

m5.5H4_js <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m5.5H4_js, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]  -0.19 0.15 -0.43  0.04
# a[2]   0.44 0.22  0.09  0.80
# sigma  0.92 0.09  0.78  1.07

post <- extract.samples(m5.5H4_js)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)
# quap posterior: 10000 samples from m5.5H4_js
# mean   sd  5.5% 94.5%      histogram
# sigma    0.92 0.09  0.77  1.07       ▁▁▂▅▇▃▁▁
# a[1]    -0.20 0.15 -0.44  0.04 ▁▁▁▁▂▅▇▇▅▂▁▁▁▁
# a[2]     0.44 0.22  0.08  0.80     ▁▁▂▇▇▃▁▁▁▁
# diff_ns -0.63 0.27 -1.06 -0.21  ▁▁▁▁▂▅▇▇▃▁▁▁▁

###

### Just Age on divorce 

# To compute the approximate posterior:
m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-3, to = 3.2, length.out=30)
mu <- link(m5.1, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m5.1)
#        mean   sd  5.5% 94.5%
# a      0.00 0.10 -0.16  0.16
# bA    -0.57 0.11 -0.74 -0.39
# sigma  0.79 0.08  0.66  0.91

###


# To compute the approximate posterior:
m5.5H4 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bA*A,
    a[S] ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-3, to = 3.2, length.out=30)
mu <- link(m5.1, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m5.5H4, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]  -0.12 0.12 -0.32  0.07
# a[2]   0.29 0.19 -0.02  0.60
# bA    -0.53 0.11 -0.70 -0.35
# sigma  0.76 0.08  0.64  0.88

post <- extract.samples(m5.5H4)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)

#          mean   sd  5.5% 94.5%    histogram
# bA      -0.53 0.11 -0.70 -0.35    ▁▁▁▃▇▇▂▁▁
# sigma    0.76 0.08  0.64  0.88 ▁▁▁▂▃▇▇▅▂▁▁▁
# a[1]    -0.13 0.12 -0.32  0.07  ▁▁▂▅▇▇▂▁▁▁▁
# a[2]     0.29 0.19 -0.02  0.60    ▁▁▁▅▇▃▁▁▁
# diff_ns -0.42 0.23 -0.78 -0.04   ▁▁▁▃▇▇▃▁▁▁

# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m5.1, m5.5H4_js, m5.5H4))




# Repeating everything with just M instead of D


# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables

# hist(d2$doy_standardized) # Z score variable I made last q in Chapter 4 (4H8)
# hist(standardize(d2$doy)) # using standardize from rethinking library
# # Proof they are exactly the same ^ 

d$D = standardize(d$Marriage)
d$A = standardize(d$MedianAgeMarriage)
d$S = ifelse(d$South==1, 2, 1) # 2 = south, 1 = not south

### Just southerness on divorce 

m5.5H4_js <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m5.5H4_js, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]  -0.19 0.15 -0.43  0.04
# a[2]   0.44 0.22  0.09  0.80
# sigma  0.92 0.09  0.78  1.07

post <- extract.samples(m5.5H4_js)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)
# quap posterior: 10000 samples from m5.5H4_js
# mean   sd  5.5% 94.5%      histogram
# sigma    0.92 0.09  0.77  1.07       ▁▁▂▅▇▃▁▁
# a[1]    -0.20 0.15 -0.44  0.04 ▁▁▁▁▂▅▇▇▅▂▁▁▁▁
# a[2]     0.44 0.22  0.08  0.80     ▁▁▂▇▇▃▁▁▁▁
# diff_ns -0.63 0.27 -1.06 -0.21  ▁▁▁▁▂▅▇▇▃▁▁▁▁

###

### Just Age on divorce 

# To compute the approximate posterior:
m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-3, to = 3.2, length.out=30)
mu <- link(m5.1, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m5.1)
#        mean   sd  5.5% 94.5%
# a      0.00 0.10 -0.16  0.16
# bA    -0.57 0.11 -0.74 -0.39
# sigma  0.79 0.08  0.66  0.91

###


# To compute the approximate posterior:
m5.5H4 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bA*A,
    a[S] ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-3, to = 3.2, length.out=30)
mu <- link(m5.1, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m5.5H4, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]  -0.12 0.12 -0.32  0.07
# a[2]   0.29 0.19 -0.02  0.60
# bA    -0.53 0.11 -0.70 -0.35
# sigma  0.76 0.08  0.64  0.88

post <- extract.samples(m5.5H4)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)

#          mean   sd  5.5% 94.5%    histogram
# bA      -0.53 0.11 -0.70 -0.35    ▁▁▁▃▇▇▂▁▁
# sigma    0.76 0.08  0.64  0.88 ▁▁▁▂▃▇▇▅▂▁▁▁
# a[1]    -0.13 0.12 -0.32  0.07  ▁▁▂▅▇▇▂▁▁▁▁
# a[2]     0.29 0.19 -0.02  0.60    ▁▁▁▅▇▃▁▁▁
# diff_ns -0.42 0.23 -0.78 -0.04   ▁▁▁▃▇▇▃▁▁▁

# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m5.1, m5.5H4_js, m5.5H4))


# So either way, this DAG can't be true, 
# bc of D _||_ S | A not being true! 

# Testing next idea of DAG: 

dag_5H5_2 = dagitty('dag {S -> A; S -> D; A -> D; A -> M}')
drawdag(dag_5H5_2)
impliedConditionalIndependencies(dag_5H5)
# D _||_ M | A
# M _||_ S | A # we already saw this to be true with last one 
# of previous section, (# Repeating everything with just M instead of D),
# so only need to test D _||_ M | A - which we already know is true! 
# From the text and example above - search plot(coeftab(m5.1, m5.2, m5.3), par=c('bA', 'bM')) -

MElist = equivalentDAGs(dag_5H5_2)

drawdag(MElist)

# Just my hypothesis, and possibly mine plus an arrow from D -> A make sense at all. 
# (No arrows to S make any sense, marrigae age / divorce rate do NOT cause a state to be southern.)



















