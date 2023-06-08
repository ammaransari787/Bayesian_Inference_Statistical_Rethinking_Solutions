# import required libraries
library(rethinking)
library(dagitty)


##### Practice Problems #####

# 6E1: List three mechanisms by which multiple regression can 
# produce false inferences about causal effects. 


# 6E2: For one of the mechanisms in the previous problem, 
# provide an example of your choice, perhaps from your own research. 

# Using the model from p 238 of Vol 2 of IO psych handbook 

# turnover_DAG = dagitty('dag {
#                        JSa -> OC 
#                        JSa -> WC 
#                        JSa -> EUoW 
#                        OC -> JSa 
#                        OC -> WC 
#                        OC -> EUoW 
#                        WC -> T 
#                        WC -> EUoW 
#                        EUoW -> JSe 
#                        JSe -> CA 
#                        CA -> T
#                        }')

# From 2001 article: 
turnover_DAG = dagitty("dag {
                       IC -> JSa 
                       IC -> WC 
                       JSa -> WC 
                       JSa -> JA 
                       JSa -> EUoW 
                       JA -> WC
                       WC -> T 
                       WC -> EUoW 
                       UR -> EUoW 
                       UR -> T 
                       EUoW -> JSe 
                       JSe -> CA 
                       CA -> T
                       }")

drawdag(turnover_DAG)

impliedConditionalIndependencies(turnover_DAG)

# adjustmentSets(turnover_DAG, exposure = c("JSa", "OC"), outcome = "T")

adjustmentSets(turnover_DAG, exposure = "JSa", outcome = "T")
# { IC }


# 6M1

dag_6M1 <- dagitty("dag {
                   U [unobserved]
                   V [unobserved]
                   X -> Y
                   U -> X
                   U -> B
                   A -> U
                   A -> C
                   C -> B
                   C -> Y
                   V -> C
                   V -> Y
}")

drawdag(dag_6M1)

impliedConditionalIndependencies(dag_6M1)

adjustmentSets(dag_6M1, exposure = "X", outcome = "Y")
# { A }


# 6M2: 

N <- 1000 

X <- rnorm(N)

Z = rnorm(N, X)

Y = rnorm(N, Z)

cor(X, Z)

cor(Y, Z)

cor(X, Y)

XYZ_6M2_df <- data.frame(cbind(X, Y, Z))

# view(XYZ_6M2_df)


# Now test for multicollinearity like with RBT data: 

m6.6M2 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bX*X + bZ*Z,
    a ~ dnorm(0, 0.2),
    bX ~ dnorm(-0, 1),
    bZ ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = XYZ_6M2_df
)


# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6M2)
mu <- link(m6.6M2, post = prior, data = list(X = c(-2,2), Z= c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(m6.6M2)

plot(coeftab(m6.6M2))

plot(precis(m6.6M2))

# Joing posteirior distribution for X and Z 
post <- extract.samples(m6.6M2)
plot(bX ~ bZ, post, col = col.alpha(rangi2,0.1), pch=16)

# Compute + plot the posterior distribution of their sum:
sum_bxby <- post$bX + post$bZ
dens(sum_bxby, col = rangi2, lwd = 2, xlab='sum of bX and bZ')

PI(sum_bxby, .89)

mean(sum_bxby)

HPDI(sum_bxby, .89)

test_lm = lm(Y ~ Z + X, data = XYZ_6M2_df)
vif(test_lm)

pairs( ~ Y + X + Z, data =XYZ_6M2_df, col = rangi2)

# Start by modeling TI as a function of JS and JB but in two bivariate regressions 

m6.6M2_2 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bX*X,
    a ~ dnorm(0, 0.2),
    bX ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = XYZ_6M2_df
)

m6.6M2_3 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bZ*Z,
    a ~ dnorm(0, 0.2),
    bZ ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = XYZ_6M2_df
)

precis(m6.6M2_2)
#       mean   sd  5.5% 94.5%
# a     0.05 0.06 -0.05  0.15
# bX    0.94 0.06  0.84  1.04
# sigma 1.45 0.05  1.37  1.52

precis(m6.6M2_3)
#       mean   sd  5.5% 94.5%
# a     0.04 0.04 -0.04  0.11
# bZ    0.96 0.03  0.91  1.01
# sigma 1.03 0.03  0.98  1.08

precis(m6.6M2)
#        mean   sd  5.5% 94.5%
# a      0.04 0.04 -0.04  0.11
# bX    -0.06 0.06 -0.16  0.04
# bZ     0.99 0.04  0.92  1.06
# sigma  1.03 0.03  0.98  1.08


plot(precis(m6.6M2_2))

plot(precis(m6.6M2_3))

plot(precis(m6.6M2))

plot(coeftab(m6.6M2, m6.6M2_2, m6.6M2_3))

# They are not both weaker, and . 
# The error terms for both don't increase (one stays same, and one is only changed from .3 to .4). 
# More like a spurious relationship, not multicollinearity.
# Y _||_ X|Z, which makes sense with X -> Z -> Y.
# Despite the fact that cor(X,Y) is 0.7220179

# Let's plot the predictor residuals as well 

##### Predictor residual plots #####

# For X, this code will approximate the posterior 
m6.6M2_4 <- quap(
  alist(
    X ~ dnorm(mu, sigma),
    mu <- a + bZ*Z,
    a ~ dnorm(0, 0.2),
    bZ ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = XYZ_6M2_df
)

# And then we compute the residuals by 
# subtracting the observed marriage rate 
# in each state from the predicted rate, 
# based upon the model above:
mu <- link(m6.6M2_4)
mu_mean <- apply(mu, 2, mean)
mu_resid <- XYZ_6M2_df$X - mu_mean


plot(XYZ_6M2_df$Y ~ mu_resid)

test_df <- cbind(XYZ_6M2_df, mu_resid)

ggplot(aes(x = mu_resid, y = Y), data = test_df) + geom_point() + geom_smooth(method = 'lm')

# So after knowing Z, X gives basically no additional information


# For JS, this code will approximate the posterior 
m6.6M2_4 <- quap(
  alist(
    Z ~ dnorm(mu, sigma),
    mu <- a + bX*X,
    a ~ dnorm(0, 0.2),
    bX ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = XYZ_6M2_df
)

# And then we compute the residuals by 
# subtracting the observed marriage rate 
# in each state from the predicted rate, 
# based upon the model above:
mu <- link(m6.6M2_4)
mu_mean <- apply(mu, 2, mean)
mu_resid <- XYZ_6M2_df$Z - mu_mean


plot(XYZ_6M2_df$Y ~ mu_resid)

test_df <- cbind(XYZ_6M2_df, mu_resid)

ggplot(aes(x = mu_resid, y = Y), data = test_df) + geom_point() + geom_smooth(method = 'lm')


# So after knowing X, Z does give us additional information, (See plot from code directly above)
# while after knowing Z, X gives basically no additional information (plot from block above this one)
# (very similar to Age vs marriage rate example on page 136)

# So, we could say:
# Y _||_ X|Z 

# What's different from the legs example from the text 
# is that both don't share the paraemters when both are included 
# - instead bX is completely dropped, 
# as in a spurious relationship. 
# bZ does not get weaker. 
# Error bars aren't increased much if at all, 
# unlike in the legs example as well (and milk example also). 


# 6M3: 

# top left 
dag_6M3_1 <- dagitty('dag{
                     X -> Y 
                     Z -> X 
                     Z -> Y 
                     A -> Z 
                     A -> Y
                     }')

adjustmentSets(dag_6M3_1, exposure = "X", outcome = "Y")
# { Z }

# top right:

dag_6M3_2 <- dagitty('dag{
                     X -> Y 
                     X -> Z 
                     Z -> Y 
                     A -> Z 
                     A -> Y
                     }')

adjustmentSets(dag_6M3_2, exposure = "X", outcome = "Y")
# {}

# bottom left 

dag_6M3_3 <- dagitty('dag{
                     A -> X
                     A -> Z
                     X -> Z
                     X -> Y
                     Y -> Z
                     }')

adjustmentSets(dag_6M3_3, exposure = "X", outcome = "Y")
# {}

# bottom right 


dag_6M3_4 <- dagitty('dag{
                     A -> X
                     A -> Z
                     X -> Z
                     X -> Y
                     Z -> Y
                     C -> X
                     }')

adjustmentSets(dag_6M3_4, exposure = "X", outcome = "Y")
# { A }
# But i think Z would also work, and proof is below: (C variable is just to confirm the c() is working as I expect)

adjustmentSets(dag_6M3_4, exposure = c("X", "C"), outcome = "Y")

adjustmentSets(dag_6M3_4, exposure = c("X", "A"), outcome = "Y")

adjustmentSets(dag_6M3_4, exposure = c("X", "Z"), outcome = "Y")


# 6H1: Use the Waffle House data, data(WaffleDivorce), 
# to find the total causal influence of number of waffle houses on divorce rate. 
# Justify your model or models with a causal graph. 

my_proposed_DAG = dagitty('dag {S -> A; S -> D; S -> W; A -> D; A -> M}')

impliedConditionalIndependencies(my_proposed_DAG)
# A _||_ W | S
# D _||_ M | A
# D _||_ W | S # just tested below - TRUE !! others are fairly obvious as well, but could test to prove, in the same way I tested this one. Could even just change d$D to the standardize(other variable) to make it super easy to test
# M _||_ S | A
# M _||_ W | S
# M _||_ W | A

adjustmentSets(my_proposed_DAG, exposure = "W", outcome = "D")
# { S }

adjustmentSets(my_proposed_DAG, exposure = "S", outcome = "D")
# {}

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$Divorce)
d$W = standardize(d$WaffleHouses)
d$S = ifelse(d$South==1, 2, 1) # 2 = south, 1 = not south

### Just southerness on divorce 

m6.6H1_js <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_js, depth = 2)


post <- extract.samples(m6.6H1_js)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)

### Just Waffle House on divorce 

# To compute the approximate posterior:
m6.6H1_jw <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bW*W,
    a ~ dnorm(0, 0.2),
    bW ~ dnorm(0, .2), 
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_jw)
mu <- link(m6.6H1_jw, post = prior, data = list(W=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
W_seq <- seq(from =-.5, to = 5.5, length.out=30)
mu <- link(m6.6H1_jw, data = list(W=W_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ W, data = d, col = rangi2)
lines(W_seq, mu.mean, lwd = 2)
shade(mu.PI, W_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_jw)


###


# To compute the approximate posterior:
m6.6H1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bW*W,
    a[S] ~ dnorm(0, 1),
    bW ~ dnorm(0, .2),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1, depth = 2)

post <- extract.samples(m6.6H1)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)


# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m6.6H1_jw, m6.6H1_js, m6.6H1))

##### 6H2 #####

# 6H2: Build a series of models to test the 
# implied conditional independencies of the causal graph you used 
# in the previous problem. 
# If any of the tests fail, 
# how do you think the graph needs to be amended? 
# Does the graph need more or fewer arrows? 
# Feel free to nominate variables that aren't in the data. 

impliedConditionalIndependencies(my_proposed_DAG)
# A _||_ W | S # TRUE, see below 
# D _||_ M | A # TRUE, see below 
# D _||_ W | S # just tested above - TRUE !! others are fairly obvious as well, but could test to prove, in the same way I tested this one. Could even just change d$D to the standardize(other variable) to make it super easy to test
# M _||_ S | A # I would say true - significant overlap in 89% PIs for South vs North.
# M _||_ W | S # TRUE, see below 
# M _||_ W | A # TRUE, see below 

drawdag(my_proposed_DAG)

# While we cannot rule out that M _||_ S | A with the results:
# marriage vs southerness + age: 
# mean   sd  5.5% 94.5%       histogram
# bA      -0.74 0.10 -0.89 -0.58        ▁▁▃▇▅▁▁▁
# sigma    0.68 0.07  0.57  0.78     ▁▁▁▂▅▇▅▂▁▁▁
# a[1]     0.06 0.11 -0.12  0.24      ▁▁▁▅▇▅▂▁▁▁
# a[2]    -0.15 0.18 -0.45  0.13 ▁▁▁▁▂▃▇▇▇▃▂▁▁▁▁
# diff_ns  0.22 0.22 -0.12  0.56       ▁▁▁▂▇▇▃▁▁
# vs 
# just marriage vs southerness:  
#          mean   sd  5.5% 94.5%      histogram
# sigma    0.98 0.10  0.82  1.13      ▁▁▁▃▇▇▂▁▁
# a[1]    -0.05 0.16 -0.30  0.21 ▁▁▁▁▁▃▇▇▇▃▂▁▁▁
# a[2]     0.12 0.25 -0.29  0.52    ▁▁▁▂▅▇▅▂▁▁▁
# diff_ns -0.17 0.30 -0.65  0.31  ▁▁▁▂▃▇▇▅▂▁▁▁▁

# , since the overalap appears moreso after controlling for Age than before without age, 
# I want to also evaluate a DAG with everything the same except also S -> M; 
# As in, 

second_proposed_DAG = dagitty('dag {S -> A; S -> D; S -> W; A -> D; A -> M; S -> M}')

drawdag(second_proposed_DAG)

impliedConditionalIndependencies(second_proposed_DAG)
# A _||_ W | S # TRUE, see below 
# D _||_ M | A, S # TRUE, see below 
# D _||_ W | S # just tested above - TRUE !!
# M _||_ W | S # # M _||_ W | S # TRUE, see below

drawdag(equivalentDAGs(second_proposed_DAG))

drawdag(equivalentDAGs(my_proposed_DAG))


# Testing # A _||_ W | S - TRUE 

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$MedianAgeMarriage)
d$W = standardize(d$WaffleHouses)
d$S = ifelse(d$South==1, 2, 1) # 2 = south, 1 = not south

### Just southerness on age 

m6.6H1_js <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_js, depth = 2)


post <- extract.samples(m6.6H1_js)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)

### Just Waffle House on age 

# To compute the approximate posterior:
m6.6H1_jw <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bW*W,
    a ~ dnorm(0, 0.2),
    bW ~ dnorm(0, .2), 
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_jw)
mu <- link(m6.6H1_jw, post = prior, data = list(W=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
W_seq <- seq(from =-.5, to = 5.5, length.out=30)
mu <- link(m6.6H1_jw, data = list(W=W_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ W, data = d, col = rangi2)
lines(W_seq, mu.mean, lwd = 2)
shade(mu.PI, W_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_jw)


###


# To compute the approximate posterior:
m6.6H1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bW*W,
    a[S] ~ dnorm(0, 1),
    bW ~ dnorm(0, .2),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1, depth = 2)

post <- extract.samples(m6.6H1)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)


# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m6.6H1_jw, m6.6H1_js, m6.6H1))


# Testing M _||_ W | S - TRUE 


# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$Marriage)
d$W = standardize(d$WaffleHouses)
d$S = ifelse(d$South==1, 2, 1) # 2 = south, 1 = not south

### Just southerness on marriage 

m6.6H1_js <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_js, depth = 2)


post <- extract.samples(m6.6H1_js)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)

### Just Waffle House on marriage 

# To compute the approximate posterior:
m6.6H1_jw <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bW*W,
    a ~ dnorm(0, 0.2),
    bW ~ dnorm(0, .2), 
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_jw)
mu <- link(m6.6H1_jw, post = prior, data = list(W=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
W_seq <- seq(from =-.5, to = 5.5, length.out=30)
mu <- link(m6.6H1_jw, data = list(W=W_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ W, data = d, col = rangi2)
lines(W_seq, mu.mean, lwd = 2)
shade(mu.PI, W_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_jw)


###


# To compute the approximate posterior:
m6.6H1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bW*W,
    a[S] ~ dnorm(0, 1),
    bW ~ dnorm(0, .2),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1, depth = 2)

post <- extract.samples(m6.6H1)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)


# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m6.6H1_jw, m6.6H1_js, m6.6H1))


# Testing D _||_ M | A - TRUE 

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$Divorce)
d$M = standardize(d$Marriage)
d$A = standardize(d$MedianAgeMarriage)

### Just age on divorce 

m6.6H1_ja <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, .2),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_ja)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_ja)
mu <- link(m6.6H1_ja, post = prior, data = list(A=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-2.5, to = 3, length.out=30)
mu <- link(m6.6H1_ja, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_ja)


### Just marriage rate on divorce 

# To compute the approximate posterior:
m6.6H1_jm <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 1), 
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_jm)
mu <- link(m6.6H1_jm, post = prior, data = list(M=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
M_seq <- seq(from =-2, to = 3, length.out=30)
mu <- link(m6.6H1_jm, data = list(M=M_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ M, data = d, col = rangi2)
lines(M_seq, mu.mean, lwd = 2)
shade(mu.PI, M_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_jm)


###


# To compute the approximate posterior:
m6.6H1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0, .2),
    bA ~ dnorm(0, 1),
    bM ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1, depth = 2)


# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m6.6H1_jm, m6.6H1_ja, m6.6H1))


# Testing M _||_ W | A - TRUE 

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$Marriage)
d$M = standardize(d$WaffleHouses)
d$A = standardize(d$MedianAgeMarriage)

### Just age on divorce 

m6.6H1_ja <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, .2),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_ja)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_ja)
mu <- link(m6.6H1_ja, post = prior, data = list(A=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-2.5, to = 3, length.out=30)
mu <- link(m6.6H1_ja, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_ja)


### Just marriage rate on divorce 

# To compute the approximate posterior:
m6.6H1_jm <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, .2), 
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_jm)
mu <- link(m6.6H1_jm, post = prior, data = list(M=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
M_seq <- seq(from =-.5, to = 5.5, length.out=30)
mu <- link(m6.6H1_jm, data = list(M=M_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ M, data = d, col = rangi2)
lines(M_seq, mu.mean, lwd = 2)
shade(mu.PI, M_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_jm)


###


# To compute the approximate posterior:
m6.6H1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0, .2),
    bA ~ dnorm(0, 1),
    bM ~ dnorm(0, .2),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1, depth = 2)


# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m6.6H1_jm, m6.6H1_ja, m6.6H1))




# Testing M _||_ S | A - I would say TRUE, in that the results don't clearly eliminate it
# (both northern and southern states have 0 within 1 sd of mean, significant overlap in posterior distributions for north and south)

# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$M = standardize(d$Marriage)
d$S = ifelse(d$South==1, 2, 1) # 2 = south, 1 = not south
d$A = standardize(d$MedianAgeMarriage)

### Just age on marriage 

m6.6H1_ja <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, .2),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_ja)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_ja)
mu <- link(m6.6H1_ja, post = prior, data = list(A=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
A_seq <- seq(from =-2.5, to = 3, length.out=30)
mu <- link(m6.6H1_ja, data = list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(M ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_ja)


### Just south on marriage rate

# To compute the approximate posterior:
# m6.6H1_js <- quap(
#   alist(
#     M ~ dnorm(mu, sigma),
#     mu <- a + bM*M,
#     a ~ dnorm(0, 0.2),
#     bM ~ dnorm(0, 1), 
#     sigma ~ dexp(1)
#   ), data = d
# )


### Just southerness on marriage 

m6.6H1_js <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_js, depth = 2)


post <- extract.samples(m6.6H1_js)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)



# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_js, depth = 2)


###


# To compute the approximate posterior:
m6.6H1 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a[S] + bA*A,
    a[S] ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1, depth = 2)

post <- extract.samples(m6.6H1)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)


# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m6.6H1_js, m6.6H1_ja, m6.6H1))





# Testing # D _||_ M | A, S (for second DAG, to see which is more supported)


# Load data and standardize variables of interest:
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$D = standardize(d$Divorce)
d$M = standardize(d$Marriage)
d$A = standardize(d$MedianAgeMarriage)
d$S = ifelse(d$South==1, 2, 1) # 2 = south, 1 = not south

### Just southerness on divorce 

m6.6H1_js <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H1_js, depth = 2)


post <- extract.samples(m6.6H1_js)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)


### Just marriage rate on divorce 

# To compute the approximate posterior:
m6.6H1_jm <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 1), 
    sigma ~ dexp(1)
  ), data = d
)

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H1_jm)
mu <- link(m6.6H1_jm, post = prior, data = list(M=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


# Now for the posterior predictions. The procedure is exactly like the examples from the previous chapter: link, then summarize with mean and PI, and then plot

# compute percentile interval of mean
M_seq <- seq(from =-2, to = 3, length.out=30)
mu <- link(m6.6H1_jm, data = list(M=M_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ M, data = d, col = rangi2)
lines(M_seq, mu.mean, lwd = 2)
shade(mu.PI, M_seq)

# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_jm)


### Just Marriage and age on divorce 

# To compute the approximate posterior:
m6.6H1_just_m_and_a <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 1), 
    bA ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1_just_m_and_a)


###


# To compute the approximate posterior:
m6.6H1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a[S] + bM*M + bA*A,
    a[S] ~ dnorm(0, 1),
    bM ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)


# If you inspect the precis output, you'll see that posterior for BA is reliable negative, 
# as seen in Figure 5.2
precis(m6.6H1, depth = 2)

post <- extract.samples(m6.6H1)

post$diff_ns <- post$a[,1] - post$a[,2]
precis(post,depth = 2)


# It will help to visualize the posterior distributions for all three models, 
# focusing just on the slope parameters bA and bM:
plot(coeftab(m6.6H1_just_m_and_a, m6.6H1_js, m6.6H1_jm, m6.6H1))


# Thus, the only three DAGs that appear make sense are:
drawdag(my_proposed_DAG)

drawdag(second_proposed_DAG)

# and:
reasonable_equiv_to_1st_proposed_dag = dagitty('dag {
                                                      D -> A
                                                      S -> D
                                                      S -> A
                                                      A -> M
                                                      S -> W
                                                      }')

drawdag(reasonable_equiv_to_1st_proposed_dag)


# 6H3: Use a model to infer the 
# total causal influence of area on weight. 
# Would increasing the area available to 
# each fox make it heavier (healthier)? 
# You might want to standardize the variables. 
# Regardless, use prior predictive simulation to show 
# that your model's prior predictions stay 
# within the possible outcome range. 

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

# what was that technique in the book used to 
# set the prior on slope?
# I remember seeing it in marriage happiness page 
# look up 

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

# Explanation for 6H3: 
# The estimated total causal effect of area on weight is 
# .02, with an sd of .09 and an 89% PI of -.13 to .17.
# Increasing the area is highly unlikely to make it healthier, 
# as this slope parameter is extremely close to 0.


# 6H4: 
# Now infer the causal impact of adding food to a territory. 
# Would this make foxes heavier? 
# Which covariates do you need to adjust for to 
# estimate the total causal influence of food? 

adjustmentSets(provided_fox_DAG, exposure= 'avgfood', outcome="weight")
# {}
# No covariates need to be adjusted for 


# load data
data(foxes)
d <- foxes

# standardize variables 
d$Ar = standardize(d$area)
d$AF = standardize(d$avgfood)
d$W = standardize(d$weight)
d$G = standardize(d$groupsize)

# what was that technique in the book used to 
# set the prior on slope?
# I remember seeing it in marriage happiness page 
# look up 

m6.6H3_2 = quap(
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
prior <- extract.prior(m6.6H3_2)
mu <- link(m6.6H3_2, post = prior, data = list(AF=c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab = "area" , ylab = 'weight')
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))

plot(d$W ~ d$AF)

# Posterior predictions 

precis(m6.6H3_2)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAF   -0.02 0.09 -0.17  0.12
# sigma  0.99 0.06  0.89  1.09


##### Prediction intervals #####
PROB_VALUE = 0.89 # he just picked all of these bc they are prime
PROB_VALUE = 0.67 # he just picked all of these bc they are prime
PROB_VALUE = 0.97 # he just picked all of these bc they are prime

# Define a sequence of weights to compute predictions for
# these values will be on the horizontal axis 
AF_seq <- seq(from=-2, to=2.5, length.out = 30)

#use link to compute mu for each sample from posterior
# and for each weight in weight.sq
mu <- link(m6.6H3_2, data=data.frame(AF=AF_seq))
str(mu)

# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=PROB_VALUE)

# simulated heights that embody the 
# uncertainty in the posterior as well as the 
# uncertainty in the Gaussian distribution of heights, using sim tool
sim.weight <- sim(m6.6H3_2, data=list(AF=AF_seq))
# sim.weight <- sim(m6.6H3_2, data=list(weight=AF_seq), n=1e4)
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

# Explanation for 6H4: 
# The estimated total causal effect of avgfood on weight is 
# -.02, with an sd of .09 and an 89% PI of -.17 to .12. (I sense a masked relationship?)
# Increasing the area is highly unlikely to make it healthier, 
# as this slope parameter is extremely close to 0.
# Also, no covatiates need to be adjusted for, as there are
# no open backdoor paths to avgfood in this model. 


# 6H5: Now infer the causal impact of group size. 
# Which covariates do you need to adjust for? 
# Looking at the posterior distribution of the resulting model, 
# what do you think explains these data? 
# That is, can you explain the estimates for all three problems? 
# How do they go together? 

adjustmentSets(provided_fox_DAG, exposure= 'groupsize', outcome="weight")
# { avgfood }

# load data
data(foxes)
d <- foxes

# standardize variables 
d$Ar = standardize(d$area)
d$AF = standardize(d$avgfood)
d$W = standardize(d$weight)
d$G = standardize(d$groupsize)

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

precis(m6.6H5)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAF    0.48 0.18  0.19  0.76
# bG    -0.57 0.18 -0.86 -0.29
# sigma  0.94 0.06  0.84  1.04

plot(precis(m6.6H5))

pairs( ~ W + AF + G, data = d, col = rangi2)

# Multicollinearity ? Let's look closer....

# Now test for multicollinearity like with RBT data: 

# To simulate from the priors, we can use extract.prior and link as in the previous chapter
# We'll plot the lines over the range of 2 standard deviations for both the outcome and predictor.
# That'll cover most of the possible range of both variables
set.seed(10)
prior <- extract.prior(m6.6H5)
mu <- link(m6.6H5, post = prior, data = list(AF = c(-2,2), G= c(-2,2)))
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha('black', 0.4))


precis(m6.6H5)

plot(coeftab(m6.6H5))

plot(precis(m6.6H5))

# Joing posterior distribution for X and Z 
post <- extract.samples(m6.6H5)
plot(bAF ~ bG, post, col = col.alpha(rangi2,0.1), pch=16)

# Compute + plot the posterior distribution of their sum:
sum_bafbg <- post$bAF + post$bG
dens(sum_bafbg, col = rangi2, lwd = 2, xlab='sum of bX and bZ')

PI(sum_bafbg, .89)

mean(sum_bafbg)

HPDI(sum_bafbg, .89)


pairs( ~ W + G + AF, data =d, col = rangi2)

# Start by modeling W as a function of AF and G but in two bivariate regressions 

m6.6H5_2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bAF*AF,
    a ~ dnorm(0, 0.2),
    bAF ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = d
)

m6.6H5_3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bG*G,
    a ~ dnorm(0, 0.2),
    bG ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6H5_2)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAF   -0.02 0.09 -0.17  0.12
# sigma  0.99 0.06  0.89  1.09

precis(m6.6H5_3)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bG    -0.16 0.09 -0.30 -0.01
# sigma  0.98 0.06  0.88  1.08

precis(m6.6H5)
#        mean   sd  5.5% 94.5%
# a      0.00 0.08 -0.13  0.13
# bAF    0.48 0.18  0.19  0.76
# bG    -0.57 0.18 -0.86 -0.29
# sigma  0.94 0.06  0.84  1.04


plot(precis(m6.6H5_2))

plot(precis(m6.6H5_3))

plot(precis(m6.6H5))

plot(coeftab(m6.6H5, m6.6H5_2, m6.6H5_3))

pairs( ~ W + G + AF, data =d, col = rangi2)

# So I wouldn't say it's really bad multicollinearity...

plot(coeftab(m6.6H5, m6.6H5_2, m6.6H5_3, m6.6H3_2, m6.6H3))

# 6H5 explantion:
# Masked relationship between avgfood and Groupsize - 
# when controlling for average food, group size makes a differnce.
# likewise, when controlling for groupsize, average food makes a difference.
# By themselves, without controlling for the other, the relationship is masked.
# Area likely is associated with avgfood, but again, this effect would be masked without knowing group size,
# which is why by itself it seemed so small.

cor(d$AF, d$G) # supports the idea 


# 6H5: 
# Consider your own research question. 
# Draw a DAG to represent it. 
# What are the testable implications of your DAG? 
# Are there any variables you could condition on to close all backdoor paths? 
# Are there unobserved variables that you have omitted? 
# Would a reasonable colleague imagine additional threats to causal inference that you have ignored? 

# Reserach question - what are the effects of learning orientation (learning mindset / growth mindset)
# on math GPA? 

# P = Parental Autonomy 
# GMA = general mental ability 
# C = conscientiousness 
# MGPA = math GPA 
# LM = learning mindset


learning_mindset_DAG <- dagitty("dag {
                                PA -> LM
                                PA -> C
                                C -> MGPA
                                LM -> MGPA
                                GMA -> MGPA
                                GMA -> C
                                }")


drawdag(learning_mindset_DAG)

impliedConditionalIndependencies(learning_mindset_DAG)
# C _||_ LM | PA
# GMA _||_ LM
# GMA _||_ PA
# MGPA _||_ PA | C, GMA, LM

adjustmentSets(learning_mindset_DAG, exposure = "LM", outcome = "MGPA")
# { C, GMA }
# { PA }

# Since LM is associated with taking more advanced math courses (Yeager et.al 2021),
# would that impact MGPA? We should limit it to specific class to eliminate that possiblity 
# e.g, harder classes result in lower GPA just due to difficulty of material.
# So, just take Calculus 2 GPA for example. Perhaps also consider effect of professor.

# A study could therefore simply recruit students taking a Calculus / other math class
# to take a survey that included scales to evaluate 
# general mental ability,
# conscientiousness,
# levels of learning mindset,
# and parental autonomy / support,
# and also get the class grade for the student,
# and use these values to evaluate the 
# testable implied conditional independencies of the DAG,
# as well as the total causal impact of LM on MGPA 
# when conditioning with both GMA and C to close the backdoor (or just PA).
# It would be trivial to also have records of the teacher of the class
# to consider that if necessary as well (or just only have one teacher/class in the study). 


# 6H7:
# For the DAG you made in the previous problem, 
# can you write a data generating simulation for it? 
# Can you design one or more statistical models to produce causal estimates? 
# If so, try to calculate interesting counterfactuals. 
# If not, use the simulation to estimate the size of the bias you might expect. 
# Under what conditions would you, for example, 
# infer the opposite of a true causal effect? 

# Look for book examples of his simulations that combined multiple to see how he did it 
# (multiple arrows in one variable)
# So two examples, search -age in chapter 5 and simulation (G_C) on this one (ch 6) that actually used predefined slopes

N = 1000

PA_u = rnorm(N)

GMA_u = rnorm(N)

LM_u = rnorm(N, PA_u)

C_u = rnorm(N, GMA_u + PA_u)

MGPA_u = rnorm(N, GMA_u + C_u + LM_u)

df_6H7 = data.frame(cbind(PA_u, GMA_u, LM_u, C_u, MGPA_u))

df_6H7$PA = standardize(df_6H7$PA_u)

df_6H7$GMA = standardize(df_6H7$GMA_u)
df_6H7$LM = standardize(df_6H7$LM_u)
df_6H7$C = standardize(df_6H7$C_u)
df_6H7$MGPA = standardize(df_6H7$MGPA_u)


# Causal estimate models and counterfactuals: 


# Causal estimate of effect of LM on MGPA (also may just do PA on MGPA)

m6.6H7 <- quap(
  alist(
    MGPA ~ dnorm(mu, sigma),
    mu <- a + bLM*LM + bC*C + bGMA*GMA,
    a ~ dnorm(0, 0.2),
    bLM ~ dnorm(0, 1),
    bC ~ dnorm(0, 1),
    bGMA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df_6H7
)


precis(m6.6H7)
#       mean   sd  5.5% 94.5%
# a     0.00 0.01 -0.01  0.01
# bLM   0.44 0.01  0.42  0.45
# bC    0.52 0.01  0.50  0.54
# bGMA  0.30 0.01  0.28  0.32
# sigma 0.29 0.01  0.28  0.30

plot(precis(m6.6H7))


# Let's try PA on MGPA 

m6.6H7_2 <- quap(
  alist(
    MGPA ~ dnorm(mu, sigma),
    mu <- a + bPA*PA,
    a ~ dnorm(0, 0.2),
    bPA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df_6H7
) 


precis(m6.6H7_2)
#       mean   sd  5.5% 94.5%
# a     0.00 0.02 -0.04  0.04
# bPA   0.61 0.02  0.57  0.65
# sigma 0.79 0.02  0.76  0.82

plot(precis(m6.6H7_2))


# 5H2: Assuming that the DAG for the divorce example is indeed M -> A -> D, 
# fit a new model and use it to estimate 
# the counterfactual effect of halving a State's marriage rate M. 
# Use the counterfactual example from the chapter (starting on page 140) as a template. 

# Running two regressions at the same time, to also estimtate the influence of A on M

m6.5H2 <- quap(
  alist(
    ## (LM, C, GMA) -> MGPA
    MGPA ~ dnorm(mu, sigma),
    mu <- a + bLM*LM + bC*C + bGMA*GMA,
    a ~ dnorm(0, 0.2),
    bLM ~ dnorm(0, 1),
    bC ~ dnorm(0, 1),
    bGMA ~ dnorm(0, 1),
    sigma ~ dexp(1),
    ## GMA -> C 
    C ~ dnorm(mu_C, sigma_C),
    mu_C <- aC + bGMAC*GMA,
    aC ~ dnorm(0,0.2),
    bGMAC ~ dnorm(0, 1),
    sigma_C ~ dexp(1)
  ), data = df_6H7
)

precis(m6.5H2)
#          mean   sd  5.5% 94.5%
# aM       0.00 0.09 -0.14  0.14
# bMA     -0.69 0.10 -0.85 -0.54
# sigma_M  0.68 0.07  0.57  0.79
# aD       0.00 0.10 -0.16  0.16
# bAD     -0.57 0.11 -0.74 -0.39
# sigma_D  0.79 0.08  0.66  0.91

# Real:
#         mean   sd  5.5% 94.5%
# a       0.00 0.01 -0.01  0.01
# bLM     0.44 0.01  0.42  0.45
# bC      0.52 0.01  0.50  0.54
# bGMA    0.30 0.01  0.28  0.32
# sigma   0.29 0.01  0.28  0.30
# aC      0.00 0.03 -0.04  0.04
# bGMAC   0.58 0.03  0.54  0.62
# sigma_C 0.81 0.02  0.78  0.84

# Look at the precis(m6.5H2) summary. You'll see that M and A are strongly negatively associated.
# If we interpret this causally, it indicates that manipulating M reduces A. (increasing marriage rate reduces age at marriage ? )
# Look at the precis(m6.5H2) summary. You'll see that A and D are strongly negatively associated.
# If we interpret this causally, it indicates that manipulating A reduces D (increasing age at marriage reduces divorce rate). 

# The goal is to simulate what would happen, if we manipulate M. (GMA)
# So, next we define a range of values for M.
GMA_seq <- seq(from = -2, to = 2, length.out=30)

# this defines a list of 30 imaginary interventions, 
# ranging from 2 standard deviations below and 2 above the mean

# Now we can use sim to simulate observations from model m6.5H2. 
# But this time we'll tell it to simulate both M and D, in that order. 
# Why in that order? 
# Because we have to simulate the influence of A on M before we 
# simulate the joint influence of A and M on D. 
# The vars argument to sim tells it both which observables to simulate 
# and in which order: 

# prep data
sim_dat <- data.frame(GMA = GMA_seq, LM = 0)

# simulate A and then D, using M_seq
s <- sim(m6.5H2, data = sim_dat, vars=c('C', "MGPA"))
# That's all there is to it - but do look at the overthinking box on page 144 
# to see the individual steps, so you can perform this kind of
# counterfactual simulation for any model fit with any software

# Now to plot the predictions:
plot(sim_dat$GMA, colMeans(s$MGPA), ylim=c(-2,2), type = 'l', xlab = 'manipulated GMA', ylab = "counterfactual MGPA")
shade(apply(s$MGPA, 2, PI), sim_dat$GMA)
mtext("Total counterfactual effect of GMA on MGPA")

plot(sim_dat$GMA, colMeans(s$C), ylim=c(-2,2), type = 'l', xlab = 'manipulated GMA', ylab = "counterfactual C")
shade(apply(s$C, 2, PI), sim_dat$GMA)
mtext("Counterfactual effect GMA -> C")

mean(df_6H7$GMA)
median(df_6H7$GMA)

sd(df_6H7$GMA)

range(df_6H7$GMA)

mean(df_6H7$C)

(30-26.1) / 1.24

# mean(WaffleDivorce$MedianAgeMarriage)
# sd(WaffleDivorce$MedianAgeMarriage)
# 
# range(WaffleDivorce$MedianAgeMarriage)
# 
# mean(WaffleDivorce$Marriage)
# sd(WaffleDivorce$Marriage)
# 
# range(WaffleDivorce$Marriage)

# Of course these calculation also permit numerical summaries. 
# For example, the expected causal effect of increasing one's GMA from the mean to 1 sd above mean:
# new data frame, standardized to mean 26.1 and std dev 1.24
# sim2_dat <- data.frame(A=(c(20,30)-26.1)/1.24)
sim2_dat <- data.frame(GMA=c(0,1), LM = 0)
s2 <- sim(m6.5H2, data=sim2_dat, vars =c("C","MGPA"))
mean(s2$MGPA[,2] - s2$MGPA[,1])
# 0.5680669
# This is an effect of over one half standard deviations. 
#  MGPA would increase by 0.5680669 standard deviations.  

# Note this is entirely impossible as one cannot change GMA. 
# But the same structure would apply for variables one could actually manipulate. 
# For example, what if we also included Parental Autonomy in the model? 
# How would an intervention on increasing parenting autonomy for students' parents
# affect MGPA? 

# First, a multiple regression model with all of them 
# - we expect MGPA _||_ PA | C, GMA, LM based on the simulation of the model
m6.6H7_3 <- quap(
  alist(
    MGPA ~ dnorm(mu, sigma),
    mu <- a + bLM*LM + bC*C + bGMA*GMA + bPA*PA,
    a ~ dnorm(0, 0.2),
    bLM ~ dnorm(0, 1),
    bC ~ dnorm(0, 1),
    bGMA ~ dnorm(0, 1),
    bPA ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = df_6H7
)

precis(m6.6H7_3)
#        mean   sd  5.5% 94.5%
# a      0.00 0.01 -0.02  0.02
# bLM    0.45 0.01  0.43  0.47
# bC     0.52 0.02  0.50  0.55
# bGMA   0.29 0.01  0.27  0.31
# bPA   -0.02 0.02 -0.04  0.01
# sigma  0.32 0.01  0.30  0.33

plot(precis(m6.6H7_3)) # MGPA _||_ PA | C, GMA, LM is evident! 

# Now the counterfactual model
m6.5H2_2 <- quap(
  alist(
    ## (LM, C, GMA) -> MGPA
    MGPA ~ dnorm(mu, sigma),
    mu <- a + bLM*LM + bC*C + bGMA*GMA,
    a ~ dnorm(0, .2),
    bLM ~ dnorm(0, 1),
    bC ~ dnorm(0, 1),
    bGMA ~ dnorm(0, 1),
    bPA ~ dnorm(0, 1),
    sigma ~ dexp(1),
    ## PA -> C 
    C ~ dnorm(mu_C, sigma_C),
    mu_C <- aC + bPAC*PA,
    aC ~ dnorm(0, .2),
    bPAC ~ dnorm(0, 1),
    sigma_C ~ dexp(1),
    ## PA -> LM
    LM ~ dnorm(mu_LM, sigma_LM),
    mu_LM <- aLM + bPALM*PA,
    aLM ~ dnorm(0, .2),
    bPALM ~ dnorm(0, 1),
    sigma_LM ~ dexp(1)
  ), data = df_6H7
)

precis(m6.5H2_2)

#          mean   sd  5.5% 94.5%
# a        0.00 0.01 -0.01  0.01
# bLM      0.45 0.01  0.43  0.47
# bC       0.52 0.01  0.50  0.54
# bGMA     0.28 0.01  0.26  0.30
# bPA      0.00 0.50 -0.80  0.80
# sigma    0.30 0.01  0.29  0.31
# aC       0.00 0.03 -0.04  0.04
# bPAC     0.59 0.03  0.55  0.63
# sigma_C  0.80 0.02  0.77  0.83
# aLM      0.00 0.02 -0.03  0.03
# bPALM    0.72 0.02  0.69  0.76
# sigma_LM 0.69 0.02  0.66  0.71


# The goal is to simulate what would happen, if we manipulate PA
# So, next we define a range of values for M.
PA_seq <- seq(from = -2, to = 2, length.out=30)

# this defines a list of 30 imaginary interventions, 
# ranging from 2 standard deviations below and 2 above the mean

# Now we can use sim to simulate observations from model m6.5H2_2. 
# But this time we'll tell it to simulate both M and D, in that order. 
# Why in that order? 
# Because we have to simulate the influence of A on M before we 
# simulate the joint influence of A and M on D. 
# The vars argument to sim tells it both which observables to simulate 
# and in which order: 

# prep data
sim_dat <- data.frame(PA = PA_seq, GMA = 0)

# simulate A and then D, using M_seq
s <- sim(m6.5H2_2, data = sim_dat, vars=c("LM","C","MGPA"))
# That's all there is to it - but do look at the overthinking box on page 144 
# to see the individual steps, so you can perform this kind of
# counterfactual simulation for any model fit with any software

# Now to plot the predictions:
plot(sim_dat$PA, colMeans(s$MGPA), ylim=c(-2,2), type = 'l', xlab = 'manipulated PA', ylab = "counterfactual MGPA")
shade(apply(s$MGPA, 2, PI), sim_dat$PA)
mtext("Total counterfactual effect of PA on MGPA")

plot(sim_dat$PA, colMeans(s$C), ylim=c(-2,2), type = 'l', xlab = 'manipulated PA', ylab = "counterfactual C")
shade(apply(s$C, 2, PI), sim_dat$PA)
mtext("Counterfactual effect PA -> C")

plot(sim_dat$PA, colMeans(s$LM), ylim=c(-2,2), type = 'l', xlab = 'manipulated PA', ylab = "counterfactual LM")
shade(apply(s$LM, 2, PI), sim_dat$PA)
mtext("Counterfactual effect PA -> LM")

mean(df_6H7$PA)
median(df_6H7$PA)

sd(df_6H7$PA)

range(df_6H7$PA)

mean(df_6H7$C)

# Of course these calculation also permit numerical summaries. 
# For example, the expected causal effect of increasing one's GMA from the mean to 1 sd above mean:
# new data frame, standardized to mean 26.1 and std dev 1.24
# sim2_dat <- data.frame(A=(c(20,30)-26.1)/1.24)
sim2_dat <- data.frame(PA=c(0,1), GMA = 0)
s2 <- sim(m6.5H2_2, data=sim2_dat, vars =c("LM","C","MGPA"))
mean(s2$MGPA[,2] - s2$MGPA[,1])
# 0.6134259
# This is an effect of over one half standard deviations. 
# MGPA would increase by 0.6134259 standard deviations.  
