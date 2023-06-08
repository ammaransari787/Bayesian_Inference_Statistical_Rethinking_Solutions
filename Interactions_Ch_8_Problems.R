# Load required libraries:
library(rethinking)
library(dagitty)
library(tidyverse)
library(glue)


##### Practice problems #####

# 8E1:
# For eaach of the causal relationships below, 
# name a hypothetical third variable that would lead to an interaction effect. 
# 1) Bread dough rises because of yeast. 
# Heat. I don't think it would rise with no added heat. 
# 2) Education leads to higher income.
# Major / field of study. For example, graduate education in some fields (art, film) may not lead to consistent increases in income. 
# 3) Gasoline makes a car go.
# Battery/electricty. If car battery is dead but gas tank is full, car still will not go. 


# 8E2:
# Which of the following explanations invokes an interaction? 
# 1 


# 8E3:
# For each of the explanations in 8E2, write a linear model that expresses the stated relationship
# 1: mu_i = a + bH*H_i + bM*M_i + bMH*M_i*H_i
# 2: mu_i = a + bC*C_i + bI*I_i
# 3: mu_i = a + bP*P_i + bF*F_i
# 4: mu_i = a + bS*S_i + bA*A_i


# 8M1:
# Recall the tulips example from the chapter. 
# Suppose another set of treatments adjusted the 
# temperature in the greenhouse over two levels: 
# cold and hot. 
# The data in the chapter were collected at the cold temperature. 
# You find none of the plants grown under the hot temperature 
# developed any blooms at all, regardless of the water and shade levels. 
# Can you explain this result in terms of interactions between 
# water shade and temperature? 

# Temperature moderates the relationship between blooms and water/shade.

# mu_i = a[tid] + bW[tid]*W + bS[tid]*S + bWS[tid]*W*S

# At cold temperatures, diff slopes between all three than at high temps.
# Two levels, just like country ruggedness vs log GDP example in text. 


# 8M2:
# Can you invent a regression equation that would make the bloom size zero, whenever the termperature is hot? 

# dd$tid <- ifelse(dd$temp_cold==1, 1, 2)
# mu_i = a[tid] + bW[tid]*W + bS[tid]*S + bWS[tid]*W*S

# Simulation test of this algorithm 

# view(d)

nrow(d)

og_temp_cold <- rep(1, 27)

d$temp_cold <- og_temp_cold

new_blooms = rep(0, 27)
new_temp_cold 

concaten = tulips

concaten$blooms_std <- concaten$blooms / max(concaten$blooms)
concaten$water_cent <- concaten$water - mean(concaten$water)
concaten$shade_cent <- concaten$shade - mean(concaten$shade)

concaten$blooms_std <- new_blooms

concaten$temp_cold <- rep(0,27)

d <- rbind(d, concaten)

nrow(d)

d$tid <- ifelse(d$temp_cold==1, 1, 2)

# view(d)

my_quap_test_temp_cold <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a[tid] + bw[tid]*water_cent + bs[tid]*shade_cent + bws[tid]*water_cent*shade_cent,
    a[tid] ~ dnorm(0, 1),
    bw[tid]  ~ dnorm(0, 0.25),
    bs[tid]  ~ dnorm(0, 0.25),
    bws[tid]  ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = d
)

# So before I run this precis, I anticipate a[cid][2] to be 0 and bws[cid2] to be 0

precis(my_quap_test_temp_cold, depth = 2)
# mean   sd  5.5% 94.5%
# a[1]    0.36 0.02  0.33  0.38
# a[2]    0.00 0.02 -0.03  0.03
# bw[1]   0.21 0.02  0.17  0.24
# bw[2]   0.00 0.02 -0.03  0.03
# bs[1]  -0.11 0.02 -0.15 -0.08
# bs[2]   0.00 0.02 -0.03  0.03
# bws[1] -0.14 0.03 -0.19 -0.10
# bws[2]  0.00 0.03 -0.04  0.04
# sigma   0.09 0.01  0.07  0.10

# Perfect! Just what we hoped we would see. Cool, so this solution looks good. 


# 8M3: 
# In parts of North America, ravens depend upon wolves for their food. 
# This is because ravens are carnivorous but 
# cannot usually kill or open carcasses of prey. 
# Wolves however can and do kill and tear open animals, 
# and they tolerate ravens co-feeding at their kills. 
# This species relationship is generally described as a 'species interaction". 
# Can you invent a hypothetical set of data on raven population size in which 
# this relationship would manifest as a statistical interaction? 
# Do you think the biological interaction could be linear? 
# Why or why not? 


# Say there are 10 cities 
N = 100
# appropriate_habitat <- rnorm(N, 0, 1)
# 
# wolf_population <- rnorm(N, 0, 1)
# 
# raven_population <- rnorm(N, wolf_population*appropriate_habitat)

appropriate_habitat <- rnorm(N, 0, 1)

wolf_population <- rnorm(N, appropriate_habitat)

AH <- standardize(appropriate_habitat)

WP <- standardize(wolf_population)

AH_rounded <- round(AH, digits = 0)
AH_cent <- ifelse(AH_rounded == 2, 1, ifelse(AH_rounded == -2, -1, AH_rounded))

WP_rounded <- round(WP, digits = 0)
WP_cent <- ifelse(WP_rounded == 2, 1, ifelse(WP_rounded == -2, -1, WP_rounded))

AH_scaled <- AH_cent + 1

WP_scaled <- WP_cent + 1

raven_population <- rnorm(N, AH_scaled*WP_scaled) 
# # now if either is 0 (lowest), 
# will not allow RP bc 0* other will cancel. 
# Similar to shade + water example.

RP <- standardize(raven_population)

RP_rounded <- round(RP, digits = 0)
RP_cent <- ifelse(RP_rounded == 2, 1, ifelse(RP_rounded == -2, -1, RP_rounded))

AH_scaled <- AH_cent + 1

RP_scaled <- RP_cent + 1

RP_cent <- RP_scaled

AH_cent <- AH_scaled

WP_cent <- WP_scaled



# d <- data.frame(cbind(appropriate_habitat, wolf_population,raven_population))

d <- data.frame(cbind(RP_cent, WP_cent,AH_cent))


# raven_pop ~ dnorm(mu, sigma),
# mu <- a + bA*appropriate_habitat + bW*Wolf_population + bWA*appropriate_hbaitat*Wolf_population 

# # converting them all to -1 0 1
# 
# d$AH <- standardize(d$appropriate_habitat)
# 
# d$WP <- standardize(d$wolf_population)
# 
# d$RP <- standardize(d$raven_population)
# 
# d$AH_rounded <- round(d$AH, digits = 0)
# d$AH_cent <- ifelse(d$AH_rounded == 2, 1, ifelse(d$AH_rounded == -2, -1, d$AH_rounded))
# 
# d$WP_rounded <- round(d$WP, digits = 0)
# d$WP_cent <- ifelse(d$WP_rounded == 2, 1, ifelse(d$WP_rounded == -2, -1, d$WP_rounded))
# 
# d$RP_rounded <- round(d$RP, digits = 0)
# d$RP_cent <- ifelse(d$RP_rounded == 2, 1, ifelse(d$RP_rounded == -2, -1, d$RP_rounded))
# 
# colnames(d) # [1] "appropriate_habitat" "wolf_population"     "raven_population"   
# 


simulation_quap_wolf_raven <- quap(
  alist(
    RP_cent ~ dnorm(mu, sigma),
    mu <- a + bA*AH_cent + bW*WP_cent + bWA*AH_cent*WP_cent,
    a ~ dnorm(1,.2),
    bA ~ dnorm(0, .25),
    bW ~ dnorm(0, .25),
    bWA ~ dnorm(0, .25),
    sigma ~ dexp(1)
  ), data = d
)

precis(simulation_quap_wolf_raven)
# Before cent:
# mean   sd  5.5% 94.5%
#   a     -0.05 0.08 -0.19  0.08
# bA    -0.09 0.09 -0.24  0.05
# bW    -0.06 0.09 -0.20  0.08
# bWA    0.87 0.08  0.74  1.01
# sigma  0.90 0.06  0.80  1.01

#After cent:
# mean   sd  5.5% 94.5%
#   a     -0.03 0.07 -0.14  0.07
# bA    -0.10 0.08 -0.23  0.03
# bW    -0.10 0.08 -0.23  0.03
# bWA    0.46 0.09  0.32  0.61
# sigma  0.70 0.05  0.62  0.77

# First, let's plot our prior distribution:
# prior predictive plots for model m8.5 (with interaction)
set.seed(7)
prior <- extract.prior(simulation_quap_wolf_raven)
par(mfrow=c(1,3)) # 3 plots in 1 row
for (s in 0:2) {
  idx <- which(d$WP_cent==s)
  plot(d$AH_cent[idx], d$RP_cent[idx], xlim = c(0,2), ylim = c(-3,3), xlab = 'habitat', ylab = 'raven population', pch=16, col=rangi2)
  mu <- link(simulation_quap_wolf_raven, post = prior, data=data.frame(WP_cent=s, AH_cent=0:2))
  for (i in 1:20) lines(0:2, mu[i,], col=col.alpha("black", .3))
  abline(h=0, lty=2) # realistic range line -1
  abline(h=2, lty=2) # realistic range line 1
  mtext(glue('WP = {s}'))
}


# Let's do some plotting with triptychs:
par(mfrow=c(1,3)) # 3 plots in 1 row
for (s in 0:2) {
  idx <- which(d$WP_cent==s)
  plot(d$AH_cent[idx], d$RP_cent[idx], xlim = c(0,2), ylim = c(-3,3), xlab = 'habitat', ylab = 'raven population', pch=16, col=rangi2)
  mu <- link(simulation_quap_wolf_raven, data=data.frame(WP_cent=s, AH_cent=0:2))
  for (i in 1:20) lines(0:2, mu[i,], col=col.alpha("black", .3))
  abline(h=0, lty=2) # realistic range line -1
  abline(h=2, lty=2) # realistic range line 1
  mtext(glue('WP = {s}'))
}
# So the code and graphs work, but I'm thinking (first attempt) plots are sort of selection effect collider bias causing the reverse slopes.

# This second way the graph works way better. 
# With no wolves, flat relationship. 
# Slope increases as there are more wolves.
# However, I don't think that actual biological interaction could be linear - 
# My reasoning is that as the raven + wolf population grows, 
# the prey supporting the habitat's being accomodating will decrease, 
# and the habitat will become less accomodating - 
# a sort of breaking point will happen with the predators, 
# where they cannot continue to increase linearly. 


# 8M4:
# Repeat the tulips analysis, 
# but this time use priors that constrain the effect of water to be positive 
# and the effect of shade to be negative. 
# Use prior predictive simulation. 
# What do these prior assumptions mean for the interaction prior, if anything? 

# Load the data:
data(tulips)
d <- tulips
str(d)

# To make estimation easier, let's center W and S and scale B by its maximum:
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

# All together now, in code form:
m8.50 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a ~ dnorm(0.5, 0.25),
    bw ~ dnorm(.2, 0.1),
    bs ~ dnorm(-.2, 0.1),
    bws ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

# OG interaction
# m8.50 <- quap(
#   alist(
#     blooms_std ~ dnorm(mu, sigma),
#     mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
#     a ~ dnorm(0.5, 0.25),
#     bw ~ dnorm(0, 0.25),
#     bs ~ dnorm(0, 0.25),
#     bws ~ dnorm(0, 0.25),
#     sigma ~ dexp(1)
#   ), data = d
# )

# No interaction but w pos/neg slopes 
# m8.50 <- quap(
#   alist(
#     blooms_std ~ dnorm(mu, sigma),
#     mu <- a + bw*water_cent + bs*shade_cent,
#     a ~ dnorm(0.5, 0.25),
#     bw ~ dnorm(.3, 0.1),
#     bs ~ dnorm(-.3, 0.1),
#     sigma ~ dexp(1)
#   ), data = d
# )

# prior 
set.seed(7)
prior <- extract.prior(m8.50)
par(mfrow=c(1,3)) # 3 plots in 1 row
for (s in -1:1) {
  idx <- which(d$shade_cent==s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1), ylim = c(-1,2), xlab = 'water', ylab = 'blooms', pch=16, col=rangi2)
  mu <- link(m8.50, post = prior, data=data.frame(shade_cent=s, water_cent=-1:1))
  for (i in 1:20) lines(-1:1, mu[i,], col=col.alpha("black", .3))
  abline(h=0, lty=2) # realistic range line 0
  abline(h=1, lty=2) # realistic range line 1
  mtext(glue('m8.4post: shade = {s}'))
}

# Explanation 8M4:
# It doesn't really seem like it's doing anything 
# to the interaction prior predictions, except making the 
# slopes a bit more steep.
# The interaction patterns remain the same,
# where when shade s 1 we see the most possible flat slopes.
# Main difference really is more lines outside realm of possibilities,
# in all 3. 


# 8H1:
# Return to the data(tulips) example in the chapter. 
# Now include the bed variable as a predictor in the interaction model. 
# Don't interact bed with the other predictors; 
# just include it as a main effect. 
# Note that bed is categorical. 
# So to use it properly, 
# you will need to either construct dummy variables or rather an index variable, 
# as explained in chapter 5.

# Load the data:
data(tulips)
d <- tulips
str(d)

# To make estimation easier, let's center W and S and scale B by its maximum:
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

# view(d)

# creating bid index variable 
d$bid <- ifelse(d$bed == 'a', 1, ifelse(d$bed == "b", 2, 3))

# NOTE: He used the coerce_index function:
# ( d$bed_idx <- coerce_index( d$bed ) )

m8H1 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a[bid] + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a[bid] ~ dnorm(0.5, 0.25),
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0, 0.25),
    bws ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8H1, depth = 2)

# Diff between categories
post <- extract.samples(m8H1)
post$diff_1_2 <- post$a[,1] - post$a[,2]
post$diff_1_3 <- post$a[,1] - post$a[,3]
post$diff_2_3 <- post$a[,2] - post$a[,3]
precis(post, depth=2)


# 8H2:
# Use WAIC to compare the model from 8H1 to a model that omits bed. 
# What do you infer from this comparison? 
# Can you reconcile the WAIC results with the posterior distribution of the bed coefficients? 

compare(m8H1, m8.5)
#       WAIC    SE dWAIC  dSE pWAIC weight
# m8H1 -24.8  9.50   0.0   NA   9.0   0.75
# m8.5 -22.7 10.03   2.2 7.53   6.3   0.25

# 99% credibility interval of diff between models
2.2 + c(-1, 1)*7.53*2.6 # = -17.378  21.778

# 97% credibility interval of diff between models
2.2 + c(-1, 1)*7.53*1.881 # = -11.96393  16.36393

# 89% credibility interval of diff between models
2.2 + c(-1, 1)*7.53*1.227 # = -7.03931 11.43931

# Explanation for 8H2:
# You cannot truly say either model is better, as the credibility interval of the difference
# has 0 solidly inside of it. 
# This is likely because the difference between the categories was also small - 
# bed 2 and 3 were virtually the exact same.
# The 89% cred interval of the diff between bed 1 and 2/3 
# was -.20 to -.04 (1 to 2) or -.22 to -.06 (1 to 3). 
# So the intercept for bed 1 was on average .12 less than bed 2 and .14 less than bed 3,
# so essentially if shade and water were at the mean, instead of being at .4 blooms_std 
# (with 1 maximum and 0 minimum), 
# bed 1 was at .27 blooms.
# Since bed 2/3 were so close, and diff between 1 and 2/3 wasn't extremely strong, 
# you aren't gaining too much by simply including this as a main effect. 


# 8H3: 
# Consider again the data(rugged) data on economic development and terrain ruggedness, 
# examined in this chapter. 
# One of the African countries in that example, Seychelles, 
# is far outside the cloud of other nations, 
# being a rare country with both relatively high GDP and high ruggedness. 
# Seychelles is also unusual, 
# in that it is a group of islands far from the coast of mainland Africa, 
# and its main economic activity is tourism. 
# A) Focus on model m8.5 from the chapter. 
# Use WAIC pointwise penalties and 
# PSIS Pareto k values to measure relative influence of each country. 
# By these criteria, 
# is Seychelles influencing the results? 
# Are there other nations that are relatively influential? 
# If so, can you explain why? 

plot(PSIS(m8.3, pointwise = TRUE)$k,  WAIC(m8.3, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)


plot(PSIS(m8.3, pointwise = TRUE)$k)

plot(WAIC(m8.3, pointwise = TRUE)$penalty)


# view(PSIS(m8.3, pointwise = TRUE))

# view(dd)

nrow(dd)

nrow(PSIS(m8.3, pointwise = TRUE))

copy_PSIS <- PSIS(m8.3, pointwise = TRUE)

copy_dd <- data.frame(dd$country)

copy_PSIS$index <- rep(0, 170)

colnames(copy_PSIS)

i = 1
while(i <= nrow(copy_PSIS))
{
  copy_PSIS[i,6] = i
  i = i + 1
}

# view(copy_PSIS)

copy_dd$index <- rep(0, 170)

colnames(copy_dd)

i = 1
while(i <= nrow(copy_dd))
{
  copy_dd[i,2] = i
  i = i + 1
}

# view(copy_dd)

merged_PSIS_country_names <- merge(copy_PSIS, copy_dd, on = 'index')

plot(merged_PSIS_country_names$k)

# identify(x=merged_PSIS_country_names$dd.country, y=merged_PSIS_country_names$k, labels = merged_PSIS_country_names$dd.country)

ggplot(aes(x = index, y = k, label = dd.country), data = merged_PSIS_country_names) +
  geom_point() +
  geom_text(hjust=-0.1, vjust=.1) +
  coord_cartesian(xlim = c(0,175))

# So Seychelles is the only one with a k value > .5, so it could be seen
# as an outlier (though it is not above .7, so not excessively bad.)

copy_WAIC <- WAIC(m8.3, pointwise = TRUE)

# view(copy_WAIC)

copy_WAIC$index <- rep(0, 170)

colnames(copy_WAIC)

i = 1
while(i <= nrow(copy_WAIC))
{
  copy_WAIC[i,5] = i
  i = i + 1
}

# view(copy_WAIC)

copy_WAIC <- subset(copy_WAIC, select = c(penalty, index))

# view(copy_WAIC)

colnames(copy_WAIC)[1] = 'waic_penalty'

merged_PSIS_country_names <- merge(merged_PSIS_country_names,copy_WAIC, on = 'index')

ggplot(aes(x = index, y = waic_penalty, label = dd.country), data = merged_PSIS_country_names) +
  geom_point() +
  geom_text(hjust=-0.1, vjust=.1) +
  coord_cartesian(xlim = c(0,175))

ggplot(aes(x = k, y = waic_penalty, label = dd.country), data = merged_PSIS_country_names) +
  geom_point() +
  geom_text(hjust=-0.1, vjust=.1) +
  coord_cartesian(xlim = c(0,2))

# view(merged_PSIS_country_names)

# Seychelles has the hightest both waic penalty and k
# Switzerland also has as slightly higher than .5 k 
# However, neither of these are above .7 so not too bad. 

precis(m8.3, depth = 2)
# mean   sd  5.5% 94.5%
# a[1]   0.89 0.02  0.86  0.91
# a[2]   1.05 0.01  1.03  1.07
# b[1]   0.13 0.07  0.01  0.25
# b[2]  -0.14 0.05 -0.23 -0.06
# sigma  0.11 0.01  0.10  0.12

ggplot(aes(x = rugged_std, y = log_gdp_std, label = country ), data = dd) +
  geom_point() +
  geom_text(hjust=-0.1, vjust=.1)

plot(log_gdp_std ~ rugged_std, data = dd)

# They are relatively influential because despite having high ruggedness,
# they still have high gdp. 

# B) 
# Now use robust regression, as described in the previous chapter. 
# Modify m8.3 to use a Student-t distribution with v = 2. 
# Does this change the results in a substantial way? 

m8.3 <- quap(
  alist(
    log_gdp_std ~ dstudent(2, mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data = dd
)

precis(m8.3, depth = 2)
# mean   sd  5.5% 94.5%
# a[1]   0.86 0.02  0.84  0.89
# a[2]   1.05 0.01  1.03  1.06
# b[1]   0.11 0.08 -0.01  0.23
# b[2]  -0.21 0.06 -0.32 -0.11
# sigma  0.08 0.01  0.07  0.10

# compared to what it was before:
# mean   sd  5.5% 94.5%
# a[1]   0.89 0.02  0.86  0.91
# a[2]   1.05 0.01  1.03  1.07
# b[1]   0.13 0.07  0.01  0.25
# b[2]  -0.14 0.05 -0.23 -0.06
# sigma  0.11 0.01  0.10  0.12

post <- extract.samples(m8.3)
diff_b1_b2 <- post$b[,1] - post$b[,2]
PI(diff_b1_b2) 
post$diff_b1_b2 = diff_b1_b2
precis(post, depth = 2)
#          5%       94% 
#   0.1700493 0.4847230 

# Let's also look at waic penalty and k:
plot(PSIS(m8.3, pointwise = TRUE)$k,  WAIC(m8.3, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)
# None above .5

PSIS(m8.3) # throws no error

# Results were actually different after using the robust regression, 
# mainly for non-african countries - stronger negative slope 
# African countries also slightly less strong positve slope, that also included 0 in 89% credibility interval.
# So seychelles was influencing the results, as well as other countries, such as switerzerland.


# 8H4: 
# The values in data(nettle) are data on language diversity in 74 nations. 
# The meaning of each column is given below:

# country, 
# number languages spoken, 
# area, 
# population in thousands, 
# number of weather stations that provided data for the next two columns, 
# mean growing season length in months, 
# standard deviation of length of growing season in months. 

# Use these data to evaluate the hypothesis that language diversity is partly a product of food security. 
# The notion is that, in productive ecologies, 
# people don't need large social networks to buffer them against risk of food shortfalls. 
# This means cultural groups can be smaller and more self-sufficient, 
# leading to more languages per capita. 
# Use the number of languages per capita as the outcome: 
# d$lang.per.cap <- d$num.lang/ d$k.pop
# Use the logarithm of this new variable as your regression outcome.
# (A count model would be better here, but you'll learn those later, in Chapter 11.) 
# This problem is open ended, 
# allowing you to decide how you address the hypotheses and the uncertain advice the modeling provides. 
# If you think you need to use WAIC anyplace, please do. 
# If you think you need certain priors, argue for them.
# If you think you need to plot predictions in a certain way, please do. 
# Just try to honestly evaluate the main effects of both mean.growing.season and sd.growing.season, 
# as well as their two-way interaction. Here are three parts to help. 
# A) 
# Evaluate the hypothesis that language diversity, as measured by log(lang.per.cap), 
# is positively associated with the average length of the growing season, mean.growing.season. 
# Consider log(area) in your regressions as a covaraiate (not an intereaction). 
# Interpret your results. 
# B) 
# Now evaluate the hypothesis that language diversity is 
# negatively associated with the standard deviation of length of growing season, sd.growing.season. 
# This hypothesis follows form uncertainty in harvest favoring social insurance through 
# larger social networks and therefore fewer languages. 
# Again ,consider log(area) as a covariate (not an interaction). 
# Interpret your results. 
# C) 
# Finally, evaluate the hypothesis that mean.growing.season and sd.growing.season interact to 
# synergistically reduce language diversity. 
# The idea is that, in nations with longer average growing season, 
# high variance makes storage and redistribution even more important than it would be otherwise. 
# That way, people can cooperate to preserve and protect windfalls to be used during the droughts. 

data(nettle)
d <- nettle
d$lang.per.cap <- d$num.lang / d$k.pop
d$log_lang.per.cap <- log(d$lang.per.cap) # outcome variable 
hist(d$log_lang.per.cap)
d$z_log_lang.per.cap = standardize(d$log_lang.per.cap)

d$log_area <- log(d$area)

d$z_log_area <- standardize(d$log_area)

d$z_mean.growing.season <- standardize(d$mean.growing.season)

# A)
# Evaluate the hypothesis that language diversity, as measured by log(lang.per.cap), 
# is positively associated with the average length of the growing season, mean.growing.season. 
# Consider log(area) in your regressions as a covaraiate (not an intereaction). 
# Interpret your results. 
m8h4_1 <- quap(
  alist(
    z_log_lang.per.cap ~ dnorm(mu, sigma),
    mu <- a + bG*z_mean.growing.season + bA*z_log_area,
    a ~ dnorm(0, 0.2),
    bG ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

# Considering the priors:

# I'll adapt this section below, from text, to this situation.
# What about those slopes? 
# What would a very strong effect of water and shade look like? 
# How big could those slopes be in theory? 
# The range of both water and shade is 2 - 
# from -1 to 1 is 2 units. 
# To take us from the theoretical minimum of zero blooms on one end to the observed maximum of 1 - 
# a range of 1 unit - on the other would require a slope of 0.5 from either variable - 
# 0.5 x 2 = 1. 
# So if we assign a standard deviation of .25 to each, 
# then 95% of the prior slopes are from -0.5 to 0.5, 
# so either variable could in principle account for the entire range, 
# but it would be unlikely. 
# Remember, the goals here are to assign weakly informative priors to discourage overfitting - 
# impossibly large effects should be assigned low prior probability - 
# and also to force ourselves to think about waht the model means. 


# What about those slopes? 
# What would a very strong effect of water and shade look like? 
# How big could those slopes be in theory? 
# The range of both water and shade is 4 - 
# from -2 to 2 is 4 units. 
# To take us from the theoretical minimum of -2 lang on one end to the observed maximum of 2 - 
# a range of 4 units - on the other would require a slope of 1 from either variable - 
# 1 x 4 = 4. 
# So if we assign a standard deviation of .5 to each, 
# then 95% of the prior slopes are from -1 to 1, 
# so either variable could in principle account for the entire range, 
# but it would be unlikely. 
# Remember, the goals here are to assign weakly informative priors to discourage overfitting - 
# impossibly large effects should be assigned low prior probability - 
# and also to force ourselves to think about waht the model means. 

precis(m8h4_1)
#        mean   sd  5.5% 94.5%
# a      0.00 0.09 -0.15  0.15
# bG     0.29 0.11  0.11  0.46
# bA    -0.17 0.11 -0.34  0.01
# sigma  0.91 0.07  0.79  1.03

plot(precis(m8h4_1))
# The slope for mean.growing season is reliably positive,
# with an 89% credibility interval of .11 to .46 and mean of .29
# We can take this to mean that when holding log(area) constant,
# an increase in one standard deviation in mean.growing.season
# results in an increase in log(lang.diversity) of between 
# .11 and .46, and our certainty of this parameters is 89% - 
# there is an 89% chance that the slope falls in this range.
# Thus, the data supports the hypothesis that language diversity, 
# is positively associated with the average length of the growing season.
# NOTE: Actually, does NOT after noting outliers and using Robust
# regression instead - see below. 

# B)
# Now evaluate the hypothesis that language diversity is 
# negatively associated with the standard deviation of length of growing season, sd.growing.season. 
# This hypothesis follows form uncertainty in harvest favoring social insurance through 
# larger social networks and therefore fewer languages. 
# Again, consider log(area) as a covariate (not an interaction). 
# Interpret your results. 

d$z_sd.growing.season <- standardize(d$sd.growing.season)

m8h4_2 <- quap(
  alist(
    z_log_lang.per.cap ~ dnorm(mu, sigma),
    mu <- a + bSD*z_sd.growing.season + bA*z_log_area,
    a ~ dnorm(0, 0.2),
    bSD ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8h4_2)
#        mean   sd  5.5% 94.5%
# a      0.00 0.10 -0.15  0.15
# bSD   -0.14 0.12 -0.34  0.06
# bA    -0.19 0.12 -0.39  0.01
# sigma  0.94 0.08  0.82  1.06

plot(precis(m8h4_2))
# bSD has 0 in it's 89% credibility interval


# I want to compare the models,
# using PSIS to also get warnings about outliers,
# mainly just to test my informal hypothesis
# that m8h4_1 is better currently bc
# the slope bG didn't overlap with zero,
# while slope bSD did in m8h4_2.
compare(m8h4_1, m8h4_2, func = PSIS)
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
#         PSIS    SE dPSIS  dSE pPSIS weight
# m8h4_1 206.0 16.46   0.0   NA   4.9   0.96
# m8h4_2 212.5 17.69   6.5 5.49   5.7   0.04

# 99% crediblility interval of the difference bw model 1 and 2
6.5 + c(-1, 1)*5.49*2.6 # -7.774 20.774

# 97% crediblility interval of the difference bw model 1 and 2
6.5 + c(-1, 1)*5.49*1.881 # = -3.82669 16.82669

# 89% crediblility interval of the difference bw model 2 and 2
6.5 + c(-1, 1)*5.49*1.227 # = -0.23623 13.23623

# So, model 1 is allegedly better, but the 
# difference is barely bigger than the standard error
# of the difference,
# to the point that 0 still falls into our 89% credibility interval of 
# the difference between model 1 and model 2.
# Thus, we would now say that the data does NOT support 
# the hypothesis that 
# language diversity is positively associated with 
# the average length of the growing season.


# Additionally, we have some pareto k values greater than .5 in both models.
# Let's take a look at those in some plots. 
plot(PSIS(m8h4_1, pointwise = TRUE)$k,  WAIC(m8h4_1, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)
# yikes, crazy outlier here... pareto k nearly .8

plot(PSIS(m8h4_2, pointwise = TRUE)$k,  WAIC(m8h4_2, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)
# less extreme PSIS values, but still high at >.6, and also a super high p_waic value... 


# In light of this, let's use a robust regression and evaluate the findings to account for the outliers.

m8h4_1_robust <- quap(
  alist(
    z_log_lang.per.cap ~ dstudent(2, mu, sigma),
    mu <- a + bG*z_mean.growing.season + bA*z_log_area,
    a ~ dnorm(0, 0.2),
    bG ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8h4_1_robust)
# mean   sd  5.5% 94.5%
# a     -0.02 0.08 -0.15  0.11
# bG     0.14 0.09 -0.02  0.29
# bA    -0.21 0.10 -0.36 -0.06
# sigma  0.59 0.08  0.47  0.71

plot(precis(m8h4_1_robust))

# compared to what it was before:
# mean   sd  5.5% 94.5%
# a      0.00 0.09 -0.15  0.15
# bG     0.29 0.11  0.11  0.46
# bA    -0.17 0.11 -0.34  0.01
# sigma  0.91 0.07  0.79  1.03

# wow, that really weakened the association we saw. 
# Now the 89% credibility interval (-.02, 0.29) includes 0. 

# let's plot it 
plot(d$z_log_lang.per.cap ~ d$z_mean.growing.season)

ggplot(aes(x = z_mean.growing.season, y = z_log_lang.per.cap, label = country), data = d) + 
  geom_point() + 
  geom_text(hjust=-0.1, vjust=.1) +
  coord_cartesian(xlim = c(-2,2))  

# Let's determine the outlier via a plot of names and pareto k values 
copy_PSIS <- PSIS(m8h4_1, pointwise = TRUE)
# copy_PSIS <- PSIS(m8h4_1_robust, pointwise = TRUE)
# copy_PSIS <- PSIS(m8h4_2, pointwise = TRUE)

m8h4_2_robust <- quap(
  alist(
    z_log_lang.per.cap ~ dstudent(2, mu, sigma),
    mu <- a + bSD*z_sd.growing.season + bA*z_log_area,
    a ~ dnorm(0, 0.2),
    bSD ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

copy_PSIS <- PSIS(m8h4_2_robust, pointwise = TRUE)

copy_d <- data.frame(d$country)

copy_PSIS$index <- rep(0, nrow(copy_PSIS))

i = 1
while(i <= nrow(copy_PSIS))
{
  copy_PSIS[i,ncol(copy_PSIS)] = i
  i = i + 1
}

copy_d$index <- rep(0, nrow(copy_d))

i = 1
while(i <= nrow(copy_d))
{
  copy_d[i,ncol(copy_d)] = i
  i = i + 1
}

merged_PSIS_country_names <- merge(copy_PSIS, copy_d, on = 'index')

ggplot(aes(x = index, y = k, label = d.country), data = merged_PSIS_country_names) +
  geom_point() +
  geom_text(hjust=-0.1, vjust=.1) +
  coord_cartesian(xlim = c(0,175))

# Vanuatu is the outlier in the first model, with a k of around 8.

# using the robust model brings all the k's way down, below .4.

# The second model (non robust) has Vanuatu as an outlier as well.

# The robust second model has all pareto k values below .5, 
# and we can also see it this way:
plot(PSIS(m8h4_2_robust, pointwise = TRUE)$k,  WAIC(m8h4_2_robust, pointwise = TRUE)$penalty, xlab = "PSIS Pareto k", ylab = 'WAIC penalty', col=rangi2, lwd=2)


# Now that we know the robust model's are preferable, 
# let's compare them that way.

# But first, let's see if the posterior for second model is different:
precis(m8h4_2_robust)
#        mean   sd  5.5% 94.5%
# a     -0.03 0.08 -0.15  0.10
# bSD   -0.16 0.09 -0.31 -0.01
# bA    -0.12 0.11 -0.29  0.05
# sigma  0.57 0.07  0.45  0.69

plot(precis(m8h4_2_robust))

# As compared to what it was before:
#        mean   sd  5.5% 94.5%
# a      0.00 0.10 -0.15  0.15
# bSD   -0.14 0.12 -0.34  0.06
# bA    -0.19 0.12 -0.39  0.01
# sigma  0.94 0.08  0.82  1.06
plot(precis(m8h4_2))

# Wow, the robust regression actually made bSD 
# no longer have 0 in it's 89% credibility interval.
# Thus, we can say that the data DOES support the hypothesis that
# language diversity is negatively associated with the 
# standard deviation of length of growing season, sd.growing.season. 


# In light of this new information,
# with robust model 1 now having 0 in the bG slope credibility interval,
# and bSD not having 0 in it's 89% credibility interval,
# I want to compare the two models PSIS now, 
# to double check that there are no errors in terms of k values,
# and also see if now model 2 is the better predictive model, 
# as I suspect:
compare(m8h4_1_robust, m8h4_2_robust, func = PSIS)
#                PSIS    SE dPSIS  dSE pPSIS weight
# m8h4_2_robust 206.5 17.61   0.0   NA   3.9   0.63
# m8h4_1_robust 207.6 16.06   1.1 4.58   4.3   0.37

# So while it says 2 is better,
# the difference is so small you definitely cannot claim 
# that one is better than the other. 
# Look at the difference in PSIS vs the standard error of the difference.
# Credibility intervals using those will show the problem:

# 99% crediblility interval of the difference bw model 1 and 2
1.1 + c(-1,1)*4.58*2.6 # -10.808  13.008

# 97% crediblility interval of the difference bw model 1 and 2
1.1 + c(-1,1)*4.58*1.881 # -7.51498  9.71498

# 89% crediblility interval of the difference bw model 1 and 2
1.1 + c(-1,1)*4.58*1.227 # -4.51966  6.71966

# So tiny, 0 is almost right at the center of density...
# Basically 0 difference.

# The plot also shows this:
plot(compare(m8h4_1_robust, m8h4_2_robust))

# What will be interesting is comparing the difference
# when we include the interaction term from part C. 

# C) 
# Finally, evaluate the hypothesis that mean.growing.season and sd.growing.season interact to 
# synergistically reduce language diversity. 
# The idea is that, in nations with longer average growing season, 
# high variance makes storage and redistribution even more important than it would be otherwise. 
# That way, people can cooperate to preserve and protect windfalls to be used during the droughts. 

m8h4_3_robust <- quap(
  alist(
    z_log_lang.per.cap ~ dstudent(2, mu, sigma),
    mu <- a + bG*z_mean.growing.season + bSD*z_sd.growing.season + bA*z_log_area + bGSD*z_mean.growing.season*z_sd.growing.season,
    a ~ dnorm(0, 0.2),
    bG ~ dnorm(0, 0.5),
    bSD ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    bGSD ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8h4_3_robust)
#        mean   sd  5.5% 94.5%
# a     -0.01 0.08 -0.14  0.12
# bG     0.12 0.10 -0.05  0.29
# bSD   -0.19 0.10 -0.35 -0.03
# bA    -0.05 0.11 -0.23  0.12
# bGSD  -0.19 0.09 -0.33 -0.05
# sigma  0.55 0.07  0.44  0.66

plot(precis(m8h4_3_robust))


# Let's see if this is more predictive at least,
# than the other two:
compare(m8h4_1_robust,m8h4_2_robust,m8h4_3_robust, func = PSIS)
#                PSIS    SE dPSIS  dSE pPSIS weight
# m8h4_3_robust 202.2 16.10   0.0   NA   6.7   0.85
# m8h4_2_robust 206.7 17.66   4.4 6.08   3.9   0.09
# m8h4_1_robust 207.5 16.09   5.3 5.17   4.2   0.06

plot(compare(m8h4_1_robust,m8h4_2_robust,m8h4_3_robust, func = PSIS))

4.4 + c(-1, 1)*6.08*2.6 # -11.408  20.208

# To plot this interaction, 
# I'll try something - making a tryporich (or 4/5 plot ?)

d$z_mean.growing.season_rounded <- round(d$z_mean.growing.season, digits = 0) 
# # DON'T NEED TO DO THIS ^ - USE QUANTILE - SEE JUST UNDER THIS BAD VERSION
table(d$z_mean.growing.season_rounded)

# Adapted the raven pop tryptotrich to get this! - 
# HOWEVER way below this is way better, don't do this INCORRECT way that uses rounding to nearest 
# Let's do some plotting with triptychs:
sd.growing_season_seq <- seq(from =-2, to = 2, length.out=30)
par(mfrow=c(1,5)) # 5 plots in 1 row
for (s in -2:2) {
  idx <- which(d$z_mean.growing.season_rounded==s)
  plot(d$z_sd.growing.season[idx], d$z_log_lang.per.cap[idx], xlim = c(-2,2), ylim = c(-2,2), xlab = 'sd growing season', ylab = 'language diversity', pch=16, col=rangi2)
  mu <- link(m8h4_3_robust, data=data.frame(z_mean.growing.season=s, z_sd.growing.season=-2:2, z_log_area = 0))
  for (i in 1:20) lines(-2:2, mu[i,], col=col.alpha("black", .3))
  abline(h=-2, lty=2) # realistic range line -2
  abline(h=2, lty=2) # realistic range line 2
  mtext(glue('{s}'))
}


# MUCH better way of doing that doesn't round - 
# just use quantile !!! 

test_.seq <- quantile(d$z_mean.growing.season, c(.025, .16, .50, .84, .975))

# Adapted the raven pop tryptotrich to get this - CORRECT way 
# Let's do some plotting with triptychs:
sd.growing_season_seq <- seq(from =-2, to = 2, length.out=5)
par(mfrow=c(1,5)) # 5 plots in 1 row
for (s in test_.seq) {
  idx <- which(d$z_mean.growing.season==s)
  plot(d$z_sd.growing.season[idx], d$z_log_lang.per.cap[idx], xlim = c(-2,2), ylim = c(-2,2), xlab = 'sd growing season', ylab = 'language diversity', pch=16, col=rangi2)
  mu <- link(m8h4_3_robust, data=data.frame(z_mean.growing.season=s, z_sd.growing.season=sd.growing_season_seq, z_log_area = 0))
  for (i in 1:20) lines(-2:2, mu[i,], col=col.alpha("black", .3))
  abline(h=-2, lty=2) # realistic range line -2
  abline(h=2, lty=2) # realistic range line 2
  mtext(glue('{s}'))
}

# Plots appear to support the hypothesis we stated !
# When mean.growing.season is long (z score 2), we see the steepest negative slope 
# as sd.growing.season increases. It's flatter in other graphs,
# indicating a moderating effect. 

# The slope for bGSD was also reliable below 0, suggesting this.

# Thus, we can say the data supports the hypothesis from part C,
# that mean.growing.season and sd.growing.season interact to 
# synergistically reduce language diversity.

plot(precis(m8h4_3_robust))

# So overall, I would say that the data supports the hypotheses in B and C. 
# And also the overall hypothesis, that language diversity is 
# partly a product of food security. 

# His solution in the book for the tryptotrichs - v interesting,
# so I'll put the code here (solution looks the same as mine, but I like that he didn't round values!)

# pull out 10% 50%, and 95% quantiles of z_sd.growing.season
# these values will be used to make the three plots
sd.seq <- quantile(d$z_sd.growing.season, c(.05, .5, .95))

# now loop over the three plots
# draw languages against mean.growingseason in each
mean.seq <- seq(from = -2, to = 2, length.out = 30)
par(mfrow=c(1,3)) # set up plot window for row of 3 plots
for (i in 1:3) {
  sd.val <- sd.seq[i] # select out value for this plot
  new.dat <- data.frame(
    z_mean.growing.season = mean.seq,
    z_sd.growing.season = sd.val,
    z_log_area = 0, row.names = NULL) # added row.names = NULL to remove warnings - results was the same, just sans warnings
  mu <- link(m8h4_3_robust, data = new.dat)
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI)
  
  # fade point color as function of distance from sd.val
  cols <- col.dist(d$z_sd.growing.season, sd.val, 2, 'slateblue')
  
  plot(z_log_lang.per.cap ~ z_mean.growing.season, data = d, col = cols)
  mtext(paste('sd.growing.season = ',round(sd.val,2)), 3, cex=.75)
  lines(mean.seq, mu.mean)
  shade(mu.PI, mean.seq)
}

# And this code will produce the analogous triptych in which sd.growing.season is varied on the horizontal axi

# pull out 10%, 50%, and 95% quantiles of mean.growing.season
top.seq <- quantile(d$z_mean.growing.season, c(0.1,0.5,0.95))

# now loop over the three plots
x.seq <- seq(from = -2, to = 2, length.out = 30)
par(mfrow = c(1,3)) # set up plot window for row of 3 plots
for (i in 1:3) {
  top.val <- top.seq[i] # select out value for this plot
  new.dat <- data.frame(
    z_mean.growing.season = top.val,
    z_sd.growing.season = x.seq,
    z_log_area = 0, row.names = NULL)
  mu <- link(m8h4_3_robust, data=new.dat)
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI)
  
  # fade point color as function of distance from sd.val
  cols <- col.dist(d$z_mean.growing.season, top.val, 5, 'slateblue')
  
  plot(z_log_lang.per.cap ~ z_sd.growing.season, data = d, col = cols)
  mtext(paste('mean.growing.season =',round(top.val,2)), 3, cex=.75)
  lines(x.seq, mu.mean)
  shade(mu.PI, x.seq)
}


# Using my 5 quantiles, as above, in his way:
# And this code will produce the analogous triptych in which sd.growing.season is varied on the horizontal axi

# pull out 10%, 50%, and 95% quantiles of mean.growing.season
top.seq <- quantile(d$z_mean.growing.season, c(0.1,0.5,0.95))
# don't use his top.seq in this one, use my -2,1,0,1,2 quantile (.25, 16, 50, 84, 97.5) from above

# now loop over the three plots
x.seq <- seq(from = -2, to = 2, length.out = 30)
par(mfrow = c(1,5)) # set up plot window for row of 3 plots
for (i in test_.seq) { # test_.seq was defined way above, 5 quantiles
  # top.val <- top.seq[i] # select out value for this plot
  top.val <- i
  new.dat <- data.frame(
    z_mean.growing.season = top.val,
    z_sd.growing.season = x.seq,
    z_log_area = 0, row.names = NULL)
  mu <- link(m8h4_3_robust, data=new.dat)
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI)
  
  # fade point color as function of distance from sd.val
  cols <- col.dist(d$z_mean.growing.season, top.val, 5, 'slateblue')
  
  plot(z_log_lang.per.cap ~ z_sd.growing.season, data = d, col = cols)
  mtext(paste('mean.growing.season =',round(top.val,2)), 3, cex=.75)
  lines(x.seq, mu.mean)
  shade(mu.PI, x.seq)
}


# 8H5:
# Consider the data(Wines2012) data table. 
# These data are expert reatings of 20 different Frech and American wines by 
# 9 different French and American judges. 
# Your goal is to model score, 
# the subjective rating assigned by each judge to each wine. 
# I recommend standardizing it. 
# In this problem, consider only variation among judges and wines. 
# Construct index variables of judge and wine and then use these 
# index variables to construct a linear regression model. 
# Justify your priors. 
# You should end up with 9 judge parameters and 20 wine parameters. 
# How do you interpret the variation among individual judges and individual wines? 
# Do you notice any patterns, just by plotting the differences? 
# Which judges gave the highest/lowest ratings? 
# Which wines were rated worst/best on average? 

# Load data:
data(Wines2012)
d <- Wines2012

# Standardize outcome variable (score):
d$score_standardized <- standardize(d$score)

# Create index variables:
d$jid <- coerce_index(d$judge)
d$wid <- coerce_index(d$wine)

# Justifying priors: 
# since score is standardized, mean is 0. 
# min is -2 and max is 2. 
# if 1 sd for one of them is 1, then
# either 'intercept' can take it to both extremes,
# albeit unlikely, this keeps them in realistic ranges.

m8H5_1 <- quap(
  alist(
    score_standardized ~ dnorm(mu, sigma),
    mu <- a[jid] + w[wid],
    a[jid] ~ dnorm(0, 1),
    w[wid] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8H5_1, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]  -0.31 0.25 -0.71  0.09
# a[2]   0.24 0.25 -0.16  0.64
# a[3]   0.23 0.25 -0.17  0.63
# a[4]  -0.60 0.25 -1.00 -0.20
# a[5]   0.88 0.25  0.48  1.28
# a[6]   0.53 0.25  0.13  0.93
# a[7]   0.15 0.25 -0.25  0.54
# a[8]  -0.73 0.25 -1.13 -0.33
# a[9]  -0.38 0.25 -0.78  0.02
# w[1]   0.14 0.31 -0.35  0.64
# w[2]   0.11 0.31 -0.39  0.60
# w[3]   0.28 0.31 -0.21  0.77
# w[4]   0.57 0.31  0.08  1.07
# w[5]  -0.13 0.31 -0.62  0.36
# w[6]  -0.38 0.31 -0.88  0.11
# w[7]   0.30 0.31 -0.19  0.79
# w[8]   0.28 0.31 -0.21  0.77
# w[9]   0.09 0.31 -0.41  0.58
# w[10]  0.13 0.31 -0.37  0.62
# w[11] -0.01 0.31 -0.50  0.48
# w[12] -0.03 0.31 -0.52  0.46
# w[13] -0.11 0.31 -0.60  0.38
# w[14]  0.01 0.31 -0.48  0.50
# w[15] -0.23 0.31 -0.72  0.27
# w[16] -0.21 0.31 -0.70  0.29
# w[17] -0.15 0.31 -0.64  0.34
# w[18] -0.89 0.31 -1.38 -0.40
# w[19] -0.17 0.31 -0.66  0.32
# w[20]  0.40 0.31 -0.09  0.89
# sigma  0.78 0.04  0.72  0.85

# I want to use PSIS pareto k to check for outliers:
PSIS(m8H5_1)
# Some Pareto k values are high (>0.5). Set pointwise=TRUE to inspect individual points.
# PSIS      lppd  penalty  std_err
# 1 486.2421 -243.1211 31.13978 22.24617

plot(PSIS(m8H5_1, pointwise = TRUE)$k) 
# So a few are pretty high, all the way up to .8, which is too high.

# For this reason, I'll do a robust regression instead:
m8H5_1_robust <- quap(
  alist(
    score_standardized ~ dstudent(2, mu, sigma),
    mu <- a[jid] + w[wid],
    a[jid] ~ dnorm(0, 1),
    w[wid] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8H5_1_robust, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]  -0.26 0.24 -0.65  0.13
# a[2]   0.47 0.23  0.10  0.84
# a[3]   0.18 0.23 -0.19  0.56
# a[4]  -0.48 0.24 -0.87 -0.09
# a[5]   0.90 0.23  0.53  1.28
# a[6]   0.61 0.23  0.24  0.98
# a[7]   0.25 0.33 -0.28  0.78
# a[8]  -0.94 0.26 -1.35 -0.52
# a[9]  -0.32 0.26 -0.73  0.09
# w[1]   0.02 0.34 -0.52  0.56
# w[2]   0.29 0.27 -0.14  0.73
# w[3]   0.14 0.25 -0.27  0.55
# w[4]   0.82 0.31  0.32  1.31
# w[5]  -0.03 0.30 -0.50  0.45
# w[6]   0.05 0.36 -0.52  0.62
# w[7]   0.74 0.27  0.30  1.17
# w[8]   0.10 0.34 -0.44  0.65
# w[9]   0.15 0.35 -0.41  0.70
# w[10] -0.01 0.27 -0.45  0.42
# w[11] -0.08 0.29 -0.55  0.39
# w[12] -0.01 0.34 -0.55  0.54
# w[13] -0.23 0.31 -0.74  0.27
# w[14] -0.03 0.27 -0.46  0.40
# w[15] -0.31 0.29 -0.78  0.16
# w[16] -0.06 0.27 -0.49  0.37
# w[17] -0.36 0.33 -0.89  0.17
# w[18] -1.03 0.30 -1.52 -0.55
# w[19] -0.23 0.28 -0.68  0.21
# w[20]  0.49 0.26  0.07  0.91
# sigma  0.52 0.04  0.45  0.59

plot(PSIS(m8H5_1_robust, pointwise = TRUE)$k)
# Much better. All are less than .5.

# best wine (4) vs worst wine (18)
set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_w4_w18 <- post$w[,4] - post$w[,18]
PI(diff_w4_w18)
#       5%      94% 
# 1.292769 2.419451 

plot(precis(m8H5_1_robust, depth = 2))

# diff between judge 5 (best) and judge 8 (worst)
set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_j5_j8 <- post$a[,5] - post$a[,8]
PI(diff_j5_j8)
#         5%      94% 
#   1.460974 2.220394 

# diff between judge 5 and 6 - seeing if it's above 0 
set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_j5_j6 <- post$a[,5] - post$a[,6]
PI(diff_j5_j6)
#            5%         94% 
#   -0.04474011  0.62476043 
# Cant say the 89% CI exludes 0 

# diff between judge 5 and 2 - seeing if it's above 0 
set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_j5_j2 <- post$a[,5] - post$a[,2]
PI(diff_j5_j2)
#          5%       94% 
#   0.1113467 0.7550159 
# Yes! So judge 5 is highly likely better than all judges except 6 

# diff between judge 4 and judge 8 
set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_j4_j8 <- post$a[,4] - post$a[,8]
PI(diff_j4_j8)
# 5%        94% 
#   0.07679541 0.83054857 

# diff between judge 9 and judge 8 
set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_j9_j8 <- post$a[,9] - post$a[,8]
PI(diff_j9_j8)
# 5%      94% 
#   0.221349 1.012850 

# diff between wine 17 and wine 18
set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_w17_w18 <- post$w[,17] - post$w[,18]
PI(diff_w17_w18)
# 5%        94% 
#   0.07778373 1.27337846 

set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_w4_w20 <- post$w[,4] - post$w[,20]
PI(diff_w4_w20)
# 5%        94% 
#   -0.1720271  0.8240299 

set.seed(1)
post <- extract.samples(m8H5_1_robust)
diff_w4_w7 <- post$w[,4] - post$w[,2]
PI(diff_w4_w7)

# Explanation for 8H5:
# Variation is huge. 
# Pretty much all parameters have credibility intervals
# that span between .5 and 1, 
# and the maximum range of all possible values is basically only 4. 
# (-2,2) since outcome is in z score form. 
# So, there's lot's of variability for each parameter.
# However, there are still things that we can say 
# with relative confidence just from the plot. (and post sample differences) 
# For example, 
# Judge 8 gave the lowest scores 
# (89% CI of diff with all judges doesn't include 0), 
# and Judge 5 gave the highest scores 
# (89% CI of diff doesn't include 0 for all other judges except judge 6). 
# Likewise, wine 18 was consistently the worst reviewed wine,
# (89% CI of diff did not include 0 vs all other wines),
# while wines 4 and 7 were 
# reliably better reviewed than all wines except 20 and 2.

# Worst judge
subset(d, subset = d$jid == 8)[1,1] # Robert Hodgson
# Best judge
subset(d, subset = d$jid == 5)[1,1] # John Foy

# Worst wine
subset(d, subset = d$wid == 18)[1,3] # I2
# Best wines
subset(d, subset = d$wid == 4)[1,3] # B2
subset(d, subset = d$wid == 7)[1,3] # D1

# One thing I should have mentioned in my answer that I didn't,
# is that the judges had more variability, 
# as most of the wines MAPs were close to 0 (average), 
# as opposed to judges which were not typically.


# 8H6: 
# Now consider three features of the wines and judges:
# 1) flight: Whether the wine is red or white.
# 2) wine.amer: Indicator variable for American wines.
# 3) judge.amer: Indicator variable for American judges.
# Use indicator or index variables to model the influence of these features on the scores. 
# Omit the individual judge and wine index variables from Problem 1. 
# Do not include interaction effects yet. 
# Again justify your priors. 
# What do you conclude about the differences among the wines and judges? 
# Try to relate the results to the inferences in the previous problem.

# Creating more index variables:
d$fid <- coerce_index(d$flight)
d$w_a_id <- d$wine.amer + 1
d$j_a_id <- d$judge.amer + 1

# Justify priors 
# Again, since outcome (score) is standardized 
# (-2 to 2 should interval should include 95% of all values),
# if only one of the parameters accounts for the entire range,
# it should be able to go from -2 to 2. 
# by making the sigma for the normal distribuiton of any 
# one of those parameters 1, the 95% interval is -2, 2. 

m8H6_1 <- quap(
  alist(
    score_standardized ~ dnorm(mu, sigma),
    mu <- a[fid] + w[w_a_id] + j[j_a_id],
    a[fid] ~ dnorm(0,1),
    w[w_a_id] ~ dnorm(0,1),
    j[j_a_id] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)

# Checking if PSIS throws warnings for outliers due to pareto k values:
PSIS(m8H6_1)
# Nice, no errors! 
# To doublecheck, let's plot: 
plot(PSIS(m8H6_1, pointwise = TRUE)$k)
# Looks very good! Well below .5 (all below .3)

precis(m8H6_1, depth = 2)
#        mean   sd  5.5% 94.5%
# a[1]   0.00 0.58 -0.93  0.93
# a[2]   0.00 0.58 -0.93  0.93
# w[1]   0.10 0.58 -0.84  1.03
# w[2]  -0.09 0.58 -1.02  0.84
# j[1]  -0.12 0.58 -1.05  0.81
# j[2]   0.13 0.58 -0.81  1.06
# sigma  0.98 0.05  0.90  1.06

plot(precis(m8H6_1, depth = 2))

# So this tells basically nothing. CI intervals are HUUUGE, 
# like range of 2 (vs full range of 4 for outcome variable).
# Low variability in means, all are essentially the same.
# This looks like it tells us almost nothing.


set.seed(1)
post <- extract.samples(m8H6_1)
diff_a1_a2 <- post$a[,1] - post$a[,2]
PI(diff_a1_a2)
#           5%        94% 
#   -0.2412305  0.2317066 

set.seed(1)
post <- extract.samples(m8H6_1)
diff_w1_w2 <- post$w[,1] - post$w[,2]
PI(diff_w1_w2)
#            5%         94% 
#   -0.05093579  0.42897460 

set.seed(1)
post <- extract.samples(m8H6_1)
diff_j1_j2 <- post$j[,1] - post$j[,2]
PI(diff_j1_j2)
#            5%         94% 
#   -0.48067796 -0.01399032

# Cannot say any of the differences' CI excludes 0.
# All of the intervals have 0 inside of them. 

# Testing compare the PSIS for both models, 
# just to confirm my hunch that m8H5_robust is WAY better than m8H5_1:
compare(m8H5_1_robust, m8H6_1, func = PSIS)
#                PSIS    SE dPSIS   dSE pPSIS weight
# m8H5_1_robust 504.1 24.33   0.0    NA  37.4      1
# m8H6_1        515.8 17.90  11.7 21.61   5.1      0

# 99% CI of diff between models
11.7 + c(-1,1)*21.61*2.6 # = -44.486  67.886

# 97% CI of diff between models
11.7 + c(-1,1)*21.61*1.881 # = -28.94841  52.34841

# 89% CI of diff between models
11.7 + c(-1,1)*21.61*1.227 # = -14.81547  38.21547

# So we cannot actually say that one model is better than the other, reliable.
# Very interesting. 


# 8H7:
# Now consider two-way interaction among the three features. 
# You should end up with three different interaction terms in your model. 
# They will be easier to build, if you use indicator variables. 
# Again justify your priors. Explain what each interaction means. 
# Be sure to interpret the model's predictions on the outcome scale 
# (mu, the expected score), not on the scale of individual parameters. 
# You can use link to help with this, 
# or just use your knowledge of the linear model instead. 
# What do you conclude about the features and the scores? 
# Can you relate the results of your model(s) to the individual judge 
# and wine inferences from 8H5? 

# What are the three featuers? 

# flight, judge.amer, wine.amer

# Using indicator (dummy) variables: 
# view(d)
table(d$flight)
d$flight_dummy <- ifelse(d$flight == 'white', 0, 1) # white = 0, red = 1


# Justifying priors: 

# Because these are dummy variables,
# when one is 1, the slope is "turned on",
# and when it's 0, the slope is turned off. 
# Thus, a will be mu when all are in their 0 state - 
# white wine, not american jude, not american wine,
# bc the score is standardized, 0 is reasonable for the mean.
# Standard deviation of a should be 1 now I think,
# so that there can still be full range (-2:2) with 0 0 0 indicators. 

# Regarding the slopes - same thing.
# When score is at its extreme, if only one 
# of the slopes is turned 'on', we want it to be normal(0,1),
# so that it could be -2:2 (slope * 1 means outcome is just the slope).

# For interaction slopes,
# in order for there to be an interaction,
# two need to be set to 1. 
# But, previous point still stands - in the (off chance)
# that the slopes for both the parameters by themselves are 0,
# we need to allow for all of the change being due to 
# that single interaction slope.
# this means the same thing, normal(0,1).

m8H7_1 <- quap(
  alist(
    score_standardized ~ dnorm(mu, sigma),
    mu <- a + 
      bF*flight_dummy + 
      bJA*judge.amer + 
      bWA*wine.amer + 
      bFJA*flight_dummy*judge.amer + 
      bFWA*flight_dummy*wine.amer + 
      bJAWA*judge.amer*wine.amer,
    a ~ dnorm(0, 1),
    bF ~ dnorm(0,1),
    bJA ~ dnorm(0,1),
    bWA ~ dnorm(0,1),
    bFJA ~ dnorm(0,1),
    bFWA ~ dnorm(0,1),
    bJAWA ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8H7_1)
#        mean   sd  5.5% 94.5%
# a     -0.18 0.20 -0.50  0.15
# bF     0.27 0.26 -0.15  0.68
# bJA    0.27 0.25 -0.14  0.67
# bWA    0.11 0.25 -0.28  0.51
# bFJA   0.06 0.27 -0.38  0.49
# bFWA  -0.52 0.28 -0.96 -0.08
# bJAWA -0.09 0.28 -0.53  0.35
# sigma  0.97 0.05  0.89  1.05


# FWA is interesting - let's examine that first

# So if the flight is red and its american, score is reliable less (by.52 standard deviations of outcome). 

plot(precis(m8H7_1))

# I bet wine 18 (worst) was an american red wine
colnames(d)
view(subset(d, subset = d$wid == 18))

# It was !

# Now explaining each interaction:

# FJA:
# If flight is red and judge is american, 
# not too much effect on outcome (.05, CI: -.38, .49)

# FWA:
# If  flight is red and american, 
# score is reliable less (by.52 standard deviations of outcome). 
# This in the only slope where 
# the credibility interval excludes 0. 

# JAWA:
# judge is american and wine is american,
# not too much effect on outcome 
# (-.09, CI: -.53, .35)


dummy_8H6 <- quap(
  alist(
    score_standardized ~ dnorm(mu, sigma),
    mu <- a + 
      bF*flight_dummy + 
      bJA*judge.amer + 
      bWA*wine.amer,
    a ~ dnorm(0, 1),
    bF ~ dnorm(0,1),
    bJA ~ dnorm(0,1),
    bWA ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d
)

precis(dummy_8H6)

plot(precis(dummy_8H6))


mu <- link(m8H7_1, data=data.frame(flight_dummy = 1, judge.amer = 1, wine.amer = 1))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
mu.PI[,1]
# 5%        94% 
#   -0.3449187  0.1850287 

mu <- link(m8H7_1, data=data.frame(flight_dummy = 1, judge.amer = 0, wine.amer = 1))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
mu.PI[,1]
#             5%          94% 
#   -0.618376069 -0.009783011 

# Basically reflects what makes sense - 
# just turning on slopes and reasoning what would happen, with sum of interactions that are on and slopes that are on.
# For example, if flight is red and wine is american, the score is higher if judge is american than it is if not. 
# which makes sense with the linear model. 



