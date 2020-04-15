## HW 6 ##
##GLMMs##
attach(salamander)
#head(salamander)
#salamander$Male <- as.factor(salamander$Male)
#salamander$Female <- as.factor(salamander$Female)

## a. Proportion of successful matings for each cross
library(dplyr)
salamander%>%
  group_by(Cross)%>%
  summarize(prop_success = mean(Mate))

# 2/3 of the time, same-population matings are successful

## b. Fit a generalized linear model to the data, 
# using the cross category to predict the success of 
# mating without using any random eﬀects. Are there 
# any signiﬁcant diﬀerences between levels?

library("MASS")
install.packages("MCMCglmm")
library(MCMCglmm)
library(lme4)
head(salamander)

linmod <- glm(Mate ~ Cross, data=salamander, family=binomial())
summary(linmod)
exp(coef(linmod))

## Cross WR is significantly different; successful mating occur 87% less often; a factor of .133 
# between a whiteside female and a rough butt male

# c. Fit using the PQL approach to linearization... males as RE, cross FE;
# then again with female as RE, cross as FE

pql_re_male <- glmmPQL(Mate ~ Cross, random= ~1 | Male, data=salamander, 
                       family=binomial())
pql_re_female <- glmmPQL(Mate ~ Cross, random= ~1 | Female, data=salamander, 
                         family=binomial()) #bias results due to binomial nature

# fixed effect coefficients remain the nearly identical, regardless 
# of RE being male or female.

# d. Use numerical integration approach (this approach approx. likelihood directly; often
# capturing more variability in RE)
num_int <- glmer(Mate ~ Cross + (1|Male) +(1|Female), data=salamander,
                 family = binomial())

summary(num_int)$varcor #More variability from the male/female random effects is captured with this method
nlme::VarCorr(pql_re_female)
summary(pql_re_female)
exp(0.9776212) #lrg variation in both female (~2.65)
exp(0.9298801) #and male (~2.53) REs

summary(num_int)
#summary(pql_re_female) #why are these standard errors smaller, despite more variation being captured in the numerical integration model? Biases for 0/1 data?
exp(summary(num_int)$coefficients[,1])
# crossWR has mating success factor of 0.05; 95% less likely to be successful from baseline RR, when controlling for variability in male/female classes

# e Comparing coeff estimates btwn models by looking at the standard errors
lm_st_err<- summary(linmod)$coefficients[,2]
pqlf_st_err <- summary(pql_re_female)$tTable[,2]
pqlm_st_err <- summary(pql_re_male)$tTable[,2]
num_int_st_err <- summary(num_int)$coefficients[,2]

lm_st_err
pqlf_st_err
pqlm_st_err
num_int_st_err #standard errors of the estimates are, in fact, higher for numerical integration than the glm with no random effect
summary(num_int)$varcor
exp(1.08)
# f. Fit with Bayesian using male and female indicators as Re
bayes_mate_base <- MCMCglmm(Mate ~ Cross, random=~Male+Female, 
                       data=salamander,
                       family='categorical',verbose=FALSE)
summary(bayes_mate_base) #Wild changes in the confidence intervals surrounding the FE's make this sampling look unstable
apply(bayes_mate_base$VCV,2,sd) #units value is higher than male or female

library(lattice)
xyplot(as.mcmc(bayes_mate_base$Sol), layout=c(2,2)) #burnin for Cross WR is extremely long compared to other crosses
xyplot(as.mcmc(bayes_mate_base$VCV), layout=c(3,1)) # 9000 iteration fix instability in the sex RE

bayes_mate_adj <- MCMCglmm(Mate ~ Cross, random=~Male+Female, 
                            data=salamander,
                            family='categorical',
                            nitt=30000, burnin=20000,
                           prior=list(G=list(list(V=1,nu=100),
                                             list(V=1,nu=100)),
                                      R = list(V=1,nu=100)),
                           verbose=FALSE)

#bayes_mate_adj2 <- MCMCglmm(Mate ~ Cross, random=~Male+Female, 
                           #data=salamander,
                           #family='categorical',
                           #nitt=30000, burnin=20000,
                           #prior=list(
                            # G=list(G1=list(V=1, nu=.1))),
                             #R = list(V = 0.5, nu = 0.002, fix = FALSE)),
                           #verbose=FALSE)

xyplot(as.mcmc(bayes_mate_adj$Sol), layout=c(2,2)) #trace plots look good; variance components look okay... 
xyplot(as.mcmc(bayes_mate_adj$VCV), layout=c(3,1))

#summary(bayes_mate_base)
#exp(-4.14)

# Comparing coefficients
exp(summary(linmod)$coefficients[,1][3])
exp(summary(pql_re_female)$tTable[,1][3])
exp(summary(pql_re_male)$tTable[,1][3])
exp(summary(num_int)$coefficients[,1][3])
#summary(bayes_mate_adj)#$coefficients
exp(-3.1454) #CrossWR from Bayes adjusted model
