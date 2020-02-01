#MULTINOMIAL REGRESSION
install.packages("carData")
data(WVS, package = "carData")
summary(WVS)
library(ggplot2)
library(tidyr)
library(dplyr)
#a. 
#by Age group
Age_Grp=cut_number(WVS$age,4)
table(WVS$poverty, Age_Grp)
Plot_DF <- WVS %>% 
  mutate(Age_Grp=cut_number(age,4)) %>% 
  group_by(Age_Grp, poverty) %>% 
  summarise(count=n()) %>% group_by(Age_Grp) %>% 
  mutate(etotal=sum(count), proportion=count/etotal) 
Age_Plot <- ggplot(Plot_DF, aes(x=Age_Grp, y=proportion,
                                group=poverty, linetype=poverty))+ 
  geom_line()
Age_Plot
#by Country
table(WVS$poverty, WVS$country)
#interesting how Norway and Sweden, countries with heavily involved governments, nonetheless 
# mostly feel that it's too little

#b. fitting a proportional odds model. Use drop1()
library(MASS)
WVS$poverty_2 <- ordered(WVS$poverty, 
                         levels=c("Too Little","About Right","Too Much"))
head(WVS)
PropOdds_model <- polr(poverty_2 ~religion + degree +
                         country + age + gender ,data=WVS)
summary(PropOdds_model)
drop1(PropOdds_model, test="Chisq")
step(PropOdds_model)
# removing religion and degree from model, even though they may add a little value
PropOdds_final <- polr(poverty_2 ~
                         country + age + gender ,data=WVS)
summary(PropOdds_final)
# for country, Australia = baseline (alphabetical)
# for gender, female = baseline (alphabetical)

# c. interpret the values of the intercepts (theta j's)
# shifting the age for interpretation
PropOdds_final_shifted <- polr(poverty_2 ~
                                  country + I(age-min(age)) + gender ,data=WVS)
Summary <- summary(PropOdds_final_shifted)
Summary$coefficients[1,1]
# thera_too_little = 0.37 represents the threshold for an 18 year old to cross
# from the opinion that the gov't does "too little" to the opinion that the gov't 
# performs "About right" when it comes to poverty

# similarly, the 2.17 represents the threshold for an 18 yr old to go from 
# "About right" to "too much" .

## these values are like 'cut points' on the underlying/latent/assumed distribution

# d. Interpret value for country coefficients
Norway <- Summary$coefficients[1,1]
Sweden <- Summary$coefficients[2,1]
USA <- Summary$coefficients[3,1]
# probability of having the opinion "Too Much" in Norway vs Australia (baseline):
p_Norway <- exp(Norway)
p_Norway # (1-.76) = .24 odds of moving from too little/about right to too much (OR too little to about right/too much) decrease by .24 in norway vs australia

p_Sweden <- exp(Sweden)
p_Sweden # (1-.58) = .42 odds of moving from too little/about right to too much (OR too little to about right/too much) decrease by .42 in sweden vs australia 

p_USA <- exp(USA)
p_USA # 1.9x more likely to go from too little/about right to too much (OR too little to about right/too much) in USA vs australia.

# e. re fit using the VGAM package, relaxing the proportional odds assumption
install.packages("VGAM")
library(VGAM)

NoPropOdds_final <- vglm(poverty_2 ~
                                 country + I(age-min(age)) + gender ,data=WVS,
                         family = cumulative(parallel = FALSE))
Summary_noProp <- summary(NoPropOdds_final)
Summary_noProp
Summary_noProp$coefficients
# Norway: moving from too little to about right/too much group vs Australia:
exp(-.0745) # 1-.93 = odds of moving from too little to about right/too much decrease by .07 when going from australia to norway
exp(-1.73) # 1-.18 = odds of moving from too little to about right/too much decrease by .82 when moving from australia to norway
#Sweden
exp(-.3866) # 1-.68 = odds of moving from too little to about right/too much decrease by .32 when going from australia to sweden
exp(-2.00) # 1-.14 = odds of moving from too little/about right to too much decrease by .86 whe going from australia to sweden
#USA
exp(.39) # odds of going from too little to about right/too much increases by 1.48x when going from australia to USA
exp(.90) #odds of going from too little/about right to too much increases by 2.46x when going from australia to USA


# f. (hopeful) exploration of interactions
head(WVS)
exp_USA <- WVS[WVS$country == "USA",]
exp_Sweden <- WVS[WVS$country == "Sweden",]
table(exp_USA$religion, exp_USA$poverty)
table(exp_Sweden$religion, exp_Sweden$poverty)             
unique(WVS$country)

table(exp_USA$degree, exp_USA$poverty) #in the US, not having a degree and thinking the gov't. does too much is almost 3x higher than those that have a degree. 
table(exp_USA$gender, exp_USA$poverty) #there appears to be little effect in the US in regards to gender

NoPropOdds_expUSA <- vglm(poverty_2 ~
                           religion ,data=exp_USA,
                         family = cumulative(parallel = FALSE))
Summary_expUSA <- summary(NoPropOdds_expUSA)
Summary_expUSA
exp(.55)
exp(.48) #for people in the USA:
        # the odds of moving from a too little/about right to too much gov't help increase by 1.62 to 1.73 times when religion = yes vs religion = no.

NoPropOdds_expSweden <- vglm(poverty_2 ~
                            religion ,data=exp_Sweden,
                          family = cumulative(parallel = FALSE))
Summary_expSweden <- summary(NoPropOdds_expSweden)
Summary_expSweden
 # in sweden, religion = Yes = insignificant
exp(-2.4) #for people in the Sweden:
# the odds of moving from a too little/about right to too much gov't help decrease by .91 for religious people.

table(exp_USA$religion, exp_USA$poverty)  
# Comparing religious people in USA vs Sweden is interesting. In the US, religious people are much
# more likely to go from too little/about right to too much gov't intervention. Whereas in Sweden the odds 
# of going from too little/about right to too much are drastically lowered for religious people.
# This seems to indicate that either the religious practices are very different, 
# or that religion is a proxy for some other feature that distinguishes these two groups of people.