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
p_Norway # (1-.76) = .24 odds of moving from too little/about right to too much decrease by .24 in norway vs australia

p_Sweden <- exp(Sweden)
p_Sweden # (1-.58) = .42 odds of moving from too little/about right to too much decrease by .42 in sweden vs australia 

p_USA <- exp(USA)
p_USA # 1.9x more likely to go from too little/about right to too much in USA vs australia.
