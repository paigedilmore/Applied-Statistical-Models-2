## HW 4 Advanced GLMs##

data(cornnit, package="faraway")
head(cornnit)

# a. Scatterplot
plot(cornnit$yield ~ cornnit$nitrogen)
plot(log(cornnit$yield) ~ cornnit$nitrogen)
# If I were to use LM, I would try log transforming at least the response(yield)

# b.
lm_plot <- lm(log(yield)~ (nitrogen^2), data=cornnit, family = gaussian)
plot(lm_plot) 
summary(lm_plot_2)
# Looking at the QQ plot and predicted vs resids, pts 21, 25 and 37 look problematic for the LM fit

# c. removing 21, 25, 27
lm_plot_2 <- lm(log(yield)~ (nitrogen^2), data=cornnit[-c(21,25,37),], family = gaussian)
plot(lm_plot_2) 
# The QQ plot looks substantially better, but the adj R Sq values are only slightly better

# d. Fit Gamma w/ log link with fitted curves for both models
gamma_mod <- glm(yield ~ (nitrogen^2),
                 family = Gamma(link=log),
                 data = cornnit[-c(21,25,37),])
data <- cornnit[-c(21,25,37),]
data$normal_pred <- predict(lm_plot_2, type="response")
data$gamma_pred <-  predict(gamma_mod, type="response")
data2<-data %>% gather(key = "type",value = "yield",yield, normal_pred, gamma_pred)
ggplot(data2) +
  geom_point(aes(nitrogen, yield, col=type), alpha=0.8)

# e. Fit inverse Gaussian and add to previous plot

inverse_mod <- glm(yield ~ (nitrogen^2),
                 family = inverse.gaussian(link="1/mu^2"),
                 data = cornnit[-c(21,25,37),])
data$inv_pred <- predict(inverse_mod, type="response")
data3<-data %>% gather(key = "type",value = "yield",yield, normal_pred, gamma_pred,inv_pred)
ggplot(data3) +
  geom_point(aes(nitrogen, yield, col=type), alpha=0.8)

# Fits extremely similar to Gamma
plot(data$gamma_pred, data$inv_pred, col=c(1,5))

# f. 95% CI when Nitrogen = 200
nitrogen<- 200
nitro <- data.frame(nitrogen)
se<- predict(gamma_mod, newdata = nitro, type="link" , se.fit = TRUE)$se.fit
nitro$g_upper <- nitro$gamma+(1.96*se)
nitro$g_lower <- nitro$gamma-(1.96*se)

# inverse model
nitro2 <- data.frame(nitrogen)
se2<- predict(inverse_mod, newdata = nitro2, type="link" , se.fit = TRUE)$se.fit
nitro2$inverse <- predict(inverse_mod, newdata = nitro2, type="link" )
nitro2$inv_upper <- nitro2$inverse+(1.96*se2)
nitro2$inv_lower <- nitro2$inverse-(1.96*se2)
nitro2

# Feed through inverse
exp(nitro$g_upper)
## 95% CI (135.3375,150.7321)

(1/nitro2$inv_upper)^.5
## 95% CI (133.61,149.27)

## Very similar, but the sign switches with the inverse function switch the lower and upper bounds

## Problem B ## 
data(gpsleep, package = "GLMsData")

# a. Scatter plot 
plot(Sleep ~ Dose, data = gpsleep)
# There seem to be discrete Dose groups
# Generally, positive linear relationship
# No 0 values for Dose

# b. histogram of Sleep variable
hist(gpsleep$Sleep)
summary(gpsleep$Sleep)
# Right skewed; Compound Poisson-Gamma. 
# "Suitable for positive and continuous data with exact zeros"

# c. Computing the proportion of zeros
vals <- unique(gpsleep$Dose)
vals
props <- list()

for (i in vals){
  tot <- gpsleep[gpsleep$Dose ==i,]%>% tally()
  tst <- gpsleep[gpsleep$Dose == i,]%>%count(Sleep)
  if (as.integer(tst[1,1]) == 0)
  {
    zeros <- tst[1,2]/tot 
  } else
  {
    zeros <- 0 
  }
  props <- c(props, zeros)
  
}
final <- cbind(vals,props)
final # That was a lot of work lol. 
# the proportion of "0" sleep decreases as Dose increases. The 0 value
# for Dose ==2 will cause an issue for Gamma, which needs strictly positive, continuous data.

# d. For records w/ positive sleep only, plot sleep against Dose

plot(Sleep ~ Dose, data = gpsleep[gpsleep$Sleep >0,])
# Looks like a logarithmic relationship

# e. Attempt the Gamma
gamma_att <- glm(Sleep ~ Dose, data=gpsleep, 
                 family=Gamma)
# per expected:
# Error: "non-positive values not allowed for the 'gamma' family" 

#f. Add a small constant and experiment
constant <- runif(5,min=0.001, max=0.1)
constant # random values

for (i in constant){
  gpsleep$SleepNew <- gpsleep$Sleep+ i
  gams <- glm(SleepNew ~ Dose, data = gpsleep,
              family=Gamma)
  print(i)
  print(summary(gams))
}
# Coefficients: The coefficients remain nearly the sample for these 5 randomly generated
# constants added to Sleep. Approx. 0.29 for the intercept, and -0.76 for the Dose.
# The p value for Dose is stable around 0.016

#Deviance: The deviance shows more variation, going from 2-3x the df, depending on what 
# constant value is added

#Dispersion: The Dispersion paramter appears relatively stable around 0.75

# When we exp(coef()), it seems like Dose moves in the incorrect direction.
# According to our plots, an increase in dose should increase sleep.
# The <1 log odds ratio makes me think it could be interpreted as the odds
# of being in the zero group decrease as Dose increases... If that's incorrect,
# then it would seem the adding a constant is just plain bad practice.

# g. Estimate p for use in Tweedie using log link. What does p say about the data?
install.packages("statmod")
library(statmod)
install.packages("tweedie")
library(tweedie)

TW_p_log <- tweedie.profile(Sleep ~ log(Dose), 
                            p.vec = seq(from=1,to=5,by=0.05),
                            link.power = 0, # log link 
                            data=gpsleep)
TW_p_log$p.max
# The output: When the response variable contains exact zeros, all values of p must be between 1 and 2; other values have been removed.
#1.05 1.1 1.15 1.2 1.25 1.3 1.35 1.4 1.45 1.5 1.55 1.6 1.65 1.7 1.75 1.8 1.85 1.9 1.95 
#...................Done.

# This demonstrates that the Compound Poisson-Gamma model is approriate

# h. Fit a tweedie glm to the data using p
TW_p_log$p.max
Tweedie_log <- glm(Sleep ~ log(Dose)
              , data=gpsleep
              , family=tweedie(var.power=TW_p_log$p.max
                                      , link.power=0))
summary(Tweedie_log)
# The goodness of fit doesn't seem great, per residual deviance
exp(log(1.034))
# Increase in Dose increases Sleep by 1.034.