## HW 2 Count Data ##

## NMES1988 - Medical Care 1987-1988 ##
install.packages("AER")
data(NMES1988, package = "AER")
library(dplyr)
install.packages("magrittr") 
library(magrittr)
NMES1988 <- NMES1988 %>% 
  select(-c("nvisits","ovisits","novisits","emergency","hospital"))
head(NMES1988)
colnames(NMES1988)

#histogram of # of visits. Use "Breaks" to obtain plot which describes variable well. 
hist(NMES1988$visits,
     main = "Histogram of Physician Office Visits",
     xlab = "Vists",
     col = "pink",
     xlim = c(0,80),
     breaks = 40)

axis(1, at= seq(0,20,5))
#CDF?
plot(ecdf(NMES1988$visits))
axis(1, at= seq(0,20,5))
## Both the histogram and the summary show that the number of visits is severely right skewed.
## This is further seen when plotting the CDF.

# b. show the relationship btwn vists & potential predictor variables health and medicaid.
library(ggplot2)
ggplot(NMES1988, aes(x=health, y=visits)) + geom_point()
ggplot(NMES1988, aes(x=medicaid, y=visits)) + geom_point()
head(table(NMES1988[NMES1988$visits <10,]$visits, NMES1988[NMES1988$visits <10,]$health))
head(table(NMES1988[NMES1988$visits <10,]$visits, NMES1988[NMES1988$visits <10,]$medicaid))
NMES1988[NMES1988$visits <10,]
# Taking all visits values into account, it appears that
# those with average health & no medicaid have more visits.
# But this appears to be due to the fact that these are the largest 
#sub-sample groups. Calculating the proportions based on the size of the sub groups
# seems to be more revealing
# ex:
head(table(NMES1988[NMES1988$visits <10,]$visits, NMES1988[NMES1988$visits <10,]$health))
summary(NMES1988[NMES1988$visits <10,]$health)
#summary(NMES1988[NMES1988$visits <10,]$health)[1] = 355
#summary(NMES1988[NMES1988$visits <10,]$health)[2] = 542
#summary(NMES1988[NMES1988$visits <10,]$health)[3] = 2902
#poor proportion w/ 0 visits: 
(59 +32+29)/355 # = 34% Poor
(397+352+542)/2902 # = 44% Average
(52+47+82)/320 # = 57% Excellent
#For patients with < 10 visits, these percentages show the proportion of
#patients, in each group, that had 0,1, or 2 visits. 
# Interpretation: Of those with Excellent health, over half the group has 0-2 doctor visits over the time period
# Conversely, for those in poor health, only 34% of them had 0-2 visits, meaning that 66% had between 3 and 9 visits in the given time period
# These results are as expected

# c. Build a Poisson reg model w/ visits as response, predicted by all vars.
# assess significance and quality of fit via the devaince. 

poisson_model <- glm(visits ~ ., data=NMES1988, family = poisson)
summary(poisson_model)
# Almost everything is significant, but the deviance is really high
# E.g. for health, we see significance in the direction we'd expect; 
# poor has an increasing relationship w/ # of visits, excellent has a decreasing relationship
exp(.287)
exp(-.386)

pchisq(summary(poisson_model$deviance, poisson_model$df.residual, lower.tail=FALSE))# unsure why this code isn't working
# Nonetheless, pretty sure the fit is poor & model is significantly different than the saturated model,
# especially given that the null deviance is similar to the model's residual deviance

# d. Make a QQ plot of the residuals. Notice any outliers that deviate significantly from the trend in the plot?
install.packages("DALEX")
install.packages("auditor")
library(DALEX)
library(auditor)
poisson_plot <- DALEX::explain(poisson_model, data = NMES1988, y = NMES1988$visits,verbose = FALSE)
poisson_plot_HalfNorm <- model_halfnormal(poisson_plot)

plot_halfnormal(poisson_plot_HalfNorm, quantiles = TRUE)
# This plot makes the model appear to be a terrible fit.
# Most values look to be notable outliers when looking at this plot.

# e. compute the mean and variance of the visits var within each of the health variable
# comment on relationship and viability for Poisson to fit this data

poor <- NMES1988[NMES1988$health == "poor",]
avg <- NMES1988[NMES1988$health == "average",]
excellent <- NMES1988[NMES1988$health == "excellent",]

mean(poor$visits) #8.89
var(poor$visits) #83.13

mean(avg$visits) #5.5
var(avg$visits) #39.81

mean(excellent$visits) #3.43
var(excellent$visits) #23.61

# More efficient way
NMES1988 %>% 
  group_by(health) %>% 
  summarise(Mean = mean(visits),
            Var = var(visits)) #%>% DisplayTable
# Data are very clearly overdispersed, resulting in greater significance than reality
# and a poor model fit.

#f. 



