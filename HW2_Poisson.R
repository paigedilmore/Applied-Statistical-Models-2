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

#f. Fit a negative binomial model vists ~ . .. Comment on significance and quality of fit
neg_bin_model <- MASS::glm.nb(visits ~ ., NMES1988)
neg_bin_modelglm <- MASS :: glm.convert(neg_bin_model)
summary(neg_bin_modelglm, dispersion =1)
pchisq(neg_bin_modelglm$deviance, neg_bin_modelglm$df.residual,
       lower.tail = FALSE)
summary(poisson_model)
# With the negative binomial model, fewer variables are significant, but their directions all make sense
# The deviance improved soooo much. The regular Poisson had a deviance of over 23000, and the NB poisson has
# a deviance just over 5,000 on 4389 DFs. 

#g. Chronic:
chronic = summary(neg_bin_modelglm)$coefficients[4]
exp(chronic) # = 1.21 = your # of visits increases by 1.21 with each additional unit increase in Chronic

# h. plot residuals vs fitted values
nb_preds <- predict(neg_bin_modelglm, type="response")
residuals(neg_bin_modelglm)
preds_res <- cbind(nb_preds, residuals(neg_bin_modelglm))
plot(preds_res, main = "residuals vs predicted values")
length(nb_preds)
length(residuals(neg_bin_modelglm))
preds_res[which(preds_res[,1] <=10),] #COME BACK TO

# i. tabulate the frequencies of counts in visits
plot(table(NMES1988$visits)) # mostly 0's
plot(table(round(predict(poisson_model, type="response"))))
#Poisson shifts the histogram away from 0, when observationally, we see many cases as 0's
#This makes it a good candidate for a zero inflated poisson model

# j Hurdle vs zip
Hurd_model <- hurdle(visits ~ ., NMES1988)
summary(Hurd_model)
# I think that the hurdle model makes sense for this data. Since we're trying to predict visits,
# I think it adds a nice layer of distinction for the coefficients that lend themselves to having a positive count of visits.

#k
install.packages("pscl")
library(pscl)
zip_mod <- zeroinfl(visits ~ ., data=NMES1988)
summary(zip_mod)

zip_model_small <- zeroinfl(visits ~ health +
                              chronic +
                              region+
                              married +
                              school +
                              employed+
                              insurance+
                              medicaid | chronic +
                                age +afam +gender + married + school+
                                insurance +medicaid, data = NMES1988)
summary(zip_model_small)

(lrt <- 2*(zip_mod$loglik-zip_model_small$loglik))
(DegFree <- zip_model_small$df.residual - zip_mod$df.residual)
pchisq(lrt,DegFree,lower.tail = FALSE) #Appears that the smaller model is different than the larger; shouldn't reduce

Pred_zero <- predict(zip_mod,type="zero") 
Pred_Count <- predict(zip_mod,type="count") 
Total_Prob0 <- Pred_zero + (1-Pred_zero)*exp(-Pred_Count)
mean(Total_Prob0)
mean(NMES1988$visits == 0)
mean(dpois(0,predict(poisson_model,type="response")))
mean(dnbinom(0,mu = predict(neg_bin_model,type="response"), 
             size = neg_bin_model$theta))

# The zip_model performs almost identically to the observed model when looking at zeros. 
#The normal Poisson greatly underestimates 0s, and the neg bin underestimates slightly.
     
############################
############################
# GLMsData Cancer#
############################
############################
install.packages("GLMsData")
data(ccancer, package="GLMsData")
#a. investigate the 0's. Are they random? Remove these observations
# These observations show structural issues - incorrect matching for M/F to Site of Cancer
head(ccancer)
cancer_df <- filter(ccancer, Count != 0)

#b. Create a var that stores the rate of #deaths per 10,000 pop. Plot against each predictor
death_rate


rate <- 10000*(cancer_df$Count/cancer_df$Population)
plot(cancer_df$Gender, rate) #Median death rate is slightly higher for females, but males have a wider distribution
plot(cancer_df$Region, rate)
plot(cancer_df$Site, rate)
#Clearly, lung cancer has both the highest death rate per 10,000, and the highest IQR. Even on in the 25th percentile, though, the 
#death rate for lung cancer is significantly higher than the other cancers
cancer_df$Deaths <- cancer_df$Count
#c Fit a poisson rate model for # of deaths 
cancer_df$Count
rate_model <- glm(Deaths ~ log(Population)+ Gender+Site+Region, family = poisson, data = cancer_df)
anova(rate_model, test="Chisq")
summary(rate_model)
exp(.33)
# = 1.39 = Cancer occurs at 1.39x higher rate in males than it does in females
pchisq(summary(rate_model)$deviance, rate_model$df.residual, lower.tail=FALSE)
# Statistically significantly different from the saturated model. Indicating a somewhat poor fit,
# despite seeming much better than the Null deviance.
       