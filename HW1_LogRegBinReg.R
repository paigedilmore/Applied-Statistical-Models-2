# Homework 1

## 1) Logistic Regression
#"birthwt" packages contains info on factors impacting infants being born with low birthweight

data(birthwt, package = "MASS")
head(birthwt)
install.packages("dplyr")
library(dplyr)


# 1a. Preliminary exploration
summary(birthwt[,c("age","lwt","smoke","ftv")])

plot(bwt ~ age, data=birthwt)
plot(bwt ~ lwt, data=birthwt)
plot(bwt ~ ui, data=birthwt)
plot(bwt ~ .,data=birthwt)

install.packages("ggplot2")
library(ggplot2)
ggplot(birthwt, aes(x=lwt, color = low))+geom_histogram(
  position="dodge", binwidth = 1
)
attach(birthwt)
ggplot(birthwt, aes(x=age, y=lwt))+geom_jitter(aes(color=low))

birthwt <- birthwt %>% select(-bwt)

#2 Fit a logistic regression model - all variables
head(birthwt)
## Converting to factor for non continuous vars
cols= c("race","smoke","ptl","ht","ui","ftv")
birthwt[cols] <- lapply(birthwt[cols], factor)  
is.factor(birthwt$race)


model <- glm(low ~ ., data=birthwt, family="binomial")
summary(model)

# Testing the deviance
# Ds=Dl: 235-189 = 46
# to obtain p value:
test_dev = summary(model)$null.deviance - summary(model)$deviance
1-pchisq(q=test_dev, df=1) # = 1.14 e-11 = significant. At least 1 variable is related to the response

#drop1 to test different models using chi sq
drop1(model, test = "Chi")
summary(model)
# Using drop1() we see that the deviance significantly increases
# with the loss of lwt, ptl, and ht, each of which are also found significant in the summary()

little_model <- glm(low ~ lwt+race+smoke+ht+ui, data=birthwt, family=binomial)
summary(little_model)
# little model increases the residual deviance to 204,with null deviance at 234
anova(little_model,model, test="Chi")
# these models are not significantly different
# When we add age, ptl, and ftv to a model that already has the other factors, we don't improve our predictive power.
# little model = 204
#model = 188
# p val = 0.075

# e. How much does smoking during pregnancy increase
#the odds of having a low birthweight baby? CI?

summary(little_model)
beta <- coef(little_model)
exp(beta)
con_intervals <-confint(little_model)

exp(con_intervals)
odds
exp(.2808) #=1.32 #Smoking while pregnant increases odds of low birthweight between 1.32x and 6.23 times, as opposed to not smoking during pregnancy
exp(1.8290) # = 6.23

# f. make a binned residual vs fitted probab. plot & comment on fit of model
log_dev_residuals <- residuals(little_model)
########### copied code###############
library(dplyr)
install.packages("tidyr")
library(tidyr)
plot_bin <- function(Y, X, bins = 100, return.DF = FALSE){ 
  Y_Name <- deparse(substitute(Y)) 
  X_Name <- deparse(substitute(X)) 
  Binned_Plot <- data.frame(Plot_Y = Y, Plot_X = X) 
  Binned_Plot$bin <- cut(Binned_Plot$Plot_X,breaks = bins) %>% 
    as.numeric
  
  Binned_Plot_summary <- Binned_Plot %>% 
    group_by(bin) %>% 
    summarise(Y_ave = mean(Plot_Y), 
              X_ave = mean(Plot_X), 
              Count = n()) %>% as.data.frame
  plot(y = Binned_Plot_summary$Y_ave, 
       x = Binned_Plot_summary$X_ave, 
       ylab = Y_Name,xlab = X_Name) 
  if(return.DF) return(Binned_Plot_summary)
}

logistic_predictions <- predict(little_model, type="response")

plot_bin(Y = log_dev_residuals,
         X = logistic_predictions,
         bins = 200)
# There is evidence of pattern, a potential assumption-violating problem
summary(birthwt$low)
log_dev_residuals
birthwt[,-1]
# g Plot binned residuals against the lwt variable
plot_bin(Y = log_dev_residuals,
         X = birthwt$lwt,
         bins = 200)
# No distinct pattern, suggesting good fit
# Although, there is some consistency in the deviance residuals around -0.5

#h Using 10 bins, plot the observed proportions of low birthweight on Y and the 
# binned predictions on x

BinnedData <- plot_bin(Y = birthwt$low,
                       X = logistic_predictions,
                       bins = 10,
                       return.DF = TRUE)
abline(0,1, lty=3)
# The fit of the model looks really, really good looking at this plot.

# i compute the Hosmer-Lemeshow stat & p value. Conclusion?
HL_BinVals <-(BinnedData$Count*BinnedData$Y_ave - BinnedData$Count*BinnedData$X_ave)^2/
  BinnedData$Count*BinnedData$X_ave*(1-BinnedData$X_ave) 
head(HL_BinVals,8)
sum(HL_BinVals) #.1776

pchisq(q=sum(HL_BinVals), df=10,lower.tail = FALSE)
# = 1 = no concern about the fit of the model.Observed events match the expected events.

#j Using .5, do the predictions & classification table
observed_class <- birthwt$low
predicted_prob <- predict(little_model, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, yes =1, no = 0)
table(observed_class, predicted_class)

# finding:            
#            predicted_class
#observed_class   0   1
#0               117  13
#1               38  21  # identified less than half of the true low birthweights (21/59)

sum(birthwt$low)

#k thresholds from .05-.829, steps of 0.001, sensitivities and specificities 
# create an ROC, comment on inverse relationship btwn sens and spec.

thresh <- seq(0.05, .829, .001)
sensitivity <- numeric(length(thresh))
specificity <- numeric(length(thresh))

for(j in seq(along = thresh)) {
  predicted_class <- ifelse(predicted_prob > thresh[j],
                            yes=1, no=0)
  Conf_matrix <- table(observed_class, predicted_class)
  #
  specificity[j] <- Conf_matrix[1,1]/(Conf_matrix[1,1] + Conf_matrix[1,2])
  #
  sensitivity[j] <- Conf_matrix[2,2]/(Conf_matrix[2,1]+Conf_matrix[2,2])
}

# ROC 
plot(1-specificity, sensitivity, type="l", main = "ROC Curve",
     xlim = c(0,1), ylim = c(0,1))
abline(0,1,lty=2)

Conf_matrix
# l. plotting the sensitivity and specificity against each other
# As we vary the threshold to determine classifications, the inverse relationship
#betweem sensitivity and specificity is strongly evident. Threshold of 0.3 seems like a good cut off to balance.

matplot(thresh, cbind(sensitivity, specificity), type="l",
        xlab = "Threshold", ylab = "Proportion", lty=1:2)
#Conf_matrix with a cutoff of 0.3
observed_class <- birthwt$low
predicted_prob2 <- predict(little_model, type = "response")
predicted_class2 <- ifelse(predicted_prob > 0.3, yes =1, no = 0)
table(observed_class, predicted_class2)
# comparisons: we decreased our specificity (dropped to 67%)
# greatly increased our sensitivity to 71%
87/(87+43)
42/(42+17)

#m. produce ROC curves for logit, probit, cauchit,cloglog
# Appreciable differences between the links? 

# logit = see ROC above
observed_class <- birthwt$low
little_model_probit <- glm(low ~ lwt+race+smoke+ht+ui, data=birthwt, family=binomial(link=probit))
predicted_prob_probit <- predict(little_model_probit, type = "response")
predicted_class_probit <- ifelse(predicted_prob_probit > 0.5, yes =1, no = 0)
table(observed_class, predicted_class_probit) #returns same matrix as logit

thresh <- seq(0.05, .829, .001)
sensitivity <- numeric(length(thresh))
specificity <- numeric(length(thresh))

for(j in seq(along = thresh)) {
  predicted_class_probit <- ifelse(predicted_prob_probit > thresh[j],
                                   yes=1, no=0)
  Conf_matrix <- table(observed_class, predicted_class_probit)
  #
  specificity[j] <- Conf_matrix[1,1]/(Conf_matrix[1,1] + Conf_matrix[1,2])
  #
  sensitivity[j] <- Conf_matrix[2,2]/(Conf_matrix[2,1]+Conf_matrix[2,2])
}

# PROBIT ROC 
plot(1-specificity, sensitivity, type="l", main = "Probit ROC Curve",
     xlim = c(0,1), ylim = c(0,1))
abline(0,1,lty=2)


# CLOGLOG
little_model_cloglog <- glm(low ~ lwt+race+smoke+ht+ui, data=birthwt, family=binomial(link=cloglog))
predicted_prob_cloglog <- predict(little_model_cloglog, type = "response")
predicted_class_cloglog <- ifelse(predicted_prob_cloglog > 0.5, yes =1, no = 0)
table(observed_class, predicted_class_cloglog) #returns a tiny varying confusion matrix

thresh <- seq(0.05, .829, .001)
sensitivity <- numeric(length(thresh))
specificity <- numeric(length(thresh))

for(j in seq(along = thresh)) {
  predicted_class_cloglog <- ifelse(predicted_prob_cloglog > thresh[j], ### unable to detect why this model will not predict any 0s
                                    yes=1, no=0)
  Conf_matrix <- table(observed_class, predicted_class_cloglog)
  #
  specificity[j] <- Conf_matrix[1,1]/(Conf_matrix[1,1] + Conf_matrix[1,2])
  #
  sensitivity[j] <- Conf_matrix[2,2]/(Conf_matrix[2,1]+Conf_matrix[2,2])
}

# CLOGLOG ROC # Will NOT predict any zeros - cannot plot
plot(1-specificity, sensitivity, type="l", main = "CLOGLOG ROC Curve",
     xlim = c(0,1), ylim = c(0,1))
abline(0,1,lty=2)

#Cauchit ROC # Will NOT predict any zeros - cannot plot

little_model_cauchit <- glm(low ~ lwt+race+smoke+ht+ui, data=birthwt, family=binomial(link=cauchit))
predicted_prob_cauchit <- predict(little_model_cauchit, type = "response")
predicted_class_cauchit <- ifelse(predicted_prob_cauchit > 0.5, yes =1, no = 0)
table(observed_class, predicted_class_cauchit) #returns a tiny varying confusion matrix

thresh <- seq(0.05, .829, .001)
sensitivity <- numeric(length(thresh))
specificity <- numeric(length(thresh))

for(j in seq(along = thresh)) {
  predicted_class_cauchit <- ifelse(predicted_prob_cauchit > thresh[j], 
                                    yes=1, no=0)
  Conf_matrix <- table(observed_class, predicted_class_cauchit)
  #
  specificity[j] <- Conf_matrix[1,1]/(Conf_matrix[1,1] + Conf_matrix[1,2])
  #
  sensitivity[j] <- Conf_matrix[2,2]/(Conf_matrix[2,1]+Conf_matrix[2,2])
}
Conf_matrix

# Cauchit ROC 
plot(1-specificity, sensitivity, type="l", main = "Cauchit ROC Curve",
     xlim = c(0,1), ylim = c(0,1))
abline(0,1,lty=2)

#####################
#####################

#BINOMIAL REGRESSION#

#####################
#####################
install.packages("boot")
library(boot)
data(downs.bc, package = "boot")
head(downs.bc)
# goal: quantify relationship between maternal age and the incidence proportion of Down's

#a. Given a "sharp rise" after age 30, I would expect an exponential relationship to fit better than linear
#b. Empirical logits
downs.bc$p <- downs.bc$r/downs.bc$m
#r = cases
downs.bc$controls = downs.bc$m-downs.bc$r
plot(x=downs.bc$age,y=log(downs.bc$r/(downs.bc$controls)),
     ylab = "Empirical Logit",
     xlab = "Age")
#Suggests and exponential/curvilinear model
head(downs.bc)
#c.
model <- glm(cbind(r, controls) ~ age, data = downs.bc, family = binomial) 
summary(model)
model_sq <- glm(cbind(r, controls) ~ age + age**2, data = downs.bc, family = binomial) 
summary(model_sq)

# Adding the squared term did not change the residual deviance:
# 184.03 for both models. Both models are significantly better when compared to the Null model

# is the model significantly worse than the saturated model?
pchisq(deviance(model_sq), df.residual(model_sq), lower=FALSE)
# = approx 0 .. so, yes it's worse.


#d. plot obs. proportions against age of mother

downs.bc$proportions <- (downs.bc$r+1)/
  (downs.bc$r + downs.bc$controls +1)
plot(proportions ~ age, data=downs.bc)
model_proportions <- predict(model, type = "response")
model_sq_proportions <- predict(model_sq, type="response")
summary(model_proportions)
plotting_data <- cbind(downs.bc$age, model_proportions, model_sq_proportions, downs.bc$proportions)
library(ggplot2)
plotting_data
plotting_data <- as.data.frame(plotting_data)
names(plotting_data)[1] <- "age"
names(plotting_data)[4] <- "empirical_logits"

plot(empirical_logits ~ age, data = plotting_data, col="blue")+
  plot(model_proportions ~ age, data=plotting_data, col="red")
plot(model_sq_proportions ~ age, data=plotting_data, col="pink")
# The relationship btwn age and probability appears curvilinear for a model w/ one 
# linear term because

# e. Pearson residuals & estimated dispersion parameter
presidual <- residuals(model_sq, type = "pearson")
Xsq <- sum(residuals(model_sq, type = "pearson")^2)
n <- nrow(downs.bc)
q <- length(coefficients(model_sq))
#estimated dispersion param:
Xsq/(n-q)
#8.144 = model is over dispersed, possibly because we have an incorrect structural form, or we are not
## accounting for more explanatory factors (which could be the case since we are only predicting with age)

# f. Fitting the beta binomial model to help with overdispersion
install.packages("dispmod")
library(dispmod)
betabin_model <- glm.binomial.disp(model_sq)
summary(betabin_model)
# residual deviance = 18.88 - much better than the strictly binomial model!
# is it significantly different than the saturated model?
pchisq(deviance(betabin_model), df.residual(betabin_model), lower=FALSE)
#.9018 = not significantly different from the saturated model! 

#g. 
predict_g <- predict(betabin_model, type="response")
cbind(downs.bc$age, predict_g)
(0.0031532358+0.0038104081)/2
# 0.35% predicted probability for a 35 year old mother to have a child with Down's.

#####################
#####################

#ORDINAL REGRESSION#

#####################
#####################
install.packages("carData")
library(carData)
data(WVS, package = "carData")
