## HW 5 ##

# 1. Cake Data
library(dplyr)
data(cake, package = "lme4")
cake <- cake %>% select(-temperature)
head(cake)

#a. Fixed and random variables?
# Recipe = Fixed
# Temp = Random
# replicate = Nested? random?
## RETURN

# b. 
cake %>% 
  group_by(temp) %>%
  summarise(no_rows = length(temp))
## balanced - all temp values (alphas) have same n 

model <- glm(angle ~ recipe * temp, data = cake, family=gaussian())
summary(model)
model2 <- lm(angle ~ recipe*temp, cake)
# temp is significant, pval =  3.377e-08
mean((residuals(model)^2))
# MSE = 59.4053 using glm, MSE = 60.76 using lm() and anova()
anova(model2)


# c. Fit mixed model that  takes into account the replicates for the recipe
# examine variance components. 
library(lme4)
mix_mod <- lmer(angle ~ recipe + temp + (1 | replicate), cake)
summary(mix_mod)
# Using recipe and temp as fixed effects, we see that there is variation in the replicates
# The replicate variance component of 39.21 shows that it makes up a large portion of the MSE from part b. 
# t value for temp = 9.215
install.packages("lmerTest")
library(lmerTest)
#d. Testing for temp and recipe effect
summary(lmerTest :: lmer(angle ~ recipe + temp + (1 | replicate), cake))$coefficients
# Both recipeB, recipeC, and temp all have significant p values

# e. QQ plots and residual vs predicted for Linear model
# Linear model
plot(model) # QQ plot = not very good fit
preds <- predict(model, type="response")
residuals(model)

plotpredlinear <- cbind(preds, residuals(model))
plot(plotpredlinear) #  

# Mixed model
plot(mix_mod) # pearson residuals vs fitted values looks good
qqnorm(resid(mix_mod))
qqline(resid(mix_mod))
# The qq plot shows that the mixed model residuals are much closer to what would be expected under normality 
mixpred <- predict(mix_mod, type="response")
plotmix<- cbind(mixpred, residuals(mix_mod))
plot(plotmix) #Substantially more randomly distributed than the linear model resids. vs predicted values! 

# f. Examine BLUPs
ranef(mix_mod)$replicate #BLUPS (Best Linear Unbiased Predictors) for replicate random effect
plot(cake$replicate, cake$angle)
abline(h=mean(cake$angle), col="purple")

predict(mix_mod, newdata=data.frame(replicate=1, recipe="A", temp=175))
cake %>%
  filter(recipe == "A", temp== 175, replicate==1)
#recipe A, temp 175, replicate 1:
#actual = 42, predicted with mixed model = 43.41

#recipe A, temp 175, replicate 8
cake %>%
  filter(recipe == "A", temp== 175, replicate==8)
predict(mix_mod, newdata=data.frame(replicate=8, recipe="A", temp=175))
# actual = 24, predicted = 24.588

# the BLUPS follow the pattern of the box and whisker plot with replicate on x and angle on y