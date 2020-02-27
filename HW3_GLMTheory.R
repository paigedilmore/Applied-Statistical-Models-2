## HW 3 ##

# A


# B

data(worldcup,package = "faraway") 
library(dplyr) 
worldcup <- worldcup %>% 
  filter(Position != "Goalkeeper") %>% 
  select(-"Saves")

# test = worldcup[2,]
# test$passes90 <- (101/351)*90
# test$tackles90 <- (14/351)*90
# test
# world2 <- worldcup
# world2$games <- world2$Time/90
# world2$passespgame<- world2$Passes/world2$games
# world2$tacklespgame <- world2$Tackles/world2$games
# 
# head(world2)
# head(worldcup)
# 
# rt_model_att2 <- glm(Shots ~ offset(log(Time)) + Position + passespgame + tacklespgame
#                      , data=world2, family = poisson)

#world3 <- world2[Reduce(`&`, lapply(world2, is.finite)),] # dropping Inf and NA values
#exp(coef(rt_model_att2))

# a new vars
worldcup$passes90<- (worldcup$Passes/worldcup$Time)*90
worldcup$tackles90 <- (worldcup$Tackles/worldcup$Time)*90
head(worldcup)
head(cbind(worldcup$Time, log(worldcup$Time)))

summary(log(worldcup$Time))

# b & #c Fit Poisson, comment on oddities 
rt_model <- glm(Shots ~ offset(Time) + Position + tackles90 + passes90
                , family = poisson, data = worldcup)

summary(rt_model)
exp(coef(rt_model))
pchisq(deviance(rt_model), rt_model$df.residual, lower.tail = FALSE) # =0
#Although no warnings appeared about the model fitting, looking at the summary() raises some suspicion. 
# Why: The null deviance, residual deviance, and all of the coefficients are super small; these 
#cannot really be used to extract much information from the model. The offset(Time) seems to put the model on 
#two different scales. 
# https://stats.stackexchange.com/questions/232666/should-i-use-an-offset-for-my-poisson-glm suggests that the 
# poisson GLM doesn't apply the offset with a log transformation automatically. 


# This model took 5 iterations to find this result. 

#d. adding control=list(maxit=2000). I believe this control argument tells the model that it can only iterate 2000 times for fitting.
rt_model_2 <- glm(Shots ~ offset(Time) + Position + tackles90 + passes90
    , family = poisson, data = worldcup, control=list(maxit=2000))

#e. 
summary(rt_model)
summary(rt_model_2)
# Compared to model 1, the summary output from model two shows extremely inflated values for the deviances
# and coefficient estimates (compared to model 1). Moreover, Tackles per game and Passes per game are opposite signs in model 2

# f Using model 2: calculating the DFBETAs for each observation to find outliers in the data
# E.g. n <- c (31, 47, 18, 9, 25, 19, 62, 32)
# e.g. pos_max <- which.max(n)

which.max(abs(dfbeta(rt_model_2))[2])
head(worldcup)



apply(abs(dfbeta(rt_model_2)), 2, which.max)
worldcup[421,]
max(abs(dfbeta(rt_model_2))

    