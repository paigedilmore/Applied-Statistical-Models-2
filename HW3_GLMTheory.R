## HW 3 ##

# 1a see attached photo 

# 1b
expression(exp(theta)) %>% D("theta") #exp(theta)
#exp(log(mu)) = mu = E[y]

#1c
expression(exp(theta) * 1) %>% D("theta") %>% D("theta") # = exp(theta)
# Variance function
expression(exp(theta)) %>% D("theta") %>% D("theta")
# The mean = variance in the Poisson distribution

# 2

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
# Why: The null deviance and residual deviance are really large. The coefficients don't make much sense
# when you interpret them (e.g. every increase in tackles per game = 144 more shots, really?); 
# The offset(Time) seems to put the model on two different scales. 
# https://stats.stackexchange.com/questions/232666/should-i-use-an-offset-for-my-poisson-glm suggests that the 
# poisson GLM doesn't apply the offset with a log transformation automatically. 


#d. adding control=list(maxit=2000). I believe this control argument tells the model that it can only iterate 2000 times for fitting.
rt_model_2 <- glm(Shots ~ offset(Time) + Position + tackles90 + passes90
    , family = poisson, data = worldcup, control=list(maxit=2000))

#e. 
summary(rt_model)
summary(rt_model_2)
# Compared to model 1, the summary output from model two shows seemingly better values for the deviances.
# The coefficients are still confusing when interpretted.Moreover, Tackles per game and Passes per game are opposite signs in model 2

# f Using model 2: calculating the DFBETAs for each observation to find outliers in the data
# E.g. n <- c (31, 47, 18, 9, 25, 19, 62, 32)
# e.g. pos_max <- which.max(n)
install.packages("ggplot2")
library(ggplot2)
which.max(abs(dfbeta(rt_model_2))[2])
head(worldcup)

apply(abs(dfbeta(rt_model_2)), 2, which.max)
worldcup[421,] # removing index 421, Spain Defender. He has very few shots for a large amt of playing Time and large amt of Passes
summary(worldcup$Shots/worldcup$Passes)
2/402
# testing things
# dfscatter <- worldcup[,c(4,8)]
# head(dfscatter)
# plot(dfscatter$Shots ~ dfscatter$tackles90, col=ifelse((dfscatter$Shots==2)
#      & (dfscatter$tackles90 == 2.5)), "red", "black")
# dfscatter[166,]

#g. remove outlier and refit
world2 <- worldcup[-421,]

rt_model_3 <- glm(Shots ~ offset(Time) + Position + tackles90 + passes90
                  , family = poisson, data = world2, control=list(maxit=2000))

summary(rt_model_3)
exp(coef(rt_model_3))
summary(rt_model_2)
exp(coef(rt_model_2))

# removing the outlier and refitting the model, the degrees of freedom and AICs are similiar, 
# but the coefficient for tackles per game becomes much more pronounced.
# It drops from .82 in model 2 to .12 in model 3. All of the signs on the coefficients
# remain the same. 

#h. interpreting passas and tackles coefficient 
exp(coef(rt_model_3)) #passes and tackles are significant using model 3 (offset(Time) & removed index 421)
# for tackles:  0.1280758 means that every 1 increase in tackles per game results in fractional amount of Shots. 
# for passes: 2.032273 means that every 1 increase in passes per game results in 2.03 more Shots. This doesn't really make
# sense intuitively (if you pass the ball, how can you take more shots), but I don't know much about soccer.

