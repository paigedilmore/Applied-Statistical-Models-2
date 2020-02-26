## HW 3 ##

# A


# B

data(worldcup,package = "faraway") 
library(dplyr) 
worldcup <- worldcup %>% 
  filter(Position != "Goalkeeper") %>% 
  select(-"Saves")

test = worldcup[2,]
test$passes90 <- (101/351)*90
test$tackles90 <- (14/351)*90
test

# a new vars
worldcup$passes90<- (worldcup$Passes/worldcup$Time)*90
worldcup$tackles90 <- (worldcup$Tackles/worldcup$Time)*90
head(worldcup)

# b Fit Poisson
rt_model <- glm(Shots ~ offset(log(Time)) + Position + tackles90 + passes90
                , family = poisson, data = worldcup)

summary(rt_model)

pchisq(deviance(rt_model), rt_model$df.residual, lower.tail = FALSE)
#Although the model didn't throw any errors, and the model fit, it's a poor fit overall.
# interpretted by the significance of the test shown above.
