## HW 7 ###
#Repeated Measures##

data(BodyWeight, package ="nlme")
head(BodyWeight)

library(ggplot2)

#a) Plot weight vs time with rat-specific lines
# Base plot

ggplot(BodyWeight, aes(x=Time, y=weight)) +
  geom_point() + facet_wrap(~Rat) + scale_y_log10()

#Base model + plot
library(lme4)
panel <- lmer(weight ~ Diet + (1| Rat), data=BodyWeight)
summary(panel) #Rat accounts for a lot of variation

#Random intercepts
panel_2 <- lme4::fortify.merMod(panel)

ggplot(panel_2, aes(x=Time, y=weight))+
  geom_point() +
  geom_line(aes(x=Time, y=.fitted, group=Rat))

## Lines do not have increasing slopes with the rats weights over time

ggplot(panel_2, aes(x=Time, y=weight))+
  geom_point() +
  geom_line(aes(x=Time, y=.fitted, group=Rat, color=Diet))

## Rats with the lowest weights are consistently on Diet 1. 
## Diets 2 and 3 may or may not be different; it's hard to tell by the plots alone.

# B) Use lmList to fit lin reg. to describe how weights vary with days to each individual rat
# How do the slopes and intercepts vary? Patterns with respect to diets?
library(dplyr)

linreg <- lmList(weight ~ Time | Rat, BodyWeight)
sapply(linreg, coef)[,]%>% as.matrix

BodyWeight%>%
  group_by(Diet)%>%
  distinct(Rat)
  


#Intercept variation: There are two(ish) intercept classes; one in the 200s, and one in the 400-500s
# Rats on Diet 1 are in the 200s intercept group
# Rats on Diet 2 are in 400s; wider range of slopes
# Rats on Diet 3 collectively had the highest intercepts

# Slopes: Diet 2 seemed to have the steepest slope

# C) Mixed Model:
mixedmod <- lmer(weight ~ Time + (1 | Rat), BodyWeight)
summary(mixedmod)
# The FE estimate for the intercept, in the context of the variance component for rat, 
# appears to be simply placed in the middle of the two(ish) classes discussed above. Then the Rat var component moves it up and down.

# D) Obtain the BLUPs for rates and list who is most heavy
max(ranef(mixedmod)$Rat)
summary(mixedmod)$coefficients[1,1] + max(ranef(mixedmod)$Rat) # = 570; Rat 12

BodyWeight%>%
  subset(Rat==12)%>%
  summarize(mean(weight))

# E) Fit mixed effect with random slope, also
mixedmod_m <- lmer(weight ~ Time + (Time | Rat), BodyWeight,
                   control=lmerControl(optimizer="Nelder_Mead"))
summary(mixedmod_m)
confint(mixedmod_m, method="boot", oldNames=FALSE)

#Do slopes and intercepts seem to vary between subjects?

#F) BLUPS from E
min((ranef(mixedmod_m)$Rat)[,2]) #Rat 7
max((ranef(mixedmod_m)$Rat)[,2]) #Rat10
#Rat 7 = .21
summary(mixedmod_m)$coefficients[2] + min((ranef(mixedmod_m)$Rat)[,2])
#Rat 10 = 1.30
summary(mixedmod_m)$coefficients[2] + max((ranef(mixedmod_m)$Rat)[,2])

BodyWeight%>%
  subset((Rat==7) | (Rat ==10))%>%
          group_by(Rat)%>%
  summarize(minw = min(weight),meanw = mean(weight), maxw = max(weight),
            rangew = max(weight)-min(weight),
            pct_chg = (max(weight)-min(weight))/min(weight))
# The percent change for Rat 10 was more than 2x the percent change for Rat 7

#G) adding diet as interaction with time. Does diet effect weight? Does it effect differently at different times?

mixedmod_diet <- summary(lmerTest::lmer(weight ~ Time*Diet + (Time | Rat), BodyWeight,
                   control=lmerControl(optimizer="Nelder_Mead")))

summary(lmerTest::lmer(weight ~ Time*Diet + (Time | Rat), BodyWeight,
                       control=lmerControl(optimizer="Nelder_Mead"))) $coefficients   
# Diet does effect weight, with 2 and 3 being significantly different than diet 1
# Diet 2's slope gets significantly steeper over time.
diet2= BodyWeight%>%
  subset(Diet==2)
         
ggplot(diet2, aes(x=Time, y=weight)) +
  geom_point() + facet_wrap(~Rat) + scale_y_log10()