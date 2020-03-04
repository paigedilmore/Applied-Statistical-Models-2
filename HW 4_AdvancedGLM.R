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

## Problem B
