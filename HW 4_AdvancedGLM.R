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
  geom_point(aes(yield, nitrogen, col=type), alpha=0.8)

# e. 
