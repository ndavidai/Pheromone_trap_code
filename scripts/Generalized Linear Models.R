### Generalize Linear Models ###

#### using cleaned data originally called "moth_counts_2" from the script "Pheromone Trap Analyses" ####
## with assistance from QCBS workshop https://r.qcbs.ca/workshop06/book-en/the-distributions-of-biological-data.html
## and UCLA website https://stats.oarc.ucla.edu/r/seminars/generalized_linear_regression/

#loading data 

moth_glm <- read.csv("input/moth_counts_stats.csv")

install.packages("forcats")

library(foreign)
library(MASS) 
library(ggplot2)
library(forcats)
negative.binomial(10) 

# quick visualizations
summary(moth_glm)
str(moth_glm)

# change variable type from chr to num for prop_oak
moth_glm$prop_oak <- as.numeric(moth_glm$prop_oak)

# quick visualizations again
summary(moth_glm)
str(moth_glm)

# looking for mistakes
unique(moth_glm$prop_oak)

# change NA's in tree species columns to 0
moth_glm <- replace(moth_glm,is.na(moth_glm),0)

unique(moth_glm$prop_oak)

#export this data 
write.csv(moth_glm, file = "input/moth_glm.csv", row.names = FALSE)

#using the glm.nb function from the MASS package to estimate a negative binomial regression
summary(glm <- glm.nb(total_continuous ~ stand_type, data = moth_glm))

#moths by prop oak
#use function 'lm' to fit (estimate) the linear model and 'summary' to extract the results
m1 <- lm(total_continuous ~ prop_oak, data = moth_glm)
summary(m1)

#Intercept of 51.52 and slope of 17.66, far from significant p-value

#use function 'plot' to graph the scatter plot & 'abline(m1)' to plot the regression line
plot(total_continuous ~ prop_oak, data = moth_glm, 
     main = "Scatter plot of moth count vs. prop oak", 
     ylab = "male moth count",
     xlab = "% oak", xlim = c(0,1))
abline(m1, col = "blue")

# model diagnostics plots to evaluate the model assumptions (of normal distribution, etc)
opar <- par(mfrow = c(2, 2), 
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(m1)

#moths by longitude
#use function 'lm' to fit (estimate) the linear model and 'summary' to extract the results
m2 <- lm(total_continuous ~ longitude_e_w, data = moth_glm)
summary(m2)

#Not significant

#use function 'plot' to graph the scatter plot & 'abline(m1)' to plot the regression line
plot(total_continuous ~ longitude_e_w, data = moth_glm, 
     main = "Scatter plot of moth count vs. longitude", 
     ylab = "male moth count",
     xlab = "longitude", xlim = c(-76,-71))
abline(m2, col = "blue")

# model diagnostics plots to evaluate the model assumptions (of normal distribution, etc)
opar <- par(mfrow = c(2, 2), 
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(m2)

#moths by forest density cover
#use function 'lm' to fit (estimate) the linear model and 'summary' to extract the results
m3 <- lm(total_continuous ~ densite_du_couvert, data = moth_glm)
summary(m3)

# p = 0.06 - still not really significant, but the strongest trend so far

#use function 'plot' to graph the scatter plot & 'abline(m1)' to plot the regression line
plot(total_continuous ~ densite_du_couvert, data = moth_glm, 
     main = "Scatter plot of moth count vs. % density cover", 
     ylab = "male moth count",
     xlab = "% density cover", xlim = c(0.1,1))
abline(m3, col = "blue")

# model diagnostics plots to evaluate the model assumptions (of normal distribution, etc)
opar <- par(mfrow = c(2, 2), 
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(m3)

#moths by % conifers
#use function 'lm' to fit (estimate) the linear model and 'summary' to extract the results
m4 <- lm(total_continuous ~ x_conifers, data = moth_glm)
summary(m4)

# p = 0.02 - only significant variable so far

#use function 'plot' to graph the scatter plot & 'abline(m1)' to plot the regression line
plot(total_continuous ~ x_conifers, data = moth_glm, 
     main = "Scatter plot of moth count vs. % conifers", 
     ylab = "male moth count",
     xlab = "% conifers", xlim = c(0,1))
abline(m4, col = "blue")

# model diagnostics plots to evaluate the model assumptions (of normal distribution, etc)
opar <- par(mfrow = c(2, 2), 
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(m4)

#moths by % maples
#use function 'lm' to fit (estimate) the linear model and 'summary' to extract the results
m5 <- lm(total_continuous ~ x_acer, data = moth_glm)
summary(m5)

# not significant

#use function 'plot' to graph the scatter plot & 'abline(m1)' to plot the regression line
plot(total_continuous ~ x_conifers, data = moth_glm, 
     main = "Scatter plot of moth count vs. % maple", 
     ylab = "male moth count",
     xlab = "% maple", xlim = c(0,1))
abline(m5, col = "blue")

# model diagnostics plots to evaluate the model assumptions (of normal distribution, etc)
opar <- par(mfrow = c(2, 2), 
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(m5)

# for this model, added in one variable at time; then removed one at time, by least significance
# left with oaks and pines as the best-fit model for a none-mixed model
m6 <- lm(total_continuous ~  prop_oak + x_acer + x_pinus + x_conifers + longitude_e_w + densite_du_couvert, data = moth_glm)
summary(m6)
AIC(m6)

# left with oaks and pines as the best-fit model for a none-mixed model (lowest AIC and p-value)
m6 <- lm(total_continuous ~  prop_oak + x_pinus , data = moth_glm)
summary(m6)
AIC(m6)

# correlation test between different continuous variables
cor.test(moth_glm$prop_oak, moth_glm$x_pinus)
cor.test(moth_glm$prop_oak, moth_glm$x_conifers)
cor.test(moth_glm$prop_oak, moth_glm$x_acer)
cor.test(moth_glm$prop_oak, moth_glm$longitude_e_w)
cor.test(moth_glm$prop_oak, moth_glm$densite_du_couvert)
cor.test(moth_glm$x_conifers, moth_glm$x_pinus)
cor.test(moth_glm$x_acer, moth_glm$x_pinus)




## code after this not ready...







##look up resource to help fix code for this...
###Multinomial Logistic Regression Analysis (https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/)
library(foreign)
library(nnet)
library(reshape2)

# getting some descriptive stats of the data (M and SD)
with(moth_glm, do.call(rbind, tapply(total_continuous, surrounded_by, function(x) c(M = mean(x), SD = sd(x)))))
with(moth_glm, do.call(rbind, tapply(total_continuous, stand_type, function(x) c(M = mean(x), SD = sd(x)))))

#Multinomial logistic regression, for categorical data (incorrect - uses moth counts as both predictor and response)
moth_glm$total_continuous <- as.factor(moth_glm$total_continuous)
moth_glm$total_continuous <- relevel(moth_glm$total_continuous, ref = "0")
test <- multinom(total_continuous ~ stand_type + surrounded_by, data = moth_glm)

summary(test)
library(sjPlot)
tab_model(test)

#Multinomial logistic regression, for categorical data (continuous moth counts removed as predictor)
moth_counts_2$total_categorical <- as.factor(moth_counts_2$total_categorical)
moth_counts_2$total_categorical2 <- relevel(moth_counts_2$total_categorical, ref = "Low")
test <- multinom(total_categorical2 ~ stand_type, data = moth_counts_2)

summary(test)
library(sjPlot)
tab_model(test)

