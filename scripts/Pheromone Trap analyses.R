### Data cleaning ###

#### moth count data ####
#loading data 

moth_counts <- read.csv("input/Pheromone_data_consolidated2.csv")

library(janitor) #janitor cleans up column names.It removes all unique characters and replaces spaces with _.
#piping through `dplyr`
moth_counts_1 <- moth_counts %>%
  clean_names() #Cleans names of an object (usually a data.frame)

# quick visualizations
summary(moth_counts_1)
str(moth_counts_1)

# change variable type from chr to num for total_from_rand
moth_counts_1$total_from_rand <- as.numeric(moth_counts_1$total_from_rand)

# quick visualizations again
summary(moth_counts_1)
str(moth_counts_1)

# looking for mistakes
unique(moth_counts_1$stand_type)
unique(moth_counts_1$moth_content_low_0_20_mid_20_50_high_50_80_very_high_80)
unique(moth_counts_1$total_consolidated_categorical)
unique(moth_counts_1$total_from_rand)

#remove all spaces
## in order to standardize all stand type names, remove all spaces
library(tidyverse)
moth_counts_clean <- moth_counts_1 %>%
  mutate(stand_type = str_replace(stand_type, " ", ""))

moth_counts_clean <- moth_counts_1 %>%
  mutate(stand_type = str_replace(stand_type, " ", ""))

unique(moth_counts_clean$stand_type)

# Remove un-needed rows #
moth_counts_clean <- moth_counts_clean[-c(103:118),]

## again, to remove 2nd space
moth_counts_clean <- moth_counts_clean %>%
  mutate(stand_type = str_replace(stand_type, " ", ""))

unique(moth_counts_clean$stand_type)

# if any column names need replacing
colnames(moth_counts_clean)[colnames(moth_counts_clean)=="total_from_rand"] <- "total_continuous"
colnames(moth_counts_clean)[colnames(moth_counts_clean)=="total_consolidated_categorical"] <- "total_categorical"
colnames(moth_counts_clean)[colnames(moth_counts_clean)=="x_quercus"] <- "prop_oak"

moth_counts_clean <- moth_counts_clean %>%
  mutate(prop_oak = str_replace(prop_oak, "NA","0"))

# change stand_type to trap_ID, so that an 'actual' stand_type column can be created
colnames(moth_counts_clean)[colnames(moth_counts_clean)=="stand_type"] <- "trap_ID"


## change "Co-Dom" to "Mid" in order to create a unique variable (different from "Dom")
moth_counts_2 <- moth_counts_clean %>%
  mutate(trap_ID = str_replace(trap_ID, "Co-Dom", "Mid"))

# create the actual stand_type column, identifying the oak treatment of each trap
moth_counts_2$stand_type <- ifelse(grepl("Mid",moth_counts_2$trap_ID), "Co-Dom",
                                   ifelse(grepl("Low",moth_counts_2$trap_ID), "Low",
                                          ifelse(grepl("Dom",moth_counts_2$trap_ID), "Dom", "")))

#export the cleaned data to use in other analyses script files
write.csv(moth_counts_2, file = "moth_counts_stats.csv", row.names = FALSE)

### box & whisker plot of moth count (numberical) by Stand type (categorical) - each data point a totalled trap count
ggplot(moth_counts_2) +
  aes(x = total_continuous, y = stand_type) +
  geom_boxplot(fill = "#A429AF") +
  labs(x = "Number of Male Moths", 
       y = "Oak Presence") +
  theme_minimal() +
  theme(plot.caption = element_text(size = NA), axis.title.y = element_text(size = 25L, 
                                                                            face = "bold"), axis.title.x = element_text(size = 25L, face = "bold"), 
        axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))


library(ggplot2)
library(forcats)


###Multinomial Logistic Regression Analysis (https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/)
library(foreign)
library(nnet)
library(reshape2)

# getting some descriptive stats of the data (M and SD)
with(moth_counts_2, do.call(rbind, tapply(total_continuous, total_categorical, function(x) c(M = mean(x), SD = sd(x)))))
with(moth_counts_2, do.call(rbind, tapply(total_continuous, prop_oak, function(x) c(M = mean(x), SD = sd(x)))))

#Multinomial logistic regression, for categorical data (incorrect - uses moth counts as both predictor and response)
moth_counts_2$total_categorical <- as.factor(moth_counts_2$total_categorical)
moth_counts_2$total_categorical2 <- relevel(moth_counts_2$total_categorical, ref = "Low")
test <- multinom(total_categorical2 ~ stand_type + total_continuous, data = moth_counts_2)

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

###Negative Binomial Generalized Linear Model, for continuous data

#From websites "https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/" 


install.packages("packagename")

library(packagename)
library(foreign)
library(MASS) 
negative.binomial(10) 

#using the glm.nb function from the MASS package to estimate a negative binomial regression
summary(t1 <- glm.nb(total_continuous ~ stand_type, data = moth_counts_2))

#Likelihood ratio tests of Negative Binomial Models
t2 <- update(t1, . ~ . - prog)
anova(t1, t2)

#Checking model assumption
t3 <- glm(total_continuous ~ stand_type, family = "poisson", data = moth_counts_2)
pchisq(2 * (logLik(t1) - logLik(t3)), df = 1, lower.tail = FALSE)

#get the confidence intervals for the coefficients by profiling the likelihood function
(est <- cbind(Estimate = coef(t1), confint(t1)))
#looking at incident rate ratios rather than coefficients. To do this, exponentiate the model coefficients
exp(est)

#attempt to look at predicted counts for various levels of the predictors.  Doesn't work yet...
newdata1 <- data.frame(stand_type = factor(1:3, levels = 1:3, 
    labels = levels(moth_counts_2$stand_type)))
newdata1$phat <- predict(t1, newdata1, type = "response")
newdata1

###Running a Poisson Regression, from website "https://stats.oarc.ucla.edu/r/dae/poisson-regression/"
summary(moth_counts_2)

install.packages("sandwich")
install.packages("msm")

library(sandwich)
library(msm)

#use the 'tapply' function to display summary statistics by program type
with(moth_counts_2, tapply(total_continuous, stand_type, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

#a conditional histogram, separated by stand type, to show distribution
ggplot(moth_counts_2, aes(total_continuous, fill = stand_type)) +
  geom_histogram(binwidth=5.0, position="dodge")

#run a Poisson regression using the 'glm' function
summary(p1 <- glm(total_continuous ~ stand_type, family="poisson", data=moth_counts_2))

#use package 'sandwich' to obtain robust standard errors, calculate the p-values, and 95% confidence interval.
cov.p1 <- vcovHC(p1, type="HC0")
std.err <- sqrt(diag(cov.p1))
r.est <- cbind(Estimate= coef(p1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(p1)/std.err), lower.tail=FALSE),
               LL = coef(p1) - 1.96 * std.err,
               UL = coef(p1) + 1.96 * std.err)

r.est

#to try to determine if there are omitted predictor variables, if the linearity assumption holds, or if there is an issue of over-dispersion
with(p1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

anova(p1, test="Chisq")


##coding to use for graph when continuous oak data is in + another variable (doesn't work yet)

## calculate and store predicted values
p1$phat <- predict(p1, type="response")

## order by program and then by math
p <- p1[with(p1, order(prop_oak, surrounded_by)), ]

## create the plot
ggplot(p1, aes(x = prop_oak, y = phat, colour = prop_oak)) +
  geom_point(aes(y = total_continuous), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Stand Type", y = "Male Moth Count")
