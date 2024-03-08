### Data cleaning ###

#### moth count data ####
#loading data 

moth_counts <- read.csv("input/Pheromone_data_consolidated.csv")

library(janitor) #janitor cleans up column names.It removes all unique characters and replaces spaces with _.
#piping through `dplyr`
moth_counts_1 <- moth_counts %>%
  clean_names() #Cleans names of an object (usually a data.frame)

# quick visualizations
summary(moth_counts_1)
str(moth_counts_1)

# looking for mistakes
unique(moth_counts_1$stand_type)

#remove all spaces
## in order to standardize all stand type names, remove all spaces
library(tidyverse)
moth_counts_clean <- moth_counts_1 %>%
  mutate(stand_type = str_replace(stand_type, " ", ""))

unique(moth_counts_clean$stand_type)

## again, to remove 2nd space
moth_counts_clean <- moth_counts_clean %>%
  mutate(stand_type = str_replace(stand_type, " ", ""))

unique(moth_counts_clean$stand_type)

# if any column names need replacing
colnames(moth_counts_clean)[colnames(moth_counts_clean)=="total_g_continuous"] <- "total_continuous"

# change stand_type to trap_ID, so that an 'actual' stand_type column can be created
colnames(moth_counts_clean)[colnames(moth_counts_clean)=="stand_type"] <- "trap_ID"


## change "Co-Dom" to "Mid" in order to create a unique variable (different from "Dom")
moth_counts_2 <- moth_counts_clean %>%
  mutate(trap_ID = str_replace(trap_ID, "Co-Dom", "Mid"))

# create the actual stand_type column, identifying the oak treatment of each trap
moth_counts_2$stand_type <- ifelse(grepl("Mid",moth_counts_2$trap_ID), "Co-Dom",
                                   ifelse(grepl("Low",moth_counts_2$trap_ID), "Low",
                                          ifelse(grepl("Dom",moth_counts_2$trap_ID), "Dom", "")))


library(ggplot2)
library(forcats)


##Multinomial Logistic Regression Analysis (https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/)
library(foreign)
library(nnet)
library(reshape2)

# getting some descriptive stats of the data (M and SD)
with(moth_counts_2, do.call(rbind, tapply(total_continuous, total_categorical, function(x) c(M = mean(x), SD = sd(x)))))

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

##Negative Binomial Generalized Linear Model, for continuous data

#From websites "https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/" 

#couldn't install "packagename"
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
