### Generalize Linear Mixed Models (LMM) ###

#### using cleaned data originally called "moth_counts_2" from the script "Pheromone Trap Analyses" ####
## with assistance from QCBS workshop https://r.qcbs.ca/workshop07/pres-en/workshop07-pres-en.html#1


#loading data 

moth_LMM <- read.csv("input/moth_glm.csv")

library(lme4)
library(MASS)
library(vcdExtra)
library(bbmle)
library(MuMIn)
library(ggplot2)
library(DescTools)
library(remotes)
library(gridExtra)
library(lattice)

# Simple theme for all ggplot figures after this
fig <- theme_bw() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_blank(), 
        strip.background=element_blank(), 
        strip.text.y = element_text(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        panel.border = element_rect(colour="black", fill = NA))

### Plot the data, lumping certain variables together
# allows for estimating slope and intercept parameters for each categorical predictor variable separately,
# uses all the data available (lumping) while accounting for pseudoreplication 
# accounts for structure in data (for example, stands nested in plots nested in forests);
# while allowing relationships to vary by different grouping factors (known as random effects);
# requires less parameter estimates than classical regression which saves degrees of freedom.

plot <- ggplot(aes(prop_oak, total_continuous), data = moth_LMM)

# Plot 1 - All data
plot + geom_point() + 
  labs(x = "percent oak", y = "moth count", 
       title = "All data") + 
  fig

# Plot 2 - By surrounding forest type
plot + geom_point() + 
  facet_wrap(~ surrounded_by) + 
  labs(x = "percent oak", y = "moth count", 
       title = "By Surrounding Forest Type") + 
  fig

# Plot 3 – By age of forest
plot + geom_point() + 
  facet_wrap(~ classe_d_age) + 
  labs(x = "percent oak", y = "moth count", 
       title = "By Forest Age") + 
  fig











##Section: 04-implement-LMM.R 

# Look at data structure
str(fish.data)

# Look at the distribution of samples for each factor
table(fish.data[ , c("Lake", "Fish_Species")])

# Look at the distribution of continuous variables:
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
hist(fish.data$Fish_Length, xlab = "Length (mm)", main = "")
hist(fish.data$Trophic_Pos, xlab = "Trophic position", main = "")

plot(fish.data)
cor(Fish_Length,Trophic_Pos)

## Challenge 3:
# Use a Z-correction to see if the data have very different scales of variation, based on species or lake
# this 'standardizes' the scales of variation

# Standardized fish length, with the function scale
fish.data$Z_Length <- (fish.data$Fish_Length - mean(fish.data$Fish_Length)) / 
  sd(fish.data$Fish_Length)

# Standardized trophic position, with the function scale
fish.data$Z_TP     <- scale(fish.data$Trophic_Pos)


lm.test <- lm(Z_TP ~ Z_Length, data = fish.data)

lm.test.resid <- rstandard(lm.test)

par(mfrow = c(1, 2))

plot(lm.test.resid ~ as.factor(fish.data$Fish_Species),
     xlab = "Species", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test.resid ~ as.factor(fish.data$Lake),
     xlab = "Lake", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)

source("glmm_funs.R")