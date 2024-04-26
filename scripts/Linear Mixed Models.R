### Generalize Linear Mixed Models (LMM) ###
#### using cleaned data originally called "moth_counts_2" from the script "Pheromone Trap Analyses" ####
## with assistance from QCBS workshop https://r.qcbs.ca/workshop07/pres-en/workshop07-pres-en.html#1


#loading data 

moth_LMM <- read.csv("input/moth_glm.csv")

install.packages(c("lme4",
                   "Matrix",
                   "MASS",
                   "vcdExtra",
                   "bbmle",
                   "MuMIn",
                   "ggplot2",
                   "DescTools",
                   "remotes",
                   "gridExtra",
                   "lattice"))

options(repos = c(CRAN = "https://cloud.r-project.org"))
utils::install.packages("Matrix")
utils::install.packages("lme4")

library(Matrix)
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

# Plot 2 - By surrounding landscape
plot + geom_point() + 
  facet_wrap(~ surrounded_by) + 
  labs(x = "percent oak", y = "moth count", 
       title = "By Surrounding Landscape Type") + 
  fig

# Plot 3 – By age of forest
plot + geom_point() + 
  facet_wrap(~ age_class) + 
  labs(x = "percent oak", y = "moth count", 
       title = "By Forest Age") + 
  fig

# Plot 4 – By forest type
plot + geom_point() + 
  facet_wrap(~ forest_type) + 
  labs(x = "percent oak", y = "moth count", 
       title = "By Forest Type") + 
  fig

# Plot 5 – By patch (site)
plot + geom_point() + 
  facet_wrap(~ patch_name) + 
  labs(x = "percent oak", y = "moth count", 
       title = "Patch") + 
  stat_smooth(method = "lm")+
    fig

# Store the plot as an object
p_patch <- ggplot(data = moth_LMM, aes(x = prop_oak, y = total_continuous)) +
  geom_point() +
  facet_wrap(~ patch_name) + 
  labs(x = "percent oak", y = "moth count", title = "Patch") + 
  stat_smooth(method = "lm")




#model of moth count by oak, by patch
by_patch <- lmer(total_continuous ~ prop_oak + (1 | patch_name), data = moth_LMM)

# extract the slopes of each of the 'patch' graphs
summary(by_patch)$coefficients

coefficients <- summary(by_patch)$coefficients

## Need to graph the slopes of each graph....



# Look at data structure
str(moth_LMM)

# Look at the distribution of samples for each categorical factor
table(moth_LMM[ , c("surrounded_by")])
table(moth_LMM[ , c("surrounded_by", "age_class")])
table(moth_LMM[ , c("surrounded_by", "forest_type")])
table(moth_LMM[ , c("forest_type")])

# Look at the distribution of continuous variables:
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
hist(moth_LMM$prop_oak, xlab = "% Oak", main = "")
hist(moth_LMM$total_continuous, xlab = "Moth Counts", main = "")

plot(moth_LMM)

# Use a Z-correction to see if the data have very different scales of variation, based on species or lake
# this 'standardizes' the scales of variation

# Standardized prop oak, with a z-correction
moth_LMM$Z_prop_oak <- (moth_LMM$prop_oak - mean(moth_LMM$prop_oak)) / 
  sd(moth_LMM$prop_oak)

# Standardized prop pine, with a z-correction
moth_LMM$Z_prop_pine <- (moth_LMM$x_pinus - mean(moth_LMM$x_pinus)) / 
  sd(moth_LMM$x_pinus)

# Standardized moth count, with the function scale
moth_LMM$Z_moth_count     <- scale(moth_LMM$total_continuous)

# Standardized longitude, with the function scale
moth_LMM$Z_longitude    <- scale(moth_LMM$longitude_e_w)


lm.test <- lm(Z_moth_count ~ Z_prop_oak, data = moth_LMM)

lm.test.resid <- rstandard(lm.test)

par(mfrow = c(1, 2))

plot(lm.test.resid ~ as.factor(moth_LMM$surrounded_by),
     xlab = "Surrounding Landscape", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test.resid ~ as.factor(moth_LMM$age_class),
     xlab = "Forest Age", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test.resid ~ as.factor(moth_LMM$forest_type),
     xlab = "Forest Type", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test.resid ~ as.factor(moth_LMM$patch_name),
     xlab = "Patch", ylab = "Standardized residuals")

abline(0, 0, lty = 2)


lm.test2 <- lm(Z_moth_count ~ Z_prop_oak + Z_prop_pine, data = moth_LMM)


lm.test2.resid <- rstandard(lm.test)

par(mfrow = c(1, 2))

plot(lm.test2.resid ~ as.factor(moth_LMM$surrounded_by),
     xlab = "Surrounding Landscape", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test2.resid ~ as.factor(moth_LMM$age_class),
     xlab = "Forest Age", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test2.resid ~ as.factor(moth_LMM$forest_type),
     xlab = "Forest Type", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test2.resid ~ as.factor(moth_LMM$patch_name),
     xlab = "Patch", ylab = "Standardized residuals")

abline(0, 0, lty = 2)


## implementing an LMM, coding potential models and model selection
lmer(Z_moth_count ~ Z_prop_oak + (1 | patch_name),
     data = moth_LMM, REML = TRUE)

lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 | patch_name),
     data = moth_LMM, REML = TRUE)

# Basic linear model / Linear model with no random effects
M0 <- lm(Z_moth_count ~ Z_prop_oak + Z_prop_pine, data = moth_LMM)
# Full model with varying intercepts
M1 <- lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 | patch_name) + (1 | stand_type), 
           data = moth_LMM, REML = FALSE)
# Full model with varying intercepts and slopes
M2 <- lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 + Z_prop_oak | patch_name) + (1 + Z_prop_oak | stand_type),
           data = moth_LMM, REML = FALSE)
# No stand type, varying intercepts only
M3 <- lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 | patch_name), data = moth_LMM, REML = FALSE)
# No patch, varying intercepts only
M4 <- lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 | stand_type), data = moth_LMM, REML = FALSE)

# No stand type, varying intercepts and slopes
M5 <- lmer(Z_moth_count ~ Z_prop_oak + (1 + Z_prop_oak | patch_name), 
           data = moth_LMM, REML = FALSE)
# No patches, varying intercepts and slopes
M6 <- lmer(Z_moth_count ~ Z_prop_oak + (1 + Z_prop_oak | stand_type), 
           data = moth_LMM, REML = FALSE)

# Full model with varying intercepts and slopes only varying by lake
M7 <- lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 | patch_name) + (1 + Z_prop_oak | stand_type),
           data = moth_LMM, REML = FALSE)
# Full model with varying intercepts and slopes only varying by species
M8 <- lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 + Z_prop_oak | patch_name) + (1 | stand_type),
           data = moth_LMM, REML = FALSE)

# Find AIC for all models (Basic linear model) using the package MuMIn
MuMIn::AICc(M0)
MuMIn::AICc(M1)
MuMIn::AICc(M2)
MuMIn::AICc(M3)
MuMIn::AICc(M4)
MuMIn::AICc(M5)
MuMIn::AICc(M6)
MuMIn::AICc(M7)
MuMIn::AICc(M8)

# Table of AIC values
AIC.table  <- MuMIn::model.sel(M0, M1, M2, M3, M4, M5, M6, M7, M8)

# Select only the columns of interest
# `df` is the degree of freedom
# `logLik` is the loglikelihood
# `delta` is the AICc difference with the lowest value
(AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")])

#lowest AICs are in M0 & M3 - 
#M0 shows there is no obvious random effect from either patch or stand type 
#M3 shows a potential slight random effect from patch 


# Take a closer look at M0 and M3.
# Because comparing two mixed effect models, can set `REML = TRUE` when generating M0 and M3
M0 <- lm(Z_moth_count ~ Z_prop_oak + Z_prop_pine, data = moth_LMM)

M3 <- lmer(Z_moth_count ~ Z_prop_oak + Z_prop_pine + (1 | patch_name), 
           data = moth_LMM, REML = TRUE)

# Print a table to compare M0 and M3 
MuMIn::model.sel(M0,M3)[ , c("df", "logLik", "AICc", "delta")]

# Plot predicted values vs residual values
par(mar=c(4,4,.5,.5))
plot(resid(M0) ~ fitted(M0), 
     xlab = 'Predicted values', 
     ylab = 'Normalized residuals')
abline(h = 0, lty = 2)

# Homogeneous dispersion of the residuals shows that the assumption is respected.

# To check the independence of the model residuals, plot residuals vs each covariate of the model
par(mfrow = c(1,3), mar=c(4,4,.5,.5))

plot(resid(M0) ~ moth_LMM$Z_prop_oak, 
     xlab = "Prop Oak", ylab = "Normalized residuals")
abline(h = 0, lty = 2)

plot(resid(M0) ~ moth_LMM$Z_prop_pine, 
     xlab = "Prop Pine", ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(resid(M0) ~ patch_name, data = moth_LMM, 
        xlab = "Patch Name", ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(resid(M8) ~ stand_type, data = moth_LMM, 
        xlab = "Stand Type", ylab = "Normalized residuals")
abline(h = 0, lty = 2)

# Homogeneous dispersion of the residuals around 0 = no pattern of residuals depending on the variable, and the assumption is respected

# Check the normality of the model residuals as residuals following a normal distribution indicate that the model is not biased.
hist(resid(M0))
(summ_M0 <- summary(M0))

hist(resid(M3))
(summ_M3 <- summary(M3))

# for producing figures, need the coefficients of the full model that are in the model summary.
summ_M0$coefficients
# Intercept = Intercept = 3.344745e-16

# We also need the coefficients for each level of the model, with the `coef` function
coef(M0)

# a simplified ggplot theme
fig <- theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background=element_blank()) +
  theme(strip.background=element_blank(),
        strip.text.y = element_text()) +
  theme(legend.background=element_blank()) +
  theme(legend.key=element_blank()) +
  theme(panel.border = element_rect(colour="black", fill=NA))

plot <- ggplot(aes(Z_prop_oak + Z_prop_pine, Z_moth_count), data = moth_LMM)
Plot_AllData <- plot + geom_point() +
  xlab("Prop Oak & Pine") + 
  ylab("Moth Count") +
  labs(title = "All data") + fig

Plot_AllData + geom_abline(intercept = summ_M0$coefficients[1,1], 
                           slope     = summ_M0$coefficients[2,1])

# Create a table with the coefs to facilitate their manipulation
patch_name.coef              <- coef(M3)$patch_name
colnames(patch_name.coef)    <- c("Intercept", "Slope")

# Figure by patch
Plot_ByPatch <- plot + 
  geom_point(aes(colour = factor(patch_name)), size = 4) +
  xlab("Prop Oak & Pine") + ylab("Moth Count") +
  labs(title = "By Patch") + fig

# Add in regression lines with the intercepts specific to each lake
Plot_ByPatch +
  geom_abline(intercept = patch_name.coef[1,1], 
              slope     = patch_name.coef[1,2], col = "coral2") +
  geom_abline(intercept = patch_name.coef[2,1], 
              slope     = patch_name.coef[2,2], col = "khaki4") +
  geom_abline(intercept = patch_name.coef[3,1], 
              slope     = patch_name.coef[3,2], col = "green4") +
  geom_abline(intercept = patch_name.coef[4,1], 
              slope     = patch_name.coef[4,2], col = "darkgoldenrod") +
  geom_abline(intercept = patch_name.coef[5,1], 
              slope     = patch_name.coef[5,2], col = "royalblue1") +
  geom_abline(intercept = patch_name.coef[6,1], 
              slope     = patch_name.coef[6,2], col = "magenta3")




