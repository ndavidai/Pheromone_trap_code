### Generalize Linear Mixed Models (LMM) ###

#### using cleaned data originally called "moth_counts_2" from the script "Pheromone Trap Analyses" ####
## with assistance from QCBS workshop https://r.qcbs.ca/workshop07/pres-en/workshop07-pres-en.html#1


#loading data 

moth_LMM <- read.csv("input/moth_glm.csv")

utils::install.packages("lme4")

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
  fig

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



#tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
#install.packages("lme4", type = "source")

#remove.packages("Matrix")
#remove.packages("lme4")
#install.packages("lme4", type = "source")


lmer(Z_moth_count ~ Z_prop_oak + (Z_moth_count | patch_name),
     data = moth_LMM, REML = TRUE)

lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)
