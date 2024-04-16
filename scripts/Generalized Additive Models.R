### Generalize Additive Models (GAMS) ###

#### using cleaned data originally called "moth_counts_2" from the script "Pheromone Trap Analyses" ####
## with assistance from QCBS workshop https://r.qcbs.ca/workshop08/pres-en/workshop08-pres-en.html#1

install.packages("ggplot2")
install.packages("mgcv")
install.packages("itsadug")

library(ggplot2)
library(mgcv)
library(itsadug)

library(ggplot2, quietly = TRUE)
library(mgcv, quietly = TRUE)


#loading data 

moth_GAM <- read.csv("input/moth_glm.csv")

#fit a linear regression model to the relationship between Moth Count and Prop Oak
linear_model <- gam(total_continuous ~ prop_oak, data = moth_GAM)
summary(linear_model)

#The linear model (w prop oak) is not explaining much of variance (Radj= 0.00501)

linear_model2 <- gam(total_continuous ~ prop_oak + x_pinus, data = moth_GAM)
summary(linear_model2)

#The linear model (w prop oak + pine) is a little better (Radj= 0.122), explaining 14% of the deviance

#looking at how the model fits the data
data_plot <- ggplot(data = moth_GAM, aes(y = total_continuous, x = prop_oak)) +
  geom_point() +
  geom_line(aes(y = fitted(linear_model)),
            colour = "red", size = 1.2) +
  theme_bw()
data_plot

#looking at how the model fits the data (with pine...)
data_plot2 <- ggplot(data = moth_GAM, aes(y = total_continuous, x = prop_oak + x_pinus)) +
  geom_point() +
  geom_line(aes(y = fitted(linear_model)),
            colour = "red", size = 1.2) +
  theme_bw()
data_plot2

#Now, fitting a model to the data using a smooth function s() with mgcv::gam()
gam_model <- gam(total_continuous ~ s(prop_oak), data = moth_GAM)
summary(gam_model)

gam_model <- gam(total_continuous ~ s(prop_oak, k = 3), data = moth_GAM)
summary(gam_model)

data_plot <- data_plot +
  geom_line(aes(y = fitted(gam_model)),
            colour = "blue", size = 1.2)
data_plot

plot(gam_model)

linear_model <- gam(total_continuous ~ prop_oak, data = moth_GAM)
smooth_model <- gam(total_continuous ~ s(prop_oak), data = moth_GAM)
AIC(linear_model, smooth_model)