### Generalize Additive Models (GAMS) ###

#### using cleaned data originally called "moth_counts_2" from the script "Pheromone Trap Analyses" ####
## with assistance from QCBS workshop https://r.qcbs.ca/workshop08/pres-en/workshop08-pres-en.html#1

#install.packages("ggplot2")
#install.packages("mgcv")
#install.packages("itsadug")

library(ggplot2)
library(mgcv)
library(itsadug)

library(ggplot2, quietly = TRUE)
library(mgcv, quietly = TRUE)


#loading data 

moth_GAM <- read.csv("input/old/moth_glm.csv")

summary(moth_GAM)
str(moth_GAM)

# All Variables GAM -------------------------------------------------------

#using the glm.nb function from the MASS package to estimate a 
#negative binomial regression - all variables from year 1 
moth_gam1_all<- gam(total_continuous ~ prop_oak + site_area + x_acer + 
                      x_pinus + surrounded_by + longitude_e_w + 
                     #densite_du_couvert +
                     s(patch_name, bs="re"), 
                   data = moth_glm, 
                   method = "REML", family = "nb")

summary(moth_gam1_all)
plot(moth_gam1_all, pages = 1)

# Print a clean table to the console or a markdown/HTML-friendly output
# Save directly to a file
tab_model(moth_gam1_all,
          show.stat = TRUE,
          p.style = "numeric",
          file = "year1_all_variable_modelsummary.doc")   # can be .doc, .html, .htm

# Fitted values
fitted_vals_all <- fitted(moth_gam1_all)

# Pearson residuals
pearson_resid_all <- residuals(moth_gam1_all, type = "pearson")

# Residual degrees of freedom
rdf_all <- df.residual(moth_gam1_all)

plot(fitted_vals_all, pearson_resid_all, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
moth_gam1_all <- sum(pearson_resid_all^2) / rdf_all
moth_gam1_all
#dispersion = 0.6074, indicating that there is mild underdispersion, not too concerning

# Correlation tests -------------------------------------------------------

#checking for collinearity between variables

#VIF = 1 indicates no multicollinearity
collinearity_all_y1 <- check_collinearity(moth_gam1_all)
print(collinearity_all_y1)

#write.csv(as.data.frame(collinearity_all_y1),
#          "Multicollinearity check all variables year 1 (VIF).csv",
#         row.names = FALSE)

# Pearson's correlation test between different continuous variables

pairs <- list(
  c("prop_oak", "x_pinus"),
  c("prop_oak", "x_conifers"),
  c("prop_oak", "x_acer"),
  c("prop_oak", "longitude_e_w"),
  c("prop_oak", "site_area"),
  c("x_conifers", "x_pinus"),
  c("x_acer", "x_pinus"),
  c("prop_oak", "site_area"),
  c("site_area", "longitude_e_w")
)

corr_results <- do.call(rbind, lapply(pairs, function(p) {
  x <- moth_glm[[p[1]]]
  y <- moth_glm[[p[2]]]
  test <- cor.test(x, y)
  
  data.frame(
    var1 = p[1],
    var2 = p[2],
    correlation = test$estimate,
    p_value = test$p.value,
    method = test$method
  )
}))

write.csv(as.data.frame(corr_results),
        "Correlation of all variables year 1.csv",
          row.names = FALSE)


# GAMS each variable ------------------------------------------------------

moth_gam1_oak<- gam(total_continuous ~ prop_oak +
                      s(patch_name, bs="re"), 
                    data = moth_glm, 
                    method = "REML", family = "nb")

summary(moth_gam1_oak)

# Fitted values
fitted_vals_oak <- fitted(moth_gam1_oak)

# Pearson residuals
pearson_resid_oak <- residuals(moth_gam1_oak, type = "pearson")

# Residual degrees of freedom
rdf_oak <- df.residual(moth_gam1_oak)

plot(fitted_vals_oak, pearson_resid_oak, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
moth_gam1_oak <- sum(pearson_resid_oak^2) / rdf_oak
moth_gam1_oak
#dispersion = 0.6106, indicating that there is mild underdispersion, not too concerning

moth_gam1_pine<- gam(total_continuous ~ x_pinus +
                      s(patch_name, bs="re"), 
                    data = moth_glm, 
                    method = "REML", family = "nb")

summary(moth_gam1_pine)

# Fitted values
fitted_vals_pine <- fitted(moth_gam1_pine)

# Pearson residuals
pearson_resid_pine <- residuals(moth_gam1_pine, type = "pearson")

# Residual degrees of freedom
rdf_pine <- df.residual(moth_gam1_pine)

plot(fitted_vals_pine, pearson_resid_pine, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
moth_gam1_pine <- sum(pearson_resid_pine^2) / rdf_pine
moth_gam1_pine
#dispersion = 0.6094, indicating that there is mild underdispersion, not too concerning

moth_gam1_both<- gam(total_continuous ~ prop_oak + x_pinus +
                      s(patch_name, bs="re"), 
                    data = moth_glm, 
                    method = "REML", family = "nb")

summary(moth_gam1_both)

# Fitted values
fitted_vals_both <- fitted(moth_gam1_both)

# Pearson residuals
pearson_resid_both <- residuals(moth_gam1_both, type = "pearson")

# Residual degrees of freedom
rdf_both <- df.residual(moth_gam1_both)

plot(fitted_vals_both, pearson_resid_both, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
moth_gam1_both <- sum(pearson_resid_both^2) / rdf_both
moth_gam1_both
#dispersion = 0.6109, indicating that there is mild underdispersion, not too concerning







##fit a linear regression model to the relationship between Moth Count and Prop Oak
linear_model <- gam(total_continuous ~ prop_oak, data = moth_GAM)
summary(linear_model)

#The linear model (w prop oak) is not explaining much of variance (Radj= 0.00501)

linear_model2 <- gam(total_continuous ~ prop_oak + x_pinus, data = moth_GAM)
summary(linear_model2)

#The linear model (w prop oak + pine) is a little better (Radj= 0.122), explaining 14% of the deviance

##looking at how the model fits the data
data_plot <- ggplot(data = moth_GAM, aes(y = total_continuous, x = prop_oak)) +
  geom_point() +
  geom_line(aes(y = fitted(linear_model)),
            colour = "red", linewidth = 1.2) +
  theme_bw()
data_plot

##looking at how the model fits the data (with pine...)
#not sure if I can actually do this (??)
data_plot2 <- ggplot(data = moth_GAM, aes(y = total_continuous, x = prop_oak + x_pinus)) +
  geom_point() +
  geom_line(aes(y = fitted(linear_model)),
            colour = "red", size = 1.2) +
  theme_bw()
data_plot2

#N#ow, fitting a model to the data using a smooth function s() with mgcv::gam()
gam_model <- gam(total_continuous ~ s(prop_oak), data = moth_GAM)
summary(gam_model)

##got the following error message: "Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#A term has fewer unique covariate combinations than specified maximum degrees of freedom
#so ran it again, but specify the level of knots 'k' used (number of basis functions used to create a smooth function)
gam_model <- gam(total_continuous ~ s(prop_oak, k = 3), data = moth_GAM)
summary(gam_model)

data_plot <- data_plot +
  geom_line(aes(y = fitted(gam_model)),
            colour = "blue", size = 1.2)
data_plot

plot(gam_model)

#edf = 1 (effective degrees of freedom), which indicates a linear relationship

gam_model2 <- gam(total_continuous ~ s(prop_oak, k = 6), data = moth_GAM)
summary(gam_model2)

data_plot2 <- data_plot +
  geom_line(aes(y = fitted(gam_model2)),
            colour = "red", size = 1.2)
data_plot2

plot(gam_model2)


#edf is above 1 (3.49), indicating a curved fit

#also tried with k = 8, which is similar to k = 6, but slightly worse.  Above k = 8, doesn't work
# so k = 6 is most suitable

linear_model <- gam(total_continuous ~ prop_oak, data = moth_GAM)
smooth_model <- gam(total_continuous ~ s(prop_oak, k = 6), data = moth_GAM)
AIC(linear_model, smooth_model)

#AIC is practically the same for both models, so there is no indication of improved performance of the non-linear model over the linear model


##to try to model the moth count using multiple predictors simultaneously.
#First, we need to convert the categorical predictor 'patch_name' into a factor variable.
moth_GAM$patch_name <- as.factor(moth_GAM$patch_name)

summary(moth_GAM)
str(moth_GAM)

##starting with a basic model, with one smoothed term (oak) and one categorical predictor (patch)
basic_model <- gam(total_continuous ~ patch_name + s(prop_oak, k = 6), data = moth_GAM, method = "REML")
basic_summary <- summary(basic_model)

basic_summary$p.table

basic_summary$s.table

par(mfrow=c(1,2))
plot(basic_model, all.terms = TRUE)

#EDF = ~1, p-value low, but not statistically significant

##adding a second term, prop pine, but with a linear relationship with moths
two_term_model <- gam(total_continuous ~ patch_name + s(prop_oak, k = 6) + x_pinus, 
                      data = moth_GAM, method = "REML")
two_term_summary <- summary(two_term_model)

two_term_summary$p.table
two_term_summary$s.table

par(mfrow=c(2,2))
plot(two_term_model, all.terms = TRUE)

#EDF = ~1, p-value statistically significant for oak, p = 0.019

##to know whether the relationship between moths and pine is non-linear, add pine as a smooth term as well
two_smooth_model <- gam(total_continuous ~ patch_name + s(prop_oak, k = 6) + s(x_pinus, k = 6), 
                        data = moth_GAM, method = "REML")
two_smooth_summary <- summary(two_smooth_model)

two_smooth_summary$p.table
two_smooth_summary$s.table

par(mfrow = c(2,2))
plot(two_smooth_model, all.terms = TRUE)

#for oak, EDF = ~1, p-value statistically significant, p = 0.019
#for pine, EDF = ~1.4, p-value statistically significant, p = 0.049

##compare AIC of the 3 models
AIC(basic_model, two_term_model, two_smooth_model)

#the two_term_model has the lowest AIC

###GAM with Interaction terms
factor_interact <- gam(total_continuous ~  s(prop_oak, k = 6, by=patch_name), 
                       data = moth_GAM, method = "REML")

summary(factor_interact)$s.table
summary(factor_interact)$p.table

par(mfrow = c(2,2))
plot(factor_interact)

vis.gam(factor_interact, theta = 120, n.grid = 50, lwd = .4)

factor_interact2 <- gam(total_continuous ~  s(prop_oak, k = 6, by=patch_name) + x_pinus, 
                       data = moth_GAM, method = "REML")

summary(factor_interact2)$s.table
summary(factor_interact2)$p.table

par(mfrow = c(2,2))
plot(factor_interact2)

vis.gam(factor_interact2, theta = 120, n.grid = 50, lwd = .4)

factor_interact3 <- gam(total_continuous ~  s(prop_oak, k = 6, by=patch_name) + s(x_pinus, k = 6), 
                        data = moth_GAM, method = "REML")

summary(factor_interact3)$s.table
summary(factor_interact3)$p.table

par(mfrow = c(2,2))
plot(factor_interact3)

vis.gam(factor_interact3, theta = 120, n.grid = 50, lwd = .4)

##interactions between two smoothed terms, oak and pine.
smooth_interact <- gam(total_continuous ~ patch_name + s(prop_oak, x_pinus, k = 6), 
                       data = moth_GAM, method = "REML")
summary(smooth_interact)$s.table
summary(smooth_interact)$p.table

smooth_interact <- gam(total_continuous ~ patch_name + site_area + s(prop_oak, x_pinus, k = 6), 
                       data = moth_GAM, method = "REML")
summary(smooth_interact)$s.table
summary(smooth_interact)$p.table

smooth_interact2 <- gam(total_continuous ~ site_area + s(prop_oak, x_pinus, k = 6), 
                       data = moth_GAM, method = "REML")
summary(smooth_interact2)$s.table
summary(smooth_interact2)$p.table

#EDF = 2, statistically significant p-value (0.017)

#check AIC
AIC(factor_interact, factor_interact2, factor_interact3, smooth_interact, smooth_interact2)
AIC(two_term_model, smooth_interact, smooth_interact2)

###Same AIC between previous 'two_term_model" and "smooth_interact", "smooth_interact_2" slightly lower AIC


##Verify:1)the choice of basis dimension k; 2)The residuals plots. EDF should be smaller than 'k'
#if they are too close, the model is being overly constrained
k.check(two_term_model)
k.check(smooth_interact)
k.check(smooth_interact2)

par(mfrow = c(2,2))
gam.check(two_term_model)
par(mfrow = c(2,2))
gam.check(smooth_interact)
par(mfrow = c(2,2))
gam.check(smooth_interact2)

#there's a larger difference between EDF and k in the two_term_model
#two_term_model <- gam(total_continuous ~ patch_name + s(prop_oak, k = 6) + x_pinus, 
#data = moth_GAM, method = "REML")


##For an interaction model, need a probability distribution that allows the variance to increase with the mean.
#'Tweedie' is a family of distributions that has this property and that works well in a GAM 
smooth_interact_tw <- gam(total_continuous ~ patch_name + s(prop_oak, k = 6, x_pinus),
                          data = moth_GAM, family = tw(link = "log"), method = "REML")

summary(smooth_interact_tw)$p.table

summary(smooth_interact_tw)$s.table

k.check(smooth_interact_tw)

par(mfrow = c(2,2))
gam.check(smooth_interact_tw)

AIC(smooth_interact_tw)

?family.mgcv

smooth_interact2_tw <- gam(total_continuous ~ site_area + s(prop_oak, k = 6, x_pinus),
                          data = moth_GAM, family = tw(link = "log"), method = "REML")

summary(smooth_interact2_tw)$p.table

summary(smooth_interact2_tw)$s.table

k.check(smooth_interact2_tw)

par(mfrow = c(2,2))
gam.check(smooth_interact2_tw)

AIC(smooth_interact2_tw)

?family.mgcv

AIC(smooth_interact, two_term_model, smooth_interact_tw, smooth_interact2_tw)

###lowest AIC is in the 'smooth interact2' (regular and Tweedie) models; 
#followed by the 'smooth interact" models
#smooth_interact2 <- gam(total_continuous ~ site_area + s(prop_oak, x_pinus, k = 6), 
#data = moth_GAM, method = "REML")
#smooth_interact <- gam(total_continuous ~ patch_name + s(prop_oak, x_pinus, k = 6), 
#data = moth_GAM, method = "REML")




