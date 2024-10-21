

#### 10-10-2024

rm(list = ls()) # to clear the environment


#install.packages("summarytools")
#install.packages("lme4")


library(foreign)
library(MASS) 
library(ggplot2)
library(forcats)
library(mgcv)
library(summarytools)
library(lme4)
library(Matrix)  # its dependency on lme4 package 
negative.binomial(10)

#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")

moth_data <- read.csv("input/Moth_count_data_2024.csv")



##To explore the distribution of your vriables and count data like moth_count
# quick visualizations
dfSummary(moth_data)
str(moth_data)

#check to see the distribution of your count data
hist(moth_data$moth_count, 
          main = "Histogram of Moth count", 
          xlab = "Spongy moth", 
          ylab = "Frequency", 
          col = "lightblue", 
          border = "black")


#Calculate the mean and standard deviation of moth_count for each level of stand_category to see if there are differences.
library(dplyr)

# Checking unique combinations
table(moth_data$stand_category, moth_data$patch_name)

# Summary statistics for moth_count by stand_category and stand_type
summary_stats <- moth_data %>%
  group_by(stand_category, stand_type) %>%
  summarise(
    mean_count = mean(moth_count, na.rm = TRUE),
    sd_count = sd(moth_count, na.rm = TRUE),
    count = n()
  )

print(summary_stats, n=22)


# Summary statistics for moth_count by stand_category only
summary_stats_2 <- moth_data %>%
  group_by(stand_category) %>%
  summarise(
    mean_count = mean(moth_count, na.rm = TRUE),
    sd_count = sd(moth_count, na.rm = TRUE),
    count = n()
  )

print(summary_stats_2, n=22)


# Summary statistics for moth_count by stand_type
summary_stats_3 <- moth_data %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(moth_count, na.rm = TRUE),
    sd_count = sd(moth_count, na.rm = TRUE),
    count = n()
  )

print(summary_stats_3, n=22)

## Remove MOM row
moth_by_stand_summary_stats <- summary_stats_3[-c(1),]

print(moth_by_stand_summary_stats, n=22)


## Upon checking i see that you have a 14th stand category for stand_category, which was typo mistake i guess as NA
# i did converted it into N, if that is okay, otherwise reverse, there is also 1 empty row for the stand category, 
## You can inspect that in your main data, Also there is an overlap in your categorization: for example there are 2 D categories, 
### 3 E categories, H as well, and I
### I would leave it to you to fix that,  



#inspect visually
p1 <- ggplot(moth_data, aes(x = stand_category, y = moth_count)) +
      geom_boxplot() +
      labs(title = "Distribution of Moth Counts by Stand Category",
       x = "Stand Category",
       y = "Moth Count") +
      theme_minimal()


print(p1)


## There are more moths in your stand type A, E, and J, upon inspecting the main data file, it seems that the concentration of either
##oka or pine doesnt effect the abundnace of your moths. 
##So keeping the Stand_type instead in your model rather than category. 

#inspect visually
p2 <- ggplot(moth_data, aes(x = stand_type, y = moth_count)) +
  geom_boxplot() +
  labs(title = "Distribution of Moth Counts by Stand Type",
       x = "Stand Type",
       y = "Moth Count") +
  theme_minimal()


print(p2)


p3 <- ggplot(moth_by_stand_summary_stats, aes(x = stand_type, y = mean_count)) +
  geom_boxplot() +
  labs(title = "Distribution of Moth Counts by Stand Type",
       x = "Stand Type",
       y = "Moth Count") +
  theme_minimal()

print(p3)


p4 <- ggplot(moth_by_stand_summary_stats, aes(x=stand_type), fill=class) + 
  geom_boxplot(aes(lower=mean_count-sd_count , upper=mean_count+sd_count , 
                   middle=mean_count , ymin=mean_count-3*sd_count , ymax=mean_count+3*sd_count),
               stat="identity") +
  labs(title = "Distribution of Moth Counts by Stand Type",
     x = "Stand Type",
     y = "Mean Moth Count") +
theme_classic()
  
print(p4)

## Some outliers in Oak, and MOM, but i kept the stand type in your model instead of stand_category

# before you proceed to the models, Make sure your variables are characters -> factors before
## adding them into the model. 

#Since you want to see linear realationship between your moth_counts with the percentage of stand concentration type
## i-e. from Oak--Pine, which you've already categorize them according to their percentage. 
## These categories needs to be converted into factors 
## but for the random factor like trap_name, you can leave the character as it is, to see if there is some variance in moth count from
## from trap to trap, in a stand 

#Convert stand_category and patch name into factors
moth_data$patch_name <- as.factor(moth_data$patch_name)
moth_data$stand_type <- as.factor(moth_data$stand_type)


#running glm model as suggested by Sabina, using patches to create a block effect, removing complexity as needed from one model to the next...
model_1 <- glmer(moth_count ~ patch_name * stand_category + (1|trap_name), family =poisson, data = moth_data)
summary(model_1)

### Too big of AIC scores but there is still variance in your trap_name (rnadom factor) about 0.962, 
### here is the thing, since your each row is one observation, hence treating the trap_name as a random effect isnt the great idea. 
### should choose glm model instead with poisson family. 


## Check model performance via package "performance"

library(performance)  ### To check model assumptions, overdispersion and and model fitness (https://easystats.github.io/performance/)

check_overdispersion(model_1)
###Overdispersion detcted in the model by 17.741, should be 1 or around 1 for good fit. 
check_model(model_1)  ### for plotting model residuals: you can inspect the messages for each reidual plot
                      ### if it follows the instruction on top of each plot, your model is a good fit.


### Try nb with stand_category model
model_1a <- glmer.nb(moth_count ~ stand_category + (1|trap_name), family =nbinom2(), data = moth_data)
summary(model_1a)

check_overdispersion(model_1a)

check_model(model_1a)

####### Stand_type model
model_1b <- glm(moth_count ~ stand_type, family = poisson, data = moth_data)
summary(model_1b)

check_overdispersion(model_1b)

check_model(model_1b)

####### Patch_name Model with random effect
model_1c <- glmer.nb(moth_count ~ patch_name + (1|trap_name), family = nbiom2(), data = moth_data
                     )
summary(model_1c)

check_overdispersion(model_1c)

check_model(model_1c)

#Check for Multicollinearity:
#If you have categorical variables like patch_name with many levels, multicollinearity can be an issue. 
#Use the car package to check Variance Inflation Factors (VIFs):
 
library(car)
vif(model_1c)

####### Patch_name Model, not with random effect - keeping all the factors the same
model_1d <- glmer.nb(moth_count ~ patch_name + (patch_name | trap_name), family = nbiom2(), data = moth_data)

check_overdispersion(model_1d)

check_model(model_1d)

###Visualize Fixed Effects:
### To better understand the effects of each patch, you can visualize the estimates with confidence intervals:
##USe

library(effects)
plot(allEffects(model_1c))

### If you need the same plot for better visualization via ggplot2, gather all the attributes manually
# 1.Extract the fixed effects estimates
fixed_effects <- summary(model_1c)$coefficients

# 2.Convert it to a data frame for easier manipulation
fixed_effects_df <- as.data.frame(fixed_effects)

# 3. Adding the row names as a column for patch names
fixed_effects_df$patch_name <- rownames(fixed_effects_df)

# 4.Rename columns for easier access
colnames(fixed_effects_df) <- c("Estimate", "Std_Error", "z_value", "p_value", "patch_name")

# 5. Calculate the 95% confidence intervals
fixed_effects_df <- fixed_effects_df %>%
  mutate(
    CI_Lower = Estimate - 1.96 * Std_Error,
    CI_Upper = Estimate + 1.96 * Std_Error
  )

# I would replace patch_name values back to the original names (if needed)
# 6. Create a lookup table that matches the shortened names to the full names as they appear in the your data
patch_name_lookup <- c(
  "(Intercept)" = "(Intercept)",
  "patch_nameMont Saint Hilaire" = "Mont Saint Hilaire",
  "patch_nameMontebello" = "Montebello",
  "patch_nameNotre Dame de Bonsecours" = "Notre Dame de Bonsecours",
  "patch_nameOka" = "Oka",
  "patch_nameOrford" = "Orford",
  "patch_namePapineauville" = "Papineauville",
  "patch_nameRigaud" = "Rigaud"
)

# 7. Replace the patch_name values using the lookup table
fixed_effects_df$patch_name <- patch_name_lookup[fixed_effects_df$patch_name]

# h. Calculate the 95% confidence intervals
fixed_effects_df <- fixed_effects_df %>%
  mutate(
    CI_Lower = Estimate - 1.96 * Std_Error,
    CI_Upper = Estimate + 1.96 * Std_Error
  )

# 8. Plot the estimates
p5 <- ggplot(fixed_effects_df, aes(x = reorder(patch_name, Estimate), y = Estimate)) +
  geom_point(size = 3, color = "blue") +  # Plot points for estimates 
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "blue") +  # Error bars for CIs
  labs(
    title = "Fixed Effects Estimates with Confidence Intervals",
    x = "Patch Name",
    y = "Estimate (log scale)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") # Add a horizontal line at y = 0
                                                                      # above 0 are the poistive estimates with greater 
                                                                      # moth counts 


print(p5)

ggsave("stand composition Effecting Moth's Population.png", plot = p3, width = 10, height = 8, dpi = 300)


p6 <- ggplot(moth_data, aes(x= patch_name, 
                            y= moth_count, 
                            group = trap_name))+
  geom_smooth(method = "lm", 
              se= FALSE, 
              linewidth = 0.5, 
              colour ="grey")+
  geom_point(shape = 21)

print(p6)

######################################################################################################
####### May be confirm the model with some 1 else as well. Thats what I got.  




model_2 <- glmer(moth_count ~ stand_category + patch_name + (1|trap_name), family =poisson, data = moth_data)

model_3 <- glmer(moth_count ~ stand_category + (1|trap_name), family =poisson, data = moth_data)


summary(model_1)
AIC(model_1)

summary(model_2)
AIC(model_2)

summary(model_3)
AIC(model_3)

summary(model_4)
AIC(model_4)

