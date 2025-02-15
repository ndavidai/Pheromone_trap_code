
#### 17-01-25

rm(list = ls()) # to clear the environment

#Run this to load packages 
source("2024_code/scripts/Packages.R")

#complete_moth_2024 <- read.csv("input/2024_consolidated_moth_counts.csv")
complete_moth_2024_variables <- read.csv("input/2024_consolidated_moth_counts_all_variables.csv")
#complete - consolidation of July and Aug moth counts, 2024
#clean_complete - only includes traps that had clean counts for BOTH July and Aug


##To explore the distribution of your variables and count data like moth_count
# quick visualizations
dfSummary(complete_moth_2024_variables)
str(complete_moth_2024_variables)


#Remove MOM traps from either "stand type" or "stand category"
stand_type_filtered <- complete_moth_2024_variables %>%
  filter(stand_type != "MOM")
stand_category_filtered <- complete_moth_2024_variables %>%
  filter(stand_category != "MOM")


# Data Summaries ----------------------------------------------------------

#check to see the distribution of your 'complete' count data
hist(complete_moth_2024_variables$complete, 
          main = "Histogram of Moth count", 
          xlab = "Spongy moth", 
          ylab = "Frequency", 
          col = "lightblue", 
          border = "black")

#check to see the distribution of your 'clean_complete' count data
hist(complete_moth_2024_variables$clean_complete, 
     main = "Histogram of Moth count", 
     xlab = "Spongy moth", 
     ylab = "Frequency", 
     col = "lightgreen", 
     border = "black")


#Calculate the mean and standard deviation of 'complete' moth counts for each 
#level of stand_category to see if there are differences.

# Checking unique combinations
table(complete_moth_2024_variables$stand_category, 
      complete_moth_2024_variables$patch_name)

table(complete_moth_2024_variables$stand_type, 
      complete_moth_2024_variables$patch_name)


# Summary statistics for 'complete' moth count by stand_category and stand_type
summary_stats <- complete_moth_2024_variables %>%
  group_by(stand_category, stand_type, patch_name) %>%
  summarise(
    mean_count = mean(complete, na.rm = TRUE),
    sd_count = sd(complete, na.rm = TRUE),
    var_count = var(complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats, n=22)


# Summary statistics for 'complete' moth count by stand_category only
summary_stats_2 <- complete_moth_2024_variables %>%
  group_by(stand_category) %>%
  summarise(
    mean_count = mean(complete, na.rm = TRUE),
    sd_count = sd(complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_2, n=22)


# Summary statistics for 'complete' moth count by stand_type
summary_stats_3 <- complete_moth_2024_variables %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(complete, na.rm = TRUE),
    sd_count = sd(complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_3, n=22)

# Summary statistics for 'clean_complete' moth count by stand_type
summary_stats_4 <- complete_moth_2024_variables %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_4, n=22)

## Remove MOM row in 'complete' moth counts
moth_by_stand_summary_stats <- summary_stats_3[-c(1),]
print(moth_by_stand_summary_stats, n=22)

## Remove MOM row in 'clean_complete' moth counts
moth_by_stand_summary_stats_2 <- summary_stats_4[-c(1),]
print(moth_by_stand_summary_stats_2, n=22)


# Visualizations ----------------------------------------------------------

##visualize stand types for each patch separately
p <- ggplot(stand_category_filtered, aes(x = stand_type, y = clean_complete)) +
  geom_point() + 
  facet_wrap(~ patch_name)

print (p)

##separate Stand Type column so that we have a Stand ID for each stand in each patch
stand_ID_filtered <- stand_type_filtered %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, sep = "\\." ) %>% 
  glimpse()

stand_ID_filtered_1 <- stand_category_filtered %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, sep = "\\." ) %>% 
  glimpse()


##visualize stand types for each patch separately
p_1 <- ggplot(stand_ID_filtered, aes(x = stand_ID, y = clean_complete, colour = stand_type)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_1)

##visualize stand categories for each patch separately
p_2 <- ggplot(stand_ID_filtered_1, aes(x = stand_ID, y = clean_complete, 
                                       colour = stand_category)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_2)

# Random Effects Model ----------------------------------------------------
##Poisson, using all levels of data collection as a random effect 
model_inter_poisson <- glmer(clean_complete ~ (1|trap_name) + 
                               (1|stand_ID) + (1|patch_name), 
                  family =poisson(), data = stand_ID_filtered_1)
summary(model_inter_poisson)

##patch is the least variable among the random effects; individual traps
##are highly variable

check_overdispersion(model_inter_poisson)
check_model(model_inter_poisson)

##In this case, the Poisson and NB are the same, so can just use Poisson

##Negative binomial, with all levels, except the lowest (trap name)
##No need to use, as Poisson works well and isn't overdispered
##In this case, the Poisson and NB are the same, so can just use Poisson

model_inter_nb <- glmer.nb(clean_complete ~ (1|stand_ID)  + 
                             (1|patch_name), family =nbinom2(), 
                     data = stand_ID_filtered_1)
summary(model_inter_nb)

check_overdispersion(model_inter_nb)
check_model(model_inter_nb)


# Contrasts ---------------------------------------------------------------
## Use Polynomial Contrast, which is an appropriate option for determining an
##intercept when the x-axis follows a sequence (Oak to Pine proportions).
##calculates a global average and then measures polynomial contrasts to 1 degree
##less than the # of variables (for stand type=>3). Basically, can look at whether
##the intermediate compositions have more contrast to each other than to the
##extremes (x1 = L (how does increasing oak density increase moth), 
##x2 = Q (to what extent are intermediate more/less than pine), x3 = C 
##(do we see contrasting effects of oak/pine vs pine/oak))


## control/shift/M gives '%>%' in R
stand_ID_filtered$stand_type %>% unique() %>% dput()

ordered(stand_ID_filtered$stand_type, levels = c("Oak", "Oak/Pine", "Pine/Oak", 
                                                 "Pine"))

stand_ID_filtered$stand_type_ord <- ordered(stand_ID_filtered$stand_type, 
                                            levels = c("Oak", "Oak/Pine", 
                                                       "Pine/Oak", "Pine"))

model_inter_poisson_2 <- glmer(clean_complete ~ (1|trap_name) + (1|stand_ID) + 
                                 (1|patch_name) + stand_type_ord, 
                             family =poisson(), data = stand_ID_filtered)
summary(model_inter_poisson_2)

check_overdispersion(model_inter_poisson_2)
check_model(model_inter_poisson_2)


##using random slopes as well as an intercept to account for the fact that 
##there is an average effect of stand type BUT individual sites respond differently
##to it
model_inter_poisson_3 <- glmer(clean_complete ~ (1|trap_name) + (1|stand_ID) + 
                                 (1+stand_type_ord|patch_name) + stand_type_ord, 
                               family =poisson(), data = stand_ID_filtered)
summary(model_inter_poisson_3)



# By Month - Moth data separated by Month, not consolidated --------------------------

moth_2024_by_month <- read.csv("input/Moth_count_data_2024.csv")
#moth_count - count for either July or August collection, 2024

##To explore the distribution of your variables and count data like moth_count
# quick visualizations
dfSummary(moth_2024_by_month)
str(moth_2024_by_month)

#Remove MOM traps from either "stand type" or "stand category"
stand_type_filtered_by_month <- moth_2024_by_month %>%
  filter(stand_type != "MOM")
stand_category_filtered_by_month <- moth_2024_by_month %>%
  filter(stand_category != "MOM")

# Data Summaries ----------------------------------------------------------

#check to see the distribution of your 'moth_count' count data
hist(moth_2024_by_month$moth_count, 
     main = "Histogram of Moth count", 
     xlab = "Spongy moth", 
     ylab = "Frequency", 
     col = "purple", 
     border = "black")

#Calculate the mean and standard deviation of moth counts for each level of 
#stand_category to see if there are differences.

# Checking unique combinations
table(moth_2024_by_month$stand_category, 
      moth_2024_by_month$patch_name)

table(moth_2024_by_month$stand_type, 
      moth_2024_by_month$patch_name)

# Summary statistics for moth count by stand_category and stand_type
summary_stats_3 <- moth_2024_by_month %>%
  group_by(stand_category, stand_type, patch_name) %>%
  summarise(
    mean_count = mean(moth_count, na.rm = TRUE),
    sd_count = sd(moth_count, na.rm = TRUE),
    var_count = var(moth_count, na.rm = TRUE),
    count = n()
  )

print(summary_stats_3, n=22)

# Summary statistics for moth count by stand_category only
summary_stats_4 <- moth_2024_by_month %>%
  group_by(stand_category) %>%
  summarise(
    mean_count = mean(moth_count, na.rm = TRUE),
    sd_count = sd(moth_count, na.rm = TRUE),
    count = n()
  )

print(summary_stats_4, n=22)


# Summary statistics for 'complete' moth count by stand_type
summary_stats_3 <- complete_moth_2024_variables %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(complete, na.rm = TRUE),
    sd_count = sd(complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_3, n=22)

# Summary statistics for 'clean_complete' moth count by stand_type
summary_stats_4 <- complete_moth_2024_variables %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_4, n=22)

# Visualizations ----------------------------------------------------------

##visualize stand types for each patch separately
p_month <- ggplot(stand_category_filtered_by_month, aes(x = stand_type, 
                                                        y = moth_count)) + 
  geom_point() + 
  facet_wrap(~ patch_name)

print (p_month)

##separate Stand Type column so that we have a Stand ID for each stand in each patch
stand_ID_filtered_by_month <- stand_type_filtered_by_month %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, 
           sep = "\\." ) %>% 
  glimpse()

stand_ID_filtered_by_month_1 <- stand_category_filtered_by_month %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, 
           sep = "\\." ) %>% 
  glimpse()

##visualize stand types for each patch separately
p_month_1 <- ggplot(stand_ID_filtered_by_month, aes(x = stand_ID, 
          y = moth_count, colour = stand_type)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_month_1)

##visualize stand categories for each patch separately
p_month_2 <- ggplot(stand_ID_filtered_by_month_1, aes(x = stand_ID, 
            y = moth_count, colour = stand_category)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_month_2)



# Previous data and models exploration ------------------------------------



##visualize % oak and pine for each patch separately
p_3 <- ggplot(stand_ID_filtered_1, aes(x = Percent_Oak, y = Percent_Pine, colour = stand_category, size = clean_complete)) +
  geom_point() + 
  facet_wrap(~ patch_name)

print (p_3)


#inspect visually by Stand Category, no MOM's
p1 <- ggplot(stand_category_filtered, aes(x = stand_category, y = clean_complete)) +
      geom_boxplot() +
      labs(title = "Distribution of Moth Counts by Stand Category",
       x = "Stand Category",
       y = "Moth Count") +
      theme_minimal()

print(p1)

##Stand_type rather than category, no MOM's 
p2 <- ggplot(stand_type_filtered, aes(x = stand_type, y = clean_complete)) +
  geom_boxplot() +
  labs(title = "Distribution of Moth Counts by Stand Type",
       x = "Stand Type",
       y = "Moth Count") +
  theme_minimal()

print(p2)


##'clean_complete' moth count means without MOMs
p3 <- ggplot(moth_by_stand_summary_stats_2, aes(x = stand_type, y = mean_count)) +
  geom_boxplot() +
  labs(title = "Distribution of Moth Counts by Stand Type",
       x = "Stand Type",
       y = "Moth Count") +
  theme_minimal()

print(p3)


p4 <- ggplot(moth_by_stand_summary_stats_2, aes(x=stand_type), fill=class) + 
  geom_boxplot(aes(lower=mean_count-sd_count , upper=mean_count+sd_count , 
                   middle=mean_count , ymin=mean_count-3*sd_count , ymax=mean_count+3*sd_count),
               stat="identity") +
  labs(title = "Distribution of Moth Counts by Stand Type",
     x = "Stand Type",
     y = "Mean Moth Count") +
theme_classic()
  
print(p4)

#Convert stand_category and patch_name into factors
complete_moth_2024_variables$patch_name <- as.factor(complete_moth_2024_variables$patch_name)
complete_moth_2024_variables$stand_type <- as.factor(complete_moth_2024_variables$stand_type)
stand_type_filtered$patch_name <- as.factor(stand_type_filtered$patch_name)
stand_type_filtered$stand_type <- as.factor(stand_type_filtered$stand_type)

dfSummary(complete_moth_2024_variables)
str(complete_moth_2024_variables)

dfSummary(stand_type_filtered)
str(stand_type_filtered)

####### Stand_type model
model <- glm(clean_complete ~ stand_type, family = poisson, data = stand_type_filtered)
summary(model)

check_overdispersion(model)
check_model(model)

###running glm model, family=poisson, using Patch as a 'random effect'

#for stand type
model_1 <- glmer(clean_complete ~ stand_type + (1|patch_name), family =poisson, data = stand_type_filtered)
summary(model_1)
### AIC=3875, variance in patch_name (random factor) = 0.286 

#for stand category
model_2 <- glmer(clean_complete ~ stand_category + (1|patch_name), family =poisson, data = stand_type_filtered)
summary(model_2)
### AIC=3696, variance in patch_name (random factor) = 0.275 


## Check model performance via package "performance"
### To check model assumptions, overdispersion and and model fitness (https://easystats.github.io/performance/)

check_overdispersion(model_1)
###Overdispersion, should be 1 or around 1 for good fit. 
#data is overdispersed

check_model(model_1)  ### for plotting model residuals: can inspect the messages for each residual plot
                      ### if it follows the instruction on top of each plot, your model is a good fit.

check_overdispersion(model_2)
#data is overdispersed

check_model(model_2)  


### Try both with negative binomial and random effect of patch name
model_1a <- glmer.nb(clean_complete ~ stand_type + (1|patch_name), family =nbinom2(), data = stand_type_filtered)
summary(model_1a)

# Perform Type III ANOVA (Sum of Squares)
##Use Type III SS when you want to test the significance of predictors in a model with multiple factors, especially when dealing with unbalanced designs.
##Type III sum of squares are “partial.”  In essence, every term in the model is tested in light of every other term in the model, meaning that main effects are tested in light of interaction terms as well as in light of other main effects. 
anova_results <- Anova(model_1a, type = "III")
summary(anova_results)

check_overdispersion(model_1a)
check_model(model_1a)
#AIC = 1213, variance by patch name = 0.228, and no overdispersion 
####MUCH lower AIC in the neg binomial compared to poisson model, variance about the same

model_2a <- glmer.nb(clean_complete ~ stand_category + (1|patch_name), family =nbinom2(), data = stand_type_filtered)
summary(model_2a)

anova_2a_results <- Anova(model_2a, type = "III")
summary(anova_2a_results)

check_overdispersion(model_2a)
check_model(model_2a)
#AIC = 1222, variance by patch name = 0.256, and no overdispersion 
####MUCH lower AIC in the neg binomial compared to poisson model, variance about the same


####### Patch_name Model with Stand Type as a random effect
model_3 <- glmer.nb(clean_complete ~ patch_name + (1|stand_type), family = nbiom2(), data = stand_type_filtered)
summary(model_3)
#AIC = 1206, variance by stand type = 0.0175, and no overdispersion 

anova_3_results <- Anova(model_3, type = "III")
summary(anova_3_results)

check_overdispersion(model_3)
check_model(model_3)

#If there are more than one variables being examined, check for Multicollinearity:
#With categorical variables like patch_name with many levels, multicollinearity can be an issue. 
#Use the car package to check Variance Inflation Factors (VIFs): A VIF of 1 means there is no correlation, 1-5 is moderate (usually acceptable), higher values indicate greater correlation
 
vif(model_1)
vif(model_1a)
vif(model_2)

####### Patch_name Model, not with random effect - keeping all the factors the same
#model_3a <- glmer.nb(clean_complete ~ patch_name + (patch_name | trap_name), family = nbiom2(), data = complete_moth_2024)

#check_overdispersion(model_3a)
#check_model(model_3a)

###Visualize Fixed Effects:
### To better understand the effects of each patch, visualize the estimates with confidence intervals:
##Use library(effects)

plot(allEffects(model))
plot(allEffects(model_1))
plot(allEffects(model_1a))
plot(allEffects(model_1b))
plot(allEffects(model_2))
plot(allEffects(model_2a))
plot(allEffects(model_2b))

plot(allEffects(model_3))

### For better visualization of the same plot,via ggplot2, gather all the attributes manually
# 1.Extract the fixed effects estimates
fixed_effects <- summary(model_1a)$coefficients
fixed_effects_2 <- summary(model_2a)$coefficients

# 2.Convert it to a data frame for easier manipulation
fixed_effects_df <- as.data.frame(fixed_effects)
fixed_effects_2_df <- as.data.frame(fixed_effects_2)

# 3. Adding the row names as a column for patch names
fixed_effects_df$patch_name <- rownames(fixed_effects_df)
fixed_effects_2_df$patch_name <- rownames(fixed_effects_2_df)

# 4.Rename columns for easier access
colnames(fixed_effects_df) <- c("Estimate", "Std_Error", "z_value", "p_value", "patch_name")
colnames(fixed_effects_2_df) <- c("Estimate", "Std_Error", "z_value", "p_value", "patch_name")

# 5. Calculate the 95% confidence intervals
fixed_effects_df <- fixed_effects_df %>%
  mutate(
    CI_Lower = Estimate - 1.96 * Std_Error,
    CI_Upper = Estimate + 1.96 * Std_Error
  )
fixed_effects_2_df <- fixed_effects_2_df %>%
  mutate(
    CI_Lower = Estimate - 1.96 * Std_Error,
    CI_Upper = Estimate + 1.96 * Std_Error
  )

# Replace patch_name values back to the original names (if needed)
# 6. Create a lookup table that matches the shortened names to the full names as they appear in the data
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
fixed_effects_2_df$patch_name <- patch_name_lookup[fixed_effects_2_df$patch_name]

# h. Calculate the 95% confidence intervals
fixed_effects_df <- fixed_effects_df %>%
  mutate(
    CI_Lower = Estimate - 1.96 * Std_Error,
    CI_Upper = Estimate + 1.96 * Std_Error
  )
fixed_effects_2_df <- fixed_effects_2_df %>%
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
# above 0 are the positive estimates with greater moth counts 

print(p5)


p6 <- ggplot(fixed_effects_2_df, aes(x = reorder(patch_name, Estimate), y = Estimate)) +
  geom_point(size = 3, color = "purple") +  # Plot points for estimates 
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "purple") +  # Error bars for CIs
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
# above 0 are the positive estimates with greater moth counts 

print(p6)




ggsave("stand composition Effecting Moth's Population.png", plot = p3, width = 10, height = 8, dpi = 300)


p7 <- ggplot(stand_type_filtered, aes(x= patch_name, 
                            y= clean_complete, 
                            group = trap_name))+
  geom_smooth(method = "lm", 
              se= FALSE, 
              linewidth = 0.5, 
              colour ="grey")+
  geom_point(shape = 21)

print(p7)

p8 <- ggplot(stand_type_filtered, aes(x= stand_type, 
                                     y= clean_complete, 
                                     group = trap_name))+
  geom_smooth(method = "lm", 
              se= FALSE, 
              linewidth = 0.5, 
              colour ="grey")+
  geom_point(shape = 21)

print(p8)




##other models to try, with additional variables 
model_4 <- glmer.nb(clean_complete ~ stand_type + landscape_type + (1|patch_name), family =nbinom2(), data = stand_type_filtered)
summary(model_4)

anova_4_results <- Anova(model_4, type = "III")
summary(anova_4_results)

check_overdispersion(model_4)
check_model(model_4)

model_5 <- glmer.nb(clean_complete ~ patch_name + landscape_type + (1|stand_type), family = nbiom2(), data = stand_type_filtered)
summary(model_5)

anova_5_results <- Anova(model_5, type = "III")
summary(anova_5_results)

check_overdispersion(model_5)
check_model(model_5)


##Poisson 
AIC(model_1)
AIC(model_2)

##Negative Binomial
AIC(model_2a)
AIC(model_1a)
AIC(model_3)
AIC(model_4)
AIC(model_5)

###In Summary: Negative Binomial models appear to have lower AIC and no overdispersion, compared to Poisson models



