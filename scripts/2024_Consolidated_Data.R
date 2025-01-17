
#### 17-01-25

rm(list = ls()) # to clear the environment

complete_moth_2024 <- read.csv("input/2024_consolidated_moth_counts.csv")
#complete - consolidation of July and Aug moth counts, 2024
#clean_complete - only includes traps that had clean counts for BOTH July and Aug


##To explore the distribution of your variables and count data like moth_count
# quick visualizations
dfSummary(complete_moth_2024)
str(complete_moth_2024)

#check to see the distribution of your 'complete' count data
hist(complete_moth_2024$complete, 
          main = "Histogram of Moth count", 
          xlab = "Spongy moth", 
          ylab = "Frequency", 
          col = "lightblue", 
          border = "black")

#check to see the distribution of your 'clean_complete' count data
hist(complete_moth_2024$clean_complete, 
     main = "Histogram of Moth count", 
     xlab = "Spongy moth", 
     ylab = "Frequency", 
     col = "lightgreen", 
     border = "black")


#Calculate the mean and standard deviation of 'complete' moth counts for each level of stand_category to see if there are differences.

# Checking unique combinations
table(complete_moth_2024$stand_category, complete_moth_2024$patch_name)

table(complete_moth_2024$stand_type, complete_moth_2024$patch_name)


# Summary statistics for 'complete' moth count by stand_category and stand_type
summary_stats <- complete_moth_2024 %>%
  group_by(stand_category, stand_type) %>%
  summarise(
    mean_count = mean(complete, na.rm = TRUE),
    sd_count = sd(complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats, n=22)


# Summary statistics for 'complete' moth count by stand_category only
summary_stats_2 <- complete_moth_2024 %>%
  group_by(stand_category) %>%
  summarise(
    mean_count = mean(complete, na.rm = TRUE),
    sd_count = sd(complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_2, n=22)


# Summary statistics for 'complete' moth count by stand_type
summary_stats_3 <- complete_moth_2024 %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(complete, na.rm = TRUE),
    sd_count = sd(complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_3, n=22)

# Summary statistics for 'clean_complete' moth count by stand_type
summary_stats_4 <- complete_moth_2024 %>%
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


#inspect visually by Stand Category
p1 <- ggplot(complete_moth_2024, aes(x = stand_category, y = clean_complete)) +
      geom_boxplot() +
      labs(title = "Distribution of Moth Counts by Stand Category",
       x = "Stand Category",
       y = "Moth Count") +
      theme_minimal()

print(p1)


##Stand_type rather than category. 
p2 <- ggplot(complete_moth_2024, aes(x = stand_type, y = clean_complete)) +
  geom_boxplot() +
  labs(title = "Distribution of Moth Counts by Stand Type",
       x = "Stand Type",
       y = "Moth Count") +
  theme_minimal()

print(p2)

##'clean_complete' moth counts without MOMs
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

#Convert stand_category and patch name into factors
complete_moth_2024$patch_name <- as.factor(complete_moth_2024$patch_name)
complete_moth_2024$stand_type <- as.factor(complete_moth_2024$stand_type)

dfSummary(complete_moth_2024)
str(complete_moth_2024)

#running glm model, family=poisson, as suggested by Sabina, using patches to create a block effect, removing complexity as needed from one model to the next...

#for stand type
model_1 <- glmer(clean_complete ~ patch_name * stand_type + (1|trap_name), family =poisson, data = complete_moth_2024)
summary(model_1)
### AIC=1205, variance in your trap_name (random factor) = 0.3141 

#for stand category
model_2 <- glmer(clean_complete ~ patch_name * stand_category + (1|trap_name), family =poisson, data = complete_moth_2024)
summary(model_2)
### AIC=1198, variance in your trap_name (random factor) = 0.2476 


## Check model performance via package "performance"
### To check model assumptions, overdispersion and and model fitness (https://easystats.github.io/performance/)

check_overdispersion(model_1)
###Overdispersion, should be 1 or around 1 for good fit. 
check_model(model_1)  ### for plotting model residuals: can inspect the messages for each residual plot
                      ### if it follows the instruction on top of each plot, your model is a good fit.

check_overdispersion(model_2)
check_model(model_1)  


### Try both with negative binomial and random effect of trap name
model_1a <- glmer.nb(clean_complete ~ stand_type + (1|trap_name), family =nbinom2(), data = complete_moth_2024)
summary(model_1a)

check_overdispersion(model_1a)
check_model(model_1a)
#higher AIC, variance by trap name, and dispersion ratio compared to poisson model

model_2a <- glmer.nb(clean_complete ~ stand_category + (1|trap_name), family =nbinom2(), data = complete_moth_2024)
summary(model_2a)

check_overdispersion(model_2a)
check_model(model_2a)
#higher AIC, variance by trap name, and dispersion ratio compared to poisson model


####### Stand_type model
model_1b <- glm(clean_complete ~ stand_type, family = poisson, data = complete_moth_2024)
summary(model_1b)

check_overdispersion(model_1b)
check_model(model_1b)

### Try both with negative binomial and random effect of patch name
model_1c <- glmer.nb(clean_complete ~ stand_type + (1|patch_name), family = nbinom2(), data = complete_moth_2024)
summary(model_1c)

check_overdispersion(model_1c)
check_model(model_1c)

model_2c <- glmer.nb(clean_complete ~ stand_category + (1|patch_name), family = nbinom2(), data = complete_moth_2024)
summary(model_2c)

check_overdispersion(model_2c)
check_model(model_2c)


####### Patch_name Model with random effect
model_3 <- glmer.nb(clean_complete ~ patch_name + (1|trap_name), family = nbiom2(), data = complete_moth_2024)
summary(model_3)

check_overdispersion(model_3)
check_model(model_3)

#Check for Multicollinearity:
#With categorical variables like patch_name with many levels, multicollinearity can be an issue. 
#Use the car package to check Variance Inflation Factors (VIFs): A VIF of 1 means there is no correlation, 1-5 is moderate (usually acceptable), higher values indicate greater correlation
 
vif(model_1)
vif(model_2)

####### Patch_name Model, not with random effect - keeping all the factors the same
#model_3a <- glmer.nb(clean_complete ~ patch_name + (patch_name | trap_name), family = nbiom2(), data = complete_moth_2024)

#check_overdispersion(model_3a)
#check_model(model_3a)

###Visualize Fixed Effects:
### To better understand the effects of each patch, visualize the estimates with confidence intervals:
##Use library(effects)

plot(allEffects(model_1a))
plot(allEffects(model_1b))
plot(allEffects(model_1c))

plot(allEffects(model_2a))
plot(allEffects(model_2c))

plot(allEffects(model_3))

### For better visualization of the same plot,via ggplot2, gather all the attributes manually
# 1.Extract the fixed effects estimates
fixed_effects <- summary(model_1)$coefficients
fixed_effects_2 <- summary(model_2)$coefficients

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


p7 <- ggplot(complete_moth_2024, aes(x= patch_name, 
                            y= clean_complete, 
                            group = trap_name))+
  geom_smooth(method = "lm", 
              se= FALSE, 
              linewidth = 0.5, 
              colour ="grey")+
  geom_point(shape = 21)

print(p7)

p8 <- ggplot(complete_moth_2024, aes(x= stand_type, 
                                     y= clean_complete, 
                                     group = trap_name))+
  geom_smooth(method = "lm", 
              se= FALSE, 
              linewidth = 0.5, 
              colour ="grey")+
  geom_point(shape = 21)

print(p8)



 




model_3 <- glmer(clean_complete ~ stand_category + patch_name + (1|trap_name), family =poisson, data = complete_moth_2024)

model_4 <- glmer(clean_complete ~ stand_category + (1|trap_name), family =poisson, data = complete_moth_2024)

model_5 <- glmer(clean_complete ~ stand_type + patch_name + (1|trap_name), family =poisson, data = complete_moth_2024)

model_6 <- glmer(clean_complete ~ stand_type + (1|trap_name), family =poisson, data = complete_moth_2024)


summary(model_1)
AIC(model_1)

summary(model_2)
AIC(model_2)

summary(model_3)
AIC(model_3)

summary(model_4)
AIC(model_4)

summary(model_5)
AIC(model_5)

summary(model_6)
AIC(model_6)


