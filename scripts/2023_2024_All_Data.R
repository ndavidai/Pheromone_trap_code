
#### 21-02-25

rm(list = ls()) # to clear the environment

#Run this to load packages 
source("2024_code/scripts/Packages.R")


complete_2023_2024 <- read.csv("input/2023_2024_all_moth_counts.csv")
#clean_complete - full summer complete moth counts available for 2023 and 2024


# Data Cleaning -----------------------------------------------------------


##To explore the distribution of your variables and count data like clean_complete
# quick visualizations
dfSummary(complete_2023_2024)
str(complete_2023_2024)

## change 'Year' from int to chr
complete_2023_2024$Year <- as.character(complete_2023_2024$Year)

str(complete_2023_2024)

# remove space in trap_name
# replace '-' with '_' in trap_name
complete_2023_2024 <- complete_2023_2024 %>%
  mutate(trap_name = str_replace(trap_name, " ", ""))
complete_2023_2024 <- complete_2023_2024 %>%
  mutate(trap_name = str_replace(trap_name, "-", "_"))

#Remove MOM traps from either "stand type" or "stand category"
stand_type_filtered <- complete_2023_2024 %>%
  filter(stand_type != "MOM")
stand_category_filtered <- complete_2023_2024 %>%
  filter(stand_category != "MOM")



# looking for mistakes
unique(complete_2023_2024$stand_type)
unique(complete_2023_2024$patch_name)
unique(complete_2023_2024$Year)
unique(complete_2023_2024$landscape_type)
unique(complete_2023_2024$stand_category)

unique(stand_type_filtered$stand_type)
unique(stand_category_filtered$stand_category)



# Data Summaries ----------------------------------------------------------

#check to see the distribution of moth count data
hist(complete_2023_2024$clean_complete, 
          main = "Histogram of Moth count", 
          xlab = "Spongy moth", 
          ylab = "Frequency", 
          col = "darkblue", 
          border = "black")


#Calculate the mean and standard deviation of moth counts for each 
#level of stand_category to see if there are differences.

# Checking unique combinations

table(stand_category_filtered$stand_category, 
      stand_category_filtered$patch_name)

table(stand_type_filtered$stand_type, 
      stand_type_filtered$patch_name)


# Summary statistics for moth count by stand_category and stand_type
summary_stats <- stand_type_filtered %>%
  group_by(stand_category, stand_type, patch_name) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    var_count = var(clean_complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats, n=22)


# Summary statistics moth count by stand_category only
summary_stats_2 <- stand_category_filtered %>%
  group_by(stand_category) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_2, n=22)


# Summary statistics for moth count by stand_type
summary_stats_3 <- stand_type_filtered %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_3, n=22)


# Summary statistics for moth count by patch
summary_stats_4 <- stand_type_filtered %>%
  group_by(patch_name) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_4, n=22)

# Summary statistics for moth count by stand_type and patch
summary_stats_5 <- stand_type_filtered %>%
  group_by(stand_type, patch_name) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    var_count = var(clean_complete, na.rm = TRUE),
    count = n()
  )

print(summary_stats_5, n=22)

## Remove MOM row in 'complete' moth counts
moth_by_stand_summary_stats <- summary_stats_3[-c(1),]
print(moth_by_stand_summary_stats, n=22)

## Remove MOM row in 'clean_complete' moth counts
moth_by_stand_summary_stats_2 <- summary_stats_4[-c(1),]
print(moth_by_stand_summary_stats_2, n=22)

###NEED TO SAVE AND EXPORT THESE SUMMARY TABLES###


# Visualizations ----------------------------------------------------------

##separate Stand Type column so that we have a Stand ID for each stand in each patch
stand_ID_filtered <- stand_type_filtered %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, sep = "\\." ) %>% 
  glimpse()

stand_ID_filtered_1 <- stand_category_filtered %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, sep = "\\." ) %>% 
  glimpse()

##Order Stand Types so shown on X-axis in order of decreasing oak
stand_ID_filtered$stand_type %>% unique() %>% dput()

ordered(stand_ID_filtered$stand_type, levels = c("Oak", "Oak/Pine", "Oak/Other", 
                                                 "Pine/Oak", "Pine", "Other"))

stand_ID_filtered$stand_type_ord <- ordered(stand_ID_filtered$stand_type, 
                                      levels = c("Oak", "Oak/Pine", "Oak/Other", 
                                                "Pine/Oak", "Pine", "Other"))
######continue from here...
##visualize stand types for each patch separately
p <- ggplot(stand_ID_filtered, aes(x = stand_type, y = clean_complete)) +
  geom_point(stat = "identity") + 
  facet_wrap(~ patch_name)

print (p)


##visualize stand types, by patch ID, for each patch separately
p_1 <- ggplot(stand_ID_filtered, aes(x = stand_ID, y = clean_complete, colour = stand_type)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_1)

##visualize stand categories, by patch ID, for each patch separately
p_2 <- ggplot(stand_ID_filtered_1, aes(x = stand_ID, y = clean_complete, 
                                       colour = stand_category)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_2)

##visualize stand types for each patch separately
p_3 <- ggplot(stand_ID_filtered, aes(x = stand_type, y = clean_complete, colour = stand_type)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_3)

##visualize stand categories for each patch separately
p_4 <- ggplot(stand_ID_filtered_1, aes(x = stand_category, y = clean_complete, 
                                       colour = stand_category)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name, scales = "free_x")

print (p_4)


# Random Effects Model ----------------------------------------------------

####NOT WORKING - Warning messages (Hessian) - need to figure out what's going
##on with the model
##Poisson, using all levels of data collection as a random effect 
model_complete_poisson <- glmer(clean_complete ~ (1|trap_name) + 
                               (1|stand_ID) + (1|patch_name), 
                  family =poisson(), data = stand_ID_filtered)
summary(model_complete_poisson)



check_overdispersion(model_complete_poisson)
check_model(model_complete_poisson)


##Negative binomial, with all levels, except the lowest (trap name)
##Worked, in comparison to Poisson model, but still gave a message of 
##having 50 or more warnings
##If Poisson and NB are the same, can just use Poisson

model_complete_nb <- glmer.nb(clean_complete ~ (1|stand_ID)  + 
                             (1|patch_name), family =nbinom2(), 
                     data = stand_ID_filtered)
summary(model_complete_nb)

check_overdispersion(model_complete_nb)
check_model(model_complete_nb)


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

##ALSO NOT WORKING!!!

model_complete_poisson_2 <- glmer(clean_complete ~ (1|trap_name) + (1|stand_ID) + 
                                 (1|patch_name) + stand_type_ord, 
                             family =poisson(), data = stand_ID_filtered)
summary(model_complete_poisson_2)

check_overdispersion(model_complete_complete_2)
check_model(model_complete_poisson_2)


##using random slopes as well as an intercept to account for the fact that 
##there is an average effect of stand type BUT individual sites respond differently
##to it - this is important because the effect of stand type clearly varies from
##patch to patch
model__complete_3 <- glmer(clean_complete ~ (1|trap_name) + (1|stand_ID) + 
                                 (1+stand_type_ord|patch_name) + stand_type_ord, 
                               family =poisson(), data = stand_ID_filtered)
summary(model_complete_poisson_3)


