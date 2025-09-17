
#### 21-02-25

library(tidyverse)
library(lme4)
library(performance)
library(summarytools)
library(ggeffects)
library(marginaleffects)
library(brms)
library(car)
library(viridis)
library(broom)
library(knitr)
library(dplyr)
library(kableExtra)
library(gt)
library(modelsummary)
library(sjPlot)


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
          main = " ", 
          xlab = "Spongy moth count/trap", 
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

# Set the levels of 'stand_type' to ensure the correct order
stand_type_filtered$stand_type <- factor(stand_type_filtered$stand_type, 
                              levels = c("Oak", "Oak/Pine", "Oak/Other", 
                                  "Pine/Oak", "Pine", "Other"))

# Create the 'stand-type by patch' table
table(stand_type_filtered$stand_type, stand_type_filtered$patch_name)

# Create the contingency table
contingency_table <- table(stand_type_filtered$stand_type, stand_type_filtered$patch_name)

# Convert the table to a data frame
contingency_df <- as.data.frame(contingency_table)


# Heatmap stands by patch -------------------------------------------------

# Create a plot (heatmap) of the contingency table
contingency_df 

ggplot(contingency_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(x = "Stand Type", y = "Patch Name", fill = "Frequency") +
  theme_minimal()

# Save the plot as an image (e.g., PNG)
#ggsave("contingency_table_heatmap.png", width = 7.5, height = 6)

#convert the data in the heatmap plot to a table format
wide_table <- contingency_df %>%
  pivot_wider(names_from = Var1, values_from = Freq, values_fill = 0)

wide_table <- wide_table %>%
  mutate(
    Var2 = trimws(as.character(Var2)),
    Var2 = dplyr::recode(Var2,
                          "Mont_Gauvin/Glen" = "Mont Gauvin",
                          "Mont_Royal" = "Mont Royal",
                          "Mont_Saint_Bruno" = "Mont Saint Bruno",
                          "Mont_Saint_Hilaire" = "Mont Saint Hilaire",
                          "Notre_Dame_de_Bonsecours" = "Notre Dame de Bonsecours",
                          "Oka" = "Parc Oka",
                          "Orford" = "Mont Orford",
                          "Parc_Michel_Chartrand" = "Parc Michel Chartrand",
                          "Parc_Pointe_aux_Prairies" = "Parc Pointe aux Prairies",
                          "Rigaud" = "Mont Rigaud",
                         ))


##print(wide_table)

patch_order <- c("Papineauville", "Montebello", "Notre Dame de Bonsecours",
                 "Kenauk", "Brownsburg", "Mont Rigaud", "Parc Oka",
                 "Mont Royal",
                 "Parc Pointe aux Prairies", "Parc Michel Chartrand",
                 "Mont Saint Bruno", "Mont Saint Hilaire","Mont Gauvin",
                 "Mont Orford", "Hatley")
wide_table <- wide_table %>%
  mutate(Var2 = factor(Var2, levels = patch_order)) %>%
  arrange(Var2)

##print(wide_table)

gt_table <- wide_table %>%
  gt() %>%
  cols_label(
    Var2 = "Patch Name"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 14
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(  # Custom striping on even-numbered rows
    style = cell_fill(color = "#f9f9f0"),  # You can change this color
    locations = cells_body(rows = seq(2, nrow(wide_table), 2))
  )

##gtsave(gt_table, "heatmap_table.png")      # Image
##gtsave(gt_table, "heatmap_table.html")     # Web preview

version$version.string





# Summary statistics for moth count by stand_category and stand_type
summary_stats <- stand_type_filtered %>%
  group_by(stand_category, stand_type, patch_name) %>%
  summarise(
    mean_count = mean(total_moth_count, na.rm = TRUE),
    sd_count = sd(total_moth_count, na.rm = TRUE),
    var_count = var(total_moth_count, na.rm = TRUE),
    count = n()
  )

##print(summary_stats, n=22)


# Summary statistics moth count by stand_category only
# summary_stats_2 <- stand_category_filtered %>%
#   group_by(stand_category) %>%
#   summarise(
#     mean_count = mean(clean_complete, na.rm = TRUE),
#     sd_count = sd(clean_complete, na.rm = TRUE),
#     count = n()
#   )
# 
# print(summary_stats_2, n=22)


# Table to use -----------------------------------------------------------

# Summary statistics for moth count by stand_type, differentiating between
##traps set and traps with usable data
summary_stats_3 <- stand_type_filtered %>%
  group_by(stand_type) %>%
  summarise(
    mean_count = mean(clean_complete, na.rm = TRUE),
    sd_count = sd(clean_complete, na.rm = TRUE),
    n_traps = n(),
    n_obs = sum(!is.na (clean_complete))
  )

print(summary_stats_3, n=22)

# Round numeric columns to 2 decimal places
summary_stats_3$mean_count <- round(summary_stats_3$mean_count, 2)
summary_stats_3$sd_count <- round(summary_stats_3$sd_count, 2)
summary_stats_3$count <- round(summary_stats_3$n_obs, 2)


# Convert the table to a data frame
#summary_table <- as.data.frame(summary_stats_3)

# Save it as a CSV file
#write.csv(summary_table, file = "Summary Stats by Stand Type.csv", 
          #row.names = FALSE)


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
# summary_stats_5 <- stand_type_filtered %>%
#   group_by(stand_type, patch_name) %>%
#   summarise(
#     mean_count = mean(clean_complete, na.rm = TRUE),
#     sd_count = sd(clean_complete, na.rm = TRUE),
#     var_count = var(clean_complete, na.rm = TRUE),
#     n_traps = n(),
#     n_obs = sum(!is.na (clean_complete))
#     
#   )
# 
# print(summary_stats_5, n=22)

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
# p <- ggplot(stand_ID_filtered, aes(x = stand_type, y = clean_complete)) +
#   geom_point(stat = "identity") + 
#   facet_wrap(~ patch_name)
# 
# print (p)


# Graph to use ------------------------------------------------------------


##visualize moth counts by stand types, for each patch separately
p_1 <- ggplot(stand_ID_filtered, aes(x = stand_type_ord, y = clean_complete, 
                                       colour = stand_type)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  facet_wrap(~ patch_name) +
  labs(x = "Stand Type", y = "Total Moth Counts", fill = "Stand Type") +
  theme(axis.text.x = element_blank(),
  axis.ticks.x = element_blank())
  # Remove x-axis labels and ticks from the individual facets
  
print (p_1)

# Save the plot as an image (e.g., PNG)
#ggsave("moth counts by stand type.png", width = 9, height = 7)

##visualize stand types, by patch ID, for each patch separately
# p_1.1 <- ggplot(stand_ID_filtered, aes(x = stand_ID, y = clean_complete, 
#                                      colour = stand_type)) +
#   geom_point(position = position_jitter(height = 0, width = 0.1)) + 
#   facet_wrap(~ patch_name, scales = "free_x")
# 
# print (p_1.1)


##visualize stand categories, by patch ID, for each patch separately
# p_2 <- ggplot(stand_ID_filtered_1, aes(x = stand_ID, y = clean_complete, 
#                                        colour = stand_category)) +
#   geom_point(position = position_jitter(height = 0, width = 0.1)) + 
#   facet_wrap(~ patch_name, scales = "free_x")
# 
# print (p_2)

##visualize stand types for each patch separately
# p_3 <- ggplot(stand_ID_filtered, aes(x = stand_type, y = clean_complete, colour = stand_type)) +
#   geom_point(position = position_jitter(height = 0, width = 0.1)) + 
#   facet_wrap(~ patch_name, scales = "free_x")
# 
# print (p_3)

##visualize stand categories for each patch separately
# p_4 <- ggplot(stand_ID_filtered_1, aes(x = stand_category, y = clean_complete, 
#                                        colour = stand_category)) +
#   geom_point(position = position_jitter(height = 0, width = 0.1)) + 
#   facet_wrap(~ patch_name, scales = "free_x")
# 
# print (p_4)


# Random Effects Model ----------------------------------------------------

##Poisson, using all levels of data collection as a random effect 
# model_complete_poisson <- glmer(
#   round(clean_complete) ~ (1|trap_name) + 
#     (1|stand_ID) + (1|patch_name), 
#   family =poisson(), data = stand_ID_filtered)
# summary(model_complete_poisson)
# 
# 
# performance::check_overdispersion(model_complete_poisson)
# performance::check_model(model_complete_poisson)



# Oak and Pine Models -----------------------------------------------------

#Poisson, using all levels of data collection as a random effect, except Oak
#which is being fitted as a fixed effect
model_complete_poisson_oak <- glmer(
  round(clean_complete) ~ (1|trap_name) +
    (Percent_Oak) + (1|patch_name),
  family =poisson(), data = stand_ID_filtered)
summary(model_complete_poisson_oak)


performance::check_overdispersion(model_complete_poisson_oak)
performance::check_model(model_complete_poisson_oak)

#Poisson, using all levels of data collection as a random effect, except Pine
#which is being fitted as a fixed effect
model_complete_poisson_pine <- glmer(
  round(clean_complete) ~ (1|trap_name) +
    (Percent_Pine) + (1|patch_name),
  family =poisson(), data = stand_ID_filtered)
summary(model_complete_poisson_pine)


performance::check_overdispersion(model_complete_poisson_pine)
performance::check_model(model_complete_poisson_pine)

#model for Oak and Pine together
model_both <- glmer(round(clean_complete) ~ Percent_Pine + Percent_Oak + 
                      (1 | trap_name) + (1 | patch_name), 
                    data = stand_ID_filtered, family = poisson)

summary(model_both)
performance::check_overdispersion(model_both)

#model for Oak and Pine together, adding an interaction of oak & pine
model_both_2 <- glmer(round(clean_complete) ~ Percent_Pine + Percent_Oak + 
                      (1 | trap_name) + (1 | patch_name) + 
                      (Percent_Pine * Percent_Oak), 
                    data = stand_ID_filtered, family = poisson)

summary(model_both_2)
performance::check_overdispersion(model_both_2)



ggplot(stand_ID_filtered, aes(x = Percent_Oak, y = clean_complete)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), 
              se = TRUE, color = "darkgreen") +
  labs(
    title = "Effect of Percent Oak on Moth Counts",
    x = "% Oak Cover",
    y = "Moth Count"
  ) +
  theme_minimal()


# Checking Maple ----------------------------------------------------------

#Poisson, using all levels of data collection as a random effect, except Maple
#which is being fitted as a fixed effect
model_complete_poisson_maple <- glmer(
  round(clean_complete) ~ (1|trap_name) +
    (Percent_Maple) + (1|patch_name),
  family =poisson(), data = stand_ID_filtered)
summary(model_complete_poisson_maple)

performance::check_overdispersion(model_complete_poisson_maple)
performance::check_model(model_complete_poisson_maple)

#model for Oak and Maple together
model_oak_maple <- glmer(round(clean_complete) ~ Percent_Maple + Percent_Oak + 
                      (1 | trap_name) + (1 | patch_name), 
                    data = stand_ID_filtered, family = poisson)

summary(model_oak_maple)
performance::check_overdispersion(model_oak_maple)

#model for Oak and Maple together, adding an interaction of oak & maple
model_oak_maple_2 <- glmer(round(clean_complete) ~ Percent_Maple + 
                             Percent_Oak + 
                        (1 | trap_name) + (1 | patch_name) + 
                        (Percent_Maple * Percent_Oak), 
                      data = stand_ID_filtered, family = poisson)

summary(model_oak_maple_2)
performance::check_overdispersion(model_oak_maple_2)

#model for Oak, Maple, and Pine together
model_all <- glmer(round(clean_complete) ~ Percent_Maple + Percent_Oak + 
                           Percent_Pine + (1 | trap_name) + (1 | patch_name), 
                         data = stand_ID_filtered, family = poisson)

summary(model_all)
performance::check_overdispersion(model_all)


##Negative binomial, with all levels, except the lowest (trap name)
##Worked, in comparison to Poisson model, but still gave a message of
##having 50 or more warnings
##If Poisson and NB are the same, can just use Poisson

# model_complete_nb <- glmer.nb(round(clean_complete) ~ (1|stand_ID)  +
#                              (1|patch_name), family =nbinom2(),
#                      data = stand_ID_filtered)
# summary(model_complete_nb)
#
# performance::check_overdispersion(model_complete_nb)
# performance::check_model(model_complete_nb)


# All Variables -----------------------------------------------------------

##Fitting a linear model with all of the possible response variables, to 
#explore a correlation between all possible variables and moth counts


#all_variables_model <- lm(clean_complete ~  Percent_Oak + Percent_Pine + 
#              landscape_type + longitude + forest_area_km2 + stand_area_ha, 
#              data = stand_ID_filtered)
#summary(all_variables_model)
#AIC(all_variables_model)

#print(summary(all_variables_model))

#checking for multicollinearity between variables
#the vif method is appropriate for linear models (lm, glm)
#vif(all_variables_model)

# Tidy up the coefficient table and round
#tidy_part <- tidy(all_variables_model)
#tidy_part[, -1] <- round(tidy_part[, -1], 3)

# Model fit stats (like R-squared)
#glance_part <- glance(all_variables_model)
#glance_part <- round(glance_part, 3)

# Save both to a nicely formatted markdown file
#sink("all_variables_model_summary.txt")

#cat("## Coefficients:\n")
#print(kable(tidy_part, format = "markdown"))

#cat("\n## Model Fit Statistics:\n")
#print(kable(glance_part, format = "markdown"))

#sink()


# Save it as a CSV file
# Tidy the model and round numeric columns
#tidy_model <- tidy(all_variables_model)

# Round all numeric columns (except the term column) to 3 decimal places
#tidy_model[ , -1] <- round(tidy_model[ , -1], 4)

# Print a clean table to the console or a markdown/HTML-friendly output
#kable(tidy_model, digits = 4, caption = "all_variables_model_results.csv")

#write.csv(tidy_model, "all_variables_model_results.csv", row.names = FALSE)



##Fitting a poisson model with all of the possible response variables, to 
#explore a correlation between all possible variables and moth counts

all_variables_poisson <- glmer(round(clean_complete) ~ (1|trap_name) + 
              Percent_Oak + Percent_Pine + landscape_type + longitude + 
              forest_area_km2 + stand_area_ha + (1|patch_name),
              family = poisson(), data = stand_ID_filtered)

summary(all_variables_poisson)
AIC(all_variables_poisson)

print(summary(all_variables_poisson))

tab_model(all_variables_poisson,
          show.ci = FALSE,     # hide CI since you only want SE & p
          show.stat = TRUE,    # show test statistics
          p.style = "numeric") # show numeric p-values


# Print a clean table to the console or a markdown/HTML-friendly output
# Save directly to a file
#tab_model(all_variables_poisson,
#          show.stat = TRUE,
#          p.style = "numeric",
#          file = "model_summary.doc")   # can be .doc, .html, .htm

#checking for collinearity between variables
#using the 'performance' package, which is built for linear 
#mixed models (glmer)
collinearity <- check_collinearity(all_variables_poisson)
print(collinearity)

# Convert to a regular data frame
col_df <- as.data.frame(collinearity)
print(col_df)

#write.csv(as.data.frame(collinearity),
 #         "Multicollinearity check (VIF).csv",
 #         row.names = FALSE)


# checking for co-variation between numeric predictors
#in a Pearson correlation matrix
numeric_vars <- stand_ID_filtered[, c("Percent_Oak", "Percent_Pine", "longitude", 
                                      "forest_area_km2", "stand_area_ha")]
# Correlation matrix
cor(numeric_vars, use = "complete.obs")

library(gt)

cor_mat <- cor(numeric_vars, use = "complete.obs")
cor_df <- as.data.frame(round(cor_mat, 3))
cor_df |> gt::gt() |> gt::tab_caption("Correlation Matrix")

#write.csv(cor_df, "correlation_matrix.csv", row.names = TRUE)


#checking for interaction between oak and landscape type
interaction_model <- lm(clean_complete ~ Percent_Oak * landscape_type + 
                          Percent_Pine + longitude + forest_area_km2 + stand_area_ha, 
                        data = stand_ID_filtered)
summary(interaction_model)
AIC(interaction_model)
performance::check_overdispersion(interaction_model)

#checking for interaction between oak and landscape type, with a glm model
interaction_model_glm <- glmer(round(clean_complete) ~ 
                                 Percent_Oak * landscape_type + 
                                 Percent_Pine + 
                      (1 | trap_name) + (1 | patch_name), 
                    data = stand_ID_filtered, family = poisson)

summary(interaction_model_glm)
AIC(interaction_model_glm)
performance::check_overdispersion(interaction_model_glm)
performance::check_model(interaction_model_glm)

tab_model(interaction_model_glm,
          show.ci = FALSE,     # hide CI since you only want SE & p
          show.stat = TRUE,    # show test statistics
          p.style = "numeric") # show numeric p-values


# Print a clean table to the console or a markdown/HTML-friendly output
# Save directly to a file
tab_model(interaction_model_glm,
         show.stat = TRUE,
          p.style = "numeric",
          file = "interaction_model_summary.doc")   # can be .doc, .html, .htm


ggplot(stand_ID_filtered, aes(x = Percent_Oak, y = clean_complete, color = landscape_type)) +
  geom_point(alpha = 0.6) +  # Add raw data points with transparency
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines by group
  theme_minimal() +
  labs(
    title = " ",
    x = "Percent Oak",
    y = "Moth Counts",
    color = "Landscape Type"
  ) +
  scale_color_viridis_d(option = "D")  # You can also try options "A", "B", "C", "E"

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


# Best Model --------------------------------------------------------------

model_complete_poisson_2 <- glmer(
  round(clean_complete) ~ (1|trap_name) 
  + (1|stand_ID) + 
    (1|patch_name) + stand_type_ord, 
  family =poisson(), data = stand_ID_filtered)
summary(model_complete_poisson_2)

performance::check_overdispersion(model_complete_poisson_2)
performance::check_model(model_complete_poisson_2)


##using the 'marginaleffects' package, we can run model-based predictions 
#(prediction => outcome expected by a fitted model for a given combination
#of predictor values)
library(marginaleffects)

plot_predictions(model_complete_poisson_2, condition = "stand_type_ord")

##using random slopes as well as an intercept to account for the fact that 
##there is an average effect of stand type BUT individual sites respond differently
##to it - this is important because the effect of stand type clearly varies from
##patch to patch
model_complete_3 <- glmer(
  round(clean_complete) ~ (1|trap_name) + (1|stand_ID) + 
    (1+stand_type_ord|patch_name) + stand_type_ord, 
  family =poisson(), data = stand_ID_filtered)
summary(model_complete_3)

#help('isSingular')

performance::check_overdispersion(model_complete_3)
performance::check_model(model_complete_3)

plot_predictions(model_complete_3, condition = "stand_type_ord")

#model 3 struggles with processing both stand type and a random effect on
#top of stand ID - too many variables
#simplify the model by - actually ends up being like poisson model 2...
# model_complete_4 <- glmer(
#   round(clean_complete) ~ (1|trap_name) + (1|stand_ID) + 
#     (1|patch_name), 
#   family =poisson(), data = stand_ID_filtered)
# summary(model_complete_4)
# 
# 
# performance::check_overdispersion(model_complete_4)
# performance::check_model(model_complete_4)

# Heatmap oak/pine spectrum -----------------------------------------------

# Create the 'oak/pine spectrum' table
table(stand_category_filtered$Percent_Oak, 
      stand_category_filtered$Percent_Pine)

# Create the contingency table
contingency_table <- table(stand_type_filtered$Percent_Oak, 
                           stand_type_filtered$Percent_Pine)

# Convert the table to a data frame
contingency_df <- as.data.frame(contingency_table)

# Create a plot (heatmap) of the contingency table
contingency_df 

ggplot(contingency_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(x = "Proportion Oak", y = "Proportion Pine", fill = "Frequency") +
  theme_minimal()

# Save the plot as an image (e.g., PNG)
#ggsave("oak_pine_heatmap.png", width = 7.5, height = 6)


# Citations ---------------------------------------------------------------

##   @Manual{,
##     title = {R: A Language and Environment for Statistical Computing},
##     author = {{R Core Team}},
##     organization = {R Foundation for Statistical Computing},
##     address = {Vienna, Austria},
##     year = {2021},
##     url = {https://www.R-project.org/},
##   }

version$version.string

citation("lme4")

# Censored data - brms ----------------------------------------------------

#' ## Censoring
#' this is *fun* and so **helpful**

##Ensuring system configuration to be able to compile C++ and 
#installing rstan

#'remotes::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan")

library(brms)

?brmsformula

##using model_complete_poisson_2, but converting it to a 'brm' model instead
#of 'glmer'

brm_model_1 <- brm(
  round(clean_complete) ~ (1|trap_name) 
  + (1|stand_ID) + 
    (1|patch_name) + stand_type_ord, 
  family =poisson(), data = stand_ID_filtered)
summary(brm_model_1)

plot_predictions(brm_model_1, condition = "stand_type_ord")

#taking the same model as above (adding 'Year') and adding the censored
#data to it
#'I have data that is "right censored". This is because each of my data points
#'is either a complete or a partial count. The partial data indicates 
#'a minimum value, where the actual value is necessarily above the available
#'data, we just don't know by how much. 

brm_model_2 <- brm(
  total_moth_count|cens(censored) ~ (1|trap_name) 
  + (1|stand_ID) + Year +
    (1|patch_name) + stand_type_ord, 
  family =poisson(), data = stand_ID_filtered, iter = 4000)
summary(brm_model_2)


plot_predictions(brm_model_2, condition = "stand_type_ord")

##same as above, but for 'model_complete_3' using random slopes as well as 
#an intercept 

brm_model_3 <- brm(
  total_moth_count|cens(censored) ~ (1|trap_name) 
  + (1|stand_ID) + Year +
    (1+stand_type_ord|patch_name) + stand_type_ord, 
  family =poisson(), data = stand_ID_filtered, iter = 4000,
  control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)
#control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
summary(brm_model_3)

#help('isSingular')

plot_predictions(brm_model_3, condition = "stand_type_ord")

loo_2 <- loo(brm_model_2)
loo_3 <- loo(brm_model_3)
loo_compare(loo_2, loo_3)

