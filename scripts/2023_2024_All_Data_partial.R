
#### 21-02-25

#install.packages(c("glmmTMB", "lme4"))

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
library(glmmTMB)
library(COMPoissonReg)
library(Rcpp)
library(numDeriv)
library(ggplot2)


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

##separate Stand Type column so that we have a Stand ID for each stand in each patch
stand_ID_filtered <- stand_type_filtered %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, sep = "\\." ) %>% 
  glimpse()

stand_ID_filtered_1 <- stand_category_filtered %>% 
  separate(trap_name, into = c("stand_ID", "trap_number"), remove = FALSE, sep = "\\." ) %>% 
  glimpse()

total_moths_2023_2024 <- sum(complete_2023_2024$clean_complete, na.rm = TRUE)
print(total_moths_2023_2024)

n_distinct(complete_2023_2024$trap_name)

n_distinct(stand_ID_filtered$stand_ID)

n_distinct(stand_ID_filtered$patch_name)



#check to see the distribution of moth count data
hist(complete_2023_2024$clean_complete, 
          main = " ", 
          xlab = "Spongy moth count/trap", 
          ylab = "Frequency", 
          col = "darkblue", 
          border = "black")

hist(complete_2023_2024$clean_complete, 
     main = " ", 
     xlab = "Spongy moth count/trap", 
     ylab = "Frequency", 
     col = "blue", 
     border = "black",
     breaks = 50)  # increase this number to make bins smaller


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



# Summary statistics for moth count by patch
summary_stats_4 <- stand_type_filtered %>%
  group_by(patch_name) %>%
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

###NEED TO SAVE AND EXPORT THESE SUMMARY TABLES###


# Visualizations ----------------------------------------------------------

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


# Negative Binomial models ------------------------------------------------

#checking compatibility of glmmTMB and lme4 packages
packageVersion("glmmTMB")
packageVersion("lme4")
packageVersion("Matrix")
packageVersion("TMB")
packageVersion("mgcv")

sapply(c("Matrix", "TMB", "lme4", "glmmTMB"), find.package)


#need to convert to 'factor' for negative binomial
stand_ID_filtered$patch_name <- as.factor(stand_ID_filtered$patch_name)
stand_ID_filtered$stand_ID <- as.factor(stand_ID_filtered$stand_ID)

library(mgcv)

##NB with Oak

Oak_nb_model <- gam(round(clean_complete) ~ Percent_Oak + Year +
                   s(patch_name, bs = "re") +  # random effect for patch_name
                   s(stand_ID, bs = "re"),     # random effect for stand_ID
                 family = nb(),                # negative binomial
                 method = "REML", 
                 data = stand_ID_filtered)
summary(Oak_nb_model)
plot(Oak_nb_model, pages = 1)

# Fitted values
fitted_vals_oak <- fitted(Oak_nb_model)

# Pearson residuals
pearson_resid_oak <- residuals(Oak_nb_model, type = "pearson")

# Residual degrees of freedom
rdf_oak <- df.residual(Oak_nb_model)

plot(fitted_vals_oak, pearson_resid_oak, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
dispersion_oak <- sum(pearson_resid_oak^2) / rdf_oak
dispersion_oak
#dispersion = 0.852, indicating that there is no over (or much under) dispersion

performance::check_model(Oak_nb_model, residual_type = "normal")


##NB with Pine

Pine_nb_model <- gam(round(clean_complete) ~ Percent_Pine + Year +
                      s(patch_name, bs = "re") +  # random effect for patch_name
                      s(stand_ID, bs = "re"),     # random effect for stand_ID
                    family = nb(),                # negative binomial
                    method = "REML", 
                    data = stand_ID_filtered)
summary(Pine_nb_model)
plot(Pine_nb_model, pages = 1)

# Fitted values
fitted_vals_pine <- fitted(Pine_nb_model)

# Pearson residuals
pearson_resid_pine <- residuals(Pine_nb_model, type = "pearson")

# Residual degrees of freedom
rdf_pine <- df.residual(Pine_nb_model)

plot(fitted_vals_pine, pearson_resid_pine, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
dispersion_pine <- sum(pearson_resid_pine^2) / rdf_pine
dispersion_pine
#dispersion = 0.845, indicating that there is no over (or much under) dispersion

performance::check_model(Pine_nb_model, residual_type = "normal")


##NB with both Oak and Pine

Both_nb_model <- gam(round(clean_complete) ~ Percent_Oak + Percent_Pine + 
                       Year +
                       Percent_Oak*Percent_Pine +
                       s(patch_name, bs = "re") +  # random effect for patch_name
                       s(stand_ID, bs = "re"),     # random effect for stand_ID
                     family = nb(),                # negative binomial
                     method = "REML", 
                     data = stand_ID_filtered)
summary(Both_nb_model)
plot(Both_nb_model, pages = 1)

# Fitted values
fitted_vals_both <- fitted(Both_nb_model)

# Pearson residuals
pearson_resid_both <- residuals(Both_nb_model, type = "pearson")

# Residual degrees of freedom
rdf_both <- df.residual(Both_nb_model)

plot(fitted_vals_both, pearson_resid_both, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
dispersion_both <- sum(pearson_resid_both^2) / rdf_both
dispersion_both
#dispersion = 0.852, indicating that there is no over (or much under) dispersion

performance::check_model(Both_nb_model, residual_type = "normal")

#Adding a variable looking at year 1 and 2 

Year_nb_model <- gam(round(clean_complete) ~ 
                       Year +
                       s(patch_name, bs = "re") +  # random effect for patch_name
                       s(stand_ID, bs = "re"),     # random effect for stand_ID
                     family = nb(),                # negative binomial
                     method = "REML", 
                     data = stand_ID_filtered)
summary(Year_nb_model)
plot(Year_nb_model, pages = 1)

# Fitted values
fitted_vals_year <- fitted(Year_nb_model)

# Pearson residuals
pearson_resid_year <- residuals(Year_nb_model, type = "pearson")

# Residual degrees of freedom
rdf_year <- df.residual(Year_nb_model)

plot(fitted_vals_year, pearson_resid_year, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
dispersion_year <- sum(pearson_resid_year^2) / rdf_year
dispersion_year
#dispersion = 0.820, indicating that there is no over (or much under) dispersion

performance::check_model(Year_nb_model, residual_type = "normal")

# Nested NB models --------------------------------------------------------


# Fit GAM with nested random effects - !!!nested line seems to bug out
#R, and after running it, nothing else works until R is restarted!!!

#checking structure of nesting
with(stand_ID_filtered, table(patch_name, stand_ID))

Oak_nb_model_nested <- gam(
  round(clean_complete) ~ Percent_Oak +
    s(patch_name, bs = "re") +               # random effect for patch_name
    s(patch_name, stand_ID, bs = "re"),      # nested random effect: stand_ID within patch_name
  family = nb(),
  method = "REML",
  data = stand_ID_filtered
)

summary(Oak_nb_model_nested)
plot(Oak_nb_model_nested, pages = 1)


Oak_nb_nested <- gam(round(clean_complete) ~ Percent_Oak + 
                       s(patch_name, bs = "re") +  # random effect for patch_name
                       s(stand_ID, bs = "re") +     # random effect for stand_ID
                       s(stand_ID, by = patch_name, bs = "re"), # nested effect: stand_ID within patch_name
                     family = nb(),                # negative binomial
                     method = "REML", 
                     data = stand_ID_filtered)
summary(Oak_nb_nested)
plot(Oak_nb_nested, pages = 1)


# View summary of fixed and random effects
summary(gam_model_nested)

# Optional: visualize random effects
plot(gam_model_nested, pages = 1)




#NB w Oak - not working due to a compatibility issue between 
#lme4 with MacOS
model_complete_nb_oak_nested <- glmmTMB(
  round(clean_complete) ~ Percent_Oak + 
    (1 | patch_name/stand_ID),
  family = nbinom2,
  data = stand_ID_filtered
)

summary(model_complete_nb_oak_nested)


performance::check_overdispersion(model_complete_nb_oak_nested)
performance::check_model(model_complete_nb_oak_nested)

# All Variables -----------------------------------------------------------

##Fitting a gam model with all of the possible response variables, to 
#explore a correlation between all possible variables and moth counts

all_variable_nb <- gam(round(clean_complete) ~ Percent_Oak + Percent_Pine + 
                         + landscape_type + longitude + 
                         forest_area_km2 + stand_area_ha + Year +
                         s(patch_name, bs = "re") +  # random effect for patch_name
                         s(stand_ID, bs = "re"),     # random effect for stand_ID
                       family = nb(),                # negative binomial
                       method = "REML", 
                       data = stand_ID_filtered)
summary(all_variable_nb)
plot(all_variable_nb, pages = 1)

# Fitted values
fitted_vals_all <- fitted(all_variable_nb)

# Pearson residuals
pearson_resid_all <- residuals(all_variable_nb, type = "pearson")

# Residual degrees of freedom
rdf_all <- df.residual(all_variable_nb)

plot(fitted_vals_all, pearson_resid_all, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
dispersion_all <- sum(pearson_resid_all^2) / rdf_all
dispersion_all
#dispersion = 0.893, indicating that there is no over (or much under) dispersion

performance::check_model(all_variable_nb, residual_type = "normal")
# Print a clean table to the console or a markdown/HTML-friendly output
# Save directly to a file
tab_model(all_variable_nb,
          show.stat = TRUE,
          p.style = "numeric",
          file = "model_summary.doc")   # can be .doc, .html, .htm

#checking for collinearity between variables

#VIF = 1 indicates no multicollinearity
collinearity_all <- check_collinearity(all_variable_nb)
print(collinearity_all)

write.csv(as.data.frame(collinearity_all),
          "Multicollinearity check nb w year (VIF).csv",
          row.names = FALSE)


# Remove longitude and forest area from all variables model--------------------------------------------------------

all_variable_nb_clean <- gam(round(clean_complete) ~ Percent_Oak + 
                               Percent_Pine + landscape_type + 
                         stand_area_ha + Year +
                         s(patch_name, bs = "re") +  # random effect for patch_name
                         s(stand_ID, bs = "re"),     # random effect for stand_ID
                       family = nb(),                # negative binomial
                       method = "REML", 
                       data = stand_ID_filtered)
summary(all_variable_nb_clean)
plot(all_variable_nb_clean, pages = 1)

# Fitted values
fitted_vals_all_clean <- fitted(all_variable_nb_clean)

# Pearson residuals
pearson_resid_all_clean <- residuals(all_variable_nb_clean, type = "pearson")

# Residual degrees of freedom
rdf_all_clean <- df.residual(all_variable_nb_clean)

plot(fitted_vals_all_clean, pearson_resid_all_clean, 
     xlab="Fitted values", ylab="Pearson residuals")
abline(h=0, col="red")

# Dispersion ratio
dispersion_all_clean <- sum(pearson_resid_all_clean^2) / rdf_all_clean
dispersion_all_clean
#dispersion = 0.893, indicating that there is no over (or much under) dispersion

performance::check_model(all_variable_nb_clean, residual_type = "normal")
# Print a clean table to the console or a markdown/HTML-friendly output
# Save directly to a file
tab_model(all_variable_nb_clean,
          show.stat = TRUE,
          p.style = "numeric",
          file = "model_summary.doc")   # can be .doc, .html, .htm


# Correlation tests -------------------------------------------------------

#checking for collinearity between variables

#VIF = 1 indicates no multicollinearity
collinearity_all <- check_collinearity(all_variable_nb)
print(collinearity_all)

#write.csv(as.data.frame(collinearity_all),
#          "Multicollinearity check nb w year (VIF).csv",
 #         row.names = FALSE)

collinearity_all_clean <- check_collinearity(all_variable_nb_clean)
print(collinearity_all_clean)

write.csv(as.data.frame(collinearity_all_clean),
          "Multicollinearity check nb w year (VIF).csv",
          row.names = FALSE)

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


# Oak/Pine graphs ---------------------------------------------------------

ggplot(data = complete_2023_2024, 
       aes(x = Percent_Oak, 
           y = Percent_Pine, 
           size = clean_complete,
           color = as.factor(Year))) +   
  # treat Year as category for distinct colors
    geom_point(alpha = 0.8, 
             position = position_jitter(width = 0.03, height = 0.03)) +
    scale_size_continuous(range = c(0.8, 11), name = "Moth counts") +
    guides(size = guide_legend(override.aes = list(size = c(2,3,4,5))))+
                # scale sizes for moth counts
  scale_color_viridis_d(
    option = "cividis",
    name = "Year",
    labels = c("1", "2")
  ) +
  labs(x = "Proportion Oak",
       y = "Proportion Pine",
       title = "Moth Counts along Oak–Pine Gradient") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )



