##########################################
### FIXED EFFECTS ESTIMATES MODEL GRAP ###
##########################################

#Run this to load the moth_counts_clean data frame used to make these graphs 
source("2024_code/scripts/2024_Data_Cleaning.R")

moth_model <- glmer.nb(moth_count ~ patch_name + (1|trap_name), family = nbiom2(), data = moth_counts_clean
)
summary(moth_model)

### If you need the same plot for better visualization via ggplot2, gather all the attributes manually
# 1.Extract the fixed effects estimates
fixed_effects <- summary(moth_model)$coefficients

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

 #I would replace patch_name values back to the original names (if needed)
 # 6. Create a lookup table that matches the shortened names to the full names as they appear in the your data
 patch_name_lookup <- c(
   "(Intercept)" = "(Intercept)",
   "patch_nameMont_Saint_Hilaire" = "Mont Saint Hilaire",
   "patch_nameMontebello" = "Montebello",
   "patch_nameNotre_Dame_de_Bonsecours" = "Notre Dame de Bonsecours",
   "patch_nameOka" = "Oka",
   "patch_nameOrford" = "Orford",
   "patch_namePapineauville" = "Papineauville",
   "patch_nameRigaud" = "Rigaud")

 #7. Replace the patch_name values using the lookup table
 fixed_effects_df$patch_name <- patch_name_lookup[fixed_effects_df$patch_name]

# 8. Plot the estimates
ggplot(fixed_effects_df, aes(x = reorder(patch_name, Estimate), y = Estimate)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, linewidth= 1.25, color = "dodgerblue") +  # Error bars for CIs
  geom_point(size = 4, color = "blue") +  # Plot points for estimates 
  labs(
    # title = "Fixed Effects Estimates with Confidence Intervals",
    x = "Patch Name",
    y = "Estimate (log scale)")+
  theme_minimal()+
  theme(text = element_text(size = 19), 
        axis.text.x = element_text(angle = 45, hjust = 1),
                panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        axis.title.y = element_text(vjust = 2))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth=1.25) # Add a horizontal line at y = 0
# above 0 are the positive estimates with greater 
# moth counts 


ggsave("Effect_of_Stand_Type_on_Moth_Population.png", path="2024_code/Graphs",width = 10, height = 8, dpi = 600, bg="white")
