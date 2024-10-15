#################
### Box Plots ###
#################

source("2024_code/scripts/2024_Data_Cleaning.R")

# Summary statistics for moth_count by stand_type
moth_count_ST_summary <- moth_counts_clean %>%
  group_by(stand_type) %>%
  summarise(mean_count = mean(moth_count, na.rm = TRUE),
            sd_count = sd(moth_count, na.rm = TRUE),
            count = n())

print(moth_count_ST_summary, n=22)

## Remove MOM row
moth_by_stand_summary_stats <- moth_count_ST_summary[-c(1),] #removes the first row, which is the MOM. 


p4 <- ggplot(moth_by_stand_summary_stats, aes(x=stand_type), fill=class) + 
  geom_boxplot(aes(lower=mean_count-sd_count , upper=mean_count+sd_count ,
                   middle=mean_count , ymin=mean_count-3*sd_count , ymax=mean_count+3*sd_count),
               stat="identity") +
  labs(title = "Distribution of Moth Counts by Stand Type",
       x = "Stand Type",
       y = "Mean Moth Count") +
  theme_classic()

print(p4)