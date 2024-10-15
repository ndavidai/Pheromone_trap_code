############################
### BOX PLOT WITH POINTS ###
############################

#Run this to load the moth_counts_clean data frame used to make these graphs 
source("2024_code/scripts/2024_Data_Cleaning.R")

#####

## MAKING GRAPHS ##

#First have to generate the mean moth counts, can do this with summariz(/s)e 
moth_counts_noMOM <- subset(moth_counts_clean, stand_type != "MOM")

moth_counts_summarized <- moth_counts_noMOM %>% 
  group_by(patch_name, stand_type,) %>% #groups the data in these group(s) to perform the following operations
  summarize(mean_moth_count = mean(moth_count, na.rm = T)) #creates a new column named mean_moth_count, which is done by taking the mean of moth_count)
          
moth_counts_summarized %>%
  ggplot(aes(x=stand_type, y=mean_moth_count))+
  scale_y_continuous()+
  geom_boxplot()+
  theme_classic()


moth_counts_clean_woNA <- subset(moth_counts_clean, stand_category != "NA")
