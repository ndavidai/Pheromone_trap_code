#########################
### STACKED BAR GRAPH ###
#########################

#Run this to load the moth_counts_clean data frame used to make these graphs 
source("2024_code/scripts/2024_Data_Cleaning.R")

#####

## MAKING GRAPHS ##

#First graph is looking simply at the TOTAL moth counts in each area, visualizes the differences the numbers in these areas. 
moth_counts_clean %>%
  ggplot(aes(x=patch_name, y=moth_count, fill=patch_name)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous(limits = c(0, 4000), minor_breaks = seq(0, 4500, 1000))+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))
#Not super useful because the data is heavily skewed by the number of traps in a location (Oka had so many traps)

#Second graph is simply just one with the clean data set but looking at the forest typings (A-M)
moth_stacked_barplot <- moth_counts_clean %>%
  ggplot(aes(x=patch_name, y=moth_count, fill=stand_category)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous(limits = c(0, 4500), minor_breaks = seq(0, 4500, 1000))+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#NA's are still in this data set, lets look at a version without NAs next

#Graph without NA's in stand_category
moth_counts_clean_woNA <- subset(moth_counts_clean, stand_category != "NA")

#moth_stacked_barplot_woNA <- add this when ready

moth_counts_clean_woNA %>%
  ggplot(aes(x=patch_name, y=moth_count, fill=stand_category)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous(limits = c(0, 4500), minor_breaks = seq(0, 4500, 1000))+
  theme(legend.background = element_rect("white"),
         axis.text.x = element_text(angle = 45, hjust = 1))

#moth_stacked_barplot_woNA

#####

## MAKING THE AVERAGED GRAPHS ## 

moth_counts_summarized <- moth_counts_clean %>% 
  group_by(patch_name, stand_category) %>% #groups the data in these groups to perform the following operations
  summarize(mean_moth_count = mean(moth_count, na.rm = T),
            SE = std.error(moth_count, na.rm = T))

moth_counts_summarized %>%
  ggplot(aes(x=patch_name, y=mean_moth_count, fill=patch_name)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous()+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

moth_counts_summarized_woNA <- subset(moth_counts_summarized, stand_category != "NA")

moth_counts_summarized_woNA %>%
  ggplot(aes(x=patch_name, y=mean_moth_count, fill=stand_category)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous()+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))



  
