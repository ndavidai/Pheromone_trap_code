#########################
### STACKED BAR GRAPH ###
#########################

#Run this to load the moth_counts_clean data frame used to make these graphs 
source("2024_code/scripts/2024_Data_Cleaning.R")

moth_stacked_barplot <- moth_counts_clean %>%
  ggplot(aes(x=patch_name, y=moth_count, fill=stand_category)) +
  geom_col()+
  theme_classic()+
  labs(x="Patch Name", y= "Moth Count")+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Graph with out NA's in stand_category
moth_counts_clean_woNA <- subset(moth_counts_clean, stand_category != "NA")

oth_counts_clean_woNA %>%
  ggplot(aes(x=patch_name, y=moth_count, fill=stand_category)) +
  geom_col()+
  theme_classic()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous(limits = c(0, 4500), minor_breaks = seq(0, 4500, 1000))+
  theme(legend.background = element_rect("white"),
         axis.text.x = element_text(angle = 45, hjust = 1))

#moth_stacked_barplot_woNA
  
