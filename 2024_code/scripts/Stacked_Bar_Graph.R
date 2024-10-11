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
  scale_y_continuous(limits = c(0, 4000))+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        legend.position = "none",
        axis.title.y = element_text(vjust = 3))+
  scale_fill_manual(values = safe_colorblind_palette)
#Not super useful because the data is heavily skewed by the number of traps in a location (Oka had so many traps)

#Second graph is simply just one with the clean data set but looking at the patch categories (A-M)

#NA's are still in this data set, so lets remove them
moth_counts_clean_woNA <- subset(moth_counts_clean, stand_category != "NA")

moth_counts_clean_woNA %>%
  ggplot(aes(x=patch_name, y=moth_count, fill=stand_category)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous(limits = c(0, 4000))+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1, face ="bold"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        axis.title.y = element_text(vjust = 3, face="bold"))+
  scale_fill_viridis_d(option = "H", name = "Patch Category") #Colour blind pallette  :) 

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

moth_counts_summarized_woNA <- subset(moth_counts_summarized2, stand_category != "NA")

moth_counts_summarized_woNA %>%
  ggplot(aes(x=patch_name, y=mean_moth_count, fill=stand_category)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Moth Count")+
  scale_y_continuous()+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

display_all(sequential = FALSE, colorblind_only = T)

library(MetBrewer)

install.packages("viridis")

library(vi)
  
