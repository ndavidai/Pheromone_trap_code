#########################
### STACKED BAR GRAPH ###
#########################

#Run this to load the moth_counts_clean data frame used to make these graphs 
source("2024_code/scripts/2024_Data_Cleaning.R")

moth_counts_clean_woNA <- subset(moth_counts_clean, stand_category != "NA")

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
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        axis.title.y = element_text(vjust = 3))+
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
  labs(x="Patch Name", y= "Mean Moth Count")+
  scale_y_continuous(limits = c(0, 400))+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        legend.position = "none",
        axis.title.y = element_text(vjust = 3))+
  scale_fill_manual(values = safe_colorblind_palette)

moth_counts_summarized_woNA <- subset(moth_counts_summarized, stand_category != "NA")  

moth_counts_summarized_woNA %>%
  ggplot(aes(x=patch_name, y=mean_moth_count, fill=stand_category)) +
  geom_col()+
  theme_minimal()+
  labs(x="Patch Name", y= "Mean Moth Count")+
  scale_y_continuous(limits = c(0, 320))+
  theme(legend.background = element_rect("white"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        axis.title.y = element_text(vjust = 3))+
  scale_fill_viridis_d(option = "H", name = "Patch Category") #Colour blind pallette  :) 

#####

#Graph of the mean number of moths found in each stand_category 

breaks <- c("A >=80/0", "B >=80/10", "C 60-70/0", "D 60-70/10", #used to create the legend
  "E 40-50/10","F 40-50/20-30","G 40-50/40-50",
  "H 20-30/20-30","I 20-30/40-50","J 0-10/40-50",
  "K 10-30/60-70","L 0/60-70","M 0->=80")

moth_counts_stand_category <- moth_counts_clean_woNA %>% 
  group_by(stand_category) %>% #groups the data in these groups to perform the following operations
  summarize(mean_moth_count = mean(moth_count, na.rm = T),
            SE_moth_count = std.error(moth_count, na.rm = T))

moth_counts_stand_category %>%
  ggplot(aes(x=stand_category, y=mean_moth_count, fill=stand_category)) +
  geom_col()+
  geom_errorbar(aes(ymin=mean_moth_count-SE_moth_count, ymax=mean_moth_count+SE_moth_count), width=0.25, linewidth=1.25, colour="lightsteelblue3")+
  labs(x="Stand Category", y= "Mean Moth Count")+
  scale_fill_viridis_d(option = "H", name= "Patch Category (Oak/Pine %)", labels=breaks)+
    scale_y_continuous()+ 
  theme_minimal()+
  theme(legend.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        axis.title.y = element_text(vjust = 3))
  
  
ggsave("stand_category_bar_SE.png", path = "2024_code/Graphs", dpi = 600, width = 20, height = 15, units = "cm", bg= "white") #NEED TO ADD BACKGROUND = WHITE BECAUSE theme.minimal IS A TRANSPARENT PNG BACKGROUND

#####

#Graph of the avg number of moths found in the each stand type

moth_count_cleanwoNAandMOM <- subset(moth_counts_clean_woNA, stand_type != "MOM")

moth_counts_summarized_standtype_woNAandMOM <- moth_count_cleanwoNAandMOM %>% 
  group_by(patch_name, stand_type) %>% #groups the data in these groups to perform the following operations
  summarize(mean_moth_count = mean(moth_count, na.rm = T))

moth_counts_summarized_standtype_woNAandMOM %>%
  ggplot(aes(x=stand_type, y=mean_moth_count, fill=patch_name)) +
  geom_col()+
  theme_minimal()+
  labs(x="Stand Type", y= "Mean Moth Count")+
  theme(legend.background = element_rect("white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1.5),
        axis.title.y = element_text(vjust = 3))+
  scale_fill_manual(name="Patch Name",values=safe_colorblind_palette_edited) #Colour blind pallette  :)

ggsave("AverageMoth_Stand_Type.png", path = "2024_code/Graphs", dpi = 600, width = 15, height = 10, units = "cm", bg= "white") #NEED TO ADD BACKGROUND = WHITE BECAUSE theme.minimal IS A TRANSPARENT PNG BACKGROUND

