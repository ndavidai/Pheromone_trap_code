#################
### BOX PLOTS ###
#################

#Run this to load the moth_counts_clean data frame used to make these graphs 
source("2024_code/scripts/2024_Data_Cleaning.R")

#####

## MAKING GRAPHS ##

#First have to generate the mean moth counts, can do this with summariz(/s)e 
moth_counts_noMOM <- subset(moth_counts_clean, stand_type != "MOM")

moth_counts_summarized <- moth_counts_noMOM %>% 
  group_by(date, patch_name, stand_type,) %>% #groups the data in these group(s) to perform the following operations
  summarize(mean_moth_count = mean(moth_count, na.rm = T),
            SD = sd(moth_count, na.rm = T)) #creates a new column named mean_moth_count, which is done by taking the mean of moth_count)

moth_counts_summarized %>%
  ggplot(aes(x=stand_type, y=mean_moth_count))+
  scale_y_continuous()+
  geom_boxplot(aes(lower=mean_moth_count-SD , upper=mean_moth_count+SD ,
               middle=mean_moth_count , ymin=mean_moth_count-3.5*SD , ymax=mean_moth_count+3.5*SD))+
  theme_classic()


moth_counts_clean_woNA <- subset(moth_counts_clean, stand_category != "NA")


#####

# Summary statistics for moth_count by stand_type
moth_count_ST_summary <- moth_counts_clean %>%
  group_by(stand_type) %>%
  summarise(mean_count = mean(moth_count, na.rm = TRUE),
            sd_count = sd(moth_count, na.rm = TRUE),
            count = n())

jitter_moth_summary <- subset(moth_counts_clean_woNA, stand_type != "MOM")
 

#print(moth_count_ST_summary, n=22)

## Remove MOM row
moth_count_ST_summary_noMOM <- moth_count_ST_summary[-c(1),] #removes the first row, which is the MOM. 


moth_count_ST_summary_noMOM %>%
ggplot(aes(x=stand_type, fill=stand_type)) + 
  geom_boxplot(aes(lower=mean_count-sd_count , upper=mean_count+sd_count ,
                   middle=mean_count , ymin=meavn_count-3.5*sd_count , ymax=mean_count+3.5*sd_count),
               stat="identity", ) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733"))+
  geom_jitter(data = moth_counts_summarized, aes(x=stand_type, y=mean_moth_count), size=2)+
  labs(x = "Stand Type",
       y = "Mean Moth Count")+
  theme_classic()




