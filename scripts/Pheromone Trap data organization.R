### Data cleaning ###

#### moth count data ####
#loading data 

moth_counts <- read.csv("Pheromone_trap_moth_counts2.csv")

library(janitor) #janitor cleans up column names.It removes all unique characters and replaces spaces with _.
#piping through `dplyr`
moth_counts_1 <- moth_counts %>%
  clean_names() #Cleans names of an object (usually a data.frame)

## Remove un-needed columns ##
library(dplyr)
moth_counts_1 <-moth_counts_1 %>% select(1:10,12:13,16:17)

## Remove un-needed rows ##
moth_counts_clean <- moth_counts_1[-c(11,18,25,36,43,50,57,64,72,79,90,97,104,112,119,126,133:1058),]

# if any column names need replacing
colnames(moth_counts_clean)[colnames(moth_counts_clean)=="x_muck_amount"] <- "muck_amount"

# quick visualizations
summary(moth_counts_clean)
str(moth_counts_clean)

# looking for mistakes
unique(moth_counts_clean$stand_type)

#remove all spaces
## in order to standardize all stand type names, remove all spaces
library(tidyverse)
moth_counts_clean <- moth_counts_clean %>%
  mutate(stand_type = str_replace(stand_type, " ", ""))

unique(moth_counts_clean$stand_type)

## again, to remove 2nd space
moth_counts_clean <- moth_counts_clean %>%
  mutate(stand_type = str_replace(stand_type, " ", ""))

unique(moth_counts_clean$stand_type)

## Merge duplicate stand types into a total sum mass and moth counts
moth_counts_total <- moth_counts_clean %>%
  group_by(stand_type) %>%
  summarise(Mass = sum(mass_g),
            Moths = sum(total_moth_count))
            
unique(moth_counts_total$Mass)
unique(moth_counts_total$Moths)

## Merge the original data set with the new totaled data set, the NEW columns are added at the end, and some are doubled
moth_counts_2 <- merge(moth_counts_clean,moth_counts_total, by="stand_type")

# change stand_type to trap_ID, so that an 'actual' stand_type column can be created
colnames(moth_counts_2)[colnames(moth_counts_2)=="stand_type"] <- "trap_ID"

colnames(moth_counts_total)[colnames(moth_counts_total)=="stand_type"] <- "trap_ID"

## change "Co-Dom" to "Mid" in order to create a unique variable (different from "Dom")
moth_counts_2 <- moth_counts_2 %>%
  mutate(trap_ID = str_replace(trap_ID, "Co-Dom", "Mid"))

moth_counts_total <- moth_counts_total %>%
  mutate(trap_ID = str_replace(trap_ID, "Co-Dom", "Mid"))

# create the actual stand_type column, identifying the oak treatment of each trap
moth_counts_2$stand_type <- ifelse(grepl("Mid",moth_counts_2$trap_ID), "Co-Dom",
                                   ifelse(grepl("Low",moth_counts_2$trap_ID), "Low",
                                          ifelse(grepl("Dom",moth_counts_2$trap_ID), "Dom", "")))

moth_counts_total$stand_type <- ifelse(grepl("Mid",moth_counts_total$trap_ID), "Co-Dom",
                                       ifelse(grepl("Low",moth_counts_total$trap_ID), "Low",
                                              ifelse(grepl("Dom",moth_counts_total$trap_ID), "Dom", "")))


#visualize data
#library(esquisse)
#esquisser(egg_mass_counts_1)

library(esquisse)
esquisser(moth_counts_total)


library(ggplot2)
library(forcats)


t  <- moth_counts_2 %>% 
  mutate(stand_type = fct_relevel(as.factor(stand_type), "Low","Co-Dom","Dom"),
         moth_count = fct_relevel(as.factor(total_content),"Very High","High", "Mid","Low")) %>%
  group_by(stand_type, moth_count) %>%
  tally()


## stacked plot of total content amount (categorical) by stand type (categorical), traps with 2 bags NOT merged
ggplot(t,aes(x=stand_type, y=n,fill=moth_count))+
  geom_col(position = "stack") +
  scale_fill_viridis_d() +
  labs(x = "Stand Type", y = "", fill = "Moth Count") +
  theme_classic()+
        theme(axis.title.x = element_text(size = 18L, face = "bold"), 
              legend.title = element_text(size = 15L, face = "bold"),
              legend.text = element_text(size = 12L),
              axis.text.x=element_text(size=13),
              axis.text.y=element_text(size=13))

ggsave("moth_count_plot.png",
       scale = 1,
       width = dpi = 400)


## box & whisker plot of moth count (numberical) by Stand type (categorical) - each data point a totalled trap count
ggplot(moth_counts_total) +
 aes(x = Moths, y = stand_type) +
 geom_boxplot(fill = "#A429AF") +
 labs(x = "Number of Male Moths", 
 y = "Oak Presence") +
 theme_minimal() +
 theme(plot.caption = element_text(size = NA), axis.title.y = element_text(size = 25L, 
 face = "bold"), axis.title.x = element_text(size = 25L, face = "bold"), 
 axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
 

library(esquisse)
esquisser(moth_counts_2)









library(dplyr)
library(ggplot2)


