####################################
### MERGING FOR  TOTAL SUM GRAPH ###
####################################

#Run this to load the moth_counts_clean data frame used to make these graphs 
source("2024_code/scripts/2024_Data_Cleaning.R")


## Merge duplicate stand types into a total sum mass and moth counts
moth_counts_total <- moth_counts_clean %>%
  group_by(stand_type) %>%
  summarise(Mass = sum(mass_g, na.rm=T), #na.rm=T so that it ignores NAs and gives actual values 
            Moths = sum(total_moth,na.rm=T))

unique(moth_counts_total$Mass)
unique(moth_counts_total$Moths)

## Not for 2024 - Merge the original data set with the new totaled data set, the NEW columns are added at the end, and some are doubled
#moth_counts_2 <- merge(moth_counts_clean,moth_counts_total, by="stand_type")

# change stand_type to trap_ID, so that an 'actual' stand_type column can be created
#colnames(moth_counts_clean)[colnames(moth_counts_clean)=="stand_type"] <- "trap_ID"

#colnames(moth_counts_total)[colnames(moth_counts_total)=="stand_type"] <- "trap_ID"

## change "Co-Dom" to "Mid" in order to create a unique variable (different from "Dom")
#moth_counts_2 <- moth_counts_2 %>%
#mutate(trap_ID = str_replace(trap_ID, "Co-Dom", "Mid"))

# moth_counts_total <- moth_counts_total %>%
#   mutate(trap_ID = str_replace(trap_ID, "Co-Dom", "Mid"))

# # create the actual stand_type column, identifying the oak treatment of each trap
# moth_counts_2$stand_type <- ifelse(grepl("Mid",moth_counts_2$trap_ID), "Co-Dom",
#                                    ifelse(grepl("Low",moth_counts_2$trap_ID), "Low",
#                                           ifelse(grepl("Dom",moth_counts_2$trap_ID), "Dom", "")))
# 
# moth_counts_total$stand_type <- ifelse(grepl("Mid",moth_counts_total$trap_ID), "Co-Dom",
#                                        ifelse(grepl("Low",moth_counts_total$trap_ID), "Low",
#                                               ifelse(grepl("Dom",moth_counts_total$trap_ID), "Dom", "")))


library(ggplot2)
library(forcats)

# # create dataset with a count of how many of each oak treatment types have low, mid, high moth counts
# t  <- moth_counts_clean %>% 
#   mutate(stand_type = fct_relevel(as.factor(stand_type), "Low","Co-Dom","Dom"),
#          moth_count = fct_relevel(as.factor(total_consolidated),"Very High","High", "Mid","Low")) %>%
#   group_by(stand_type, moth_count) %>%
#   tally()
# 
# ## Remove un-needed rows ##
# t_clean <- t[-c(5,10,15),]
# 
# # create dataset with a count of how many of each oak treatment types had various "muck" amounts
# t_muck  <- moth_counts_2 %>% 
#   mutate(stand_type = fct_relevel(as.factor(stand_type), "Low","Co-Dom","Dom"),
#          muck_count = fct_relevel(as.factor(muck_amount),"Very High","High","Medium","Low")) %>%
#   group_by(stand_type, muck_amount) %>%
#   tally() %>%
#   filter(muck_amount != "" & muck_amount != "  ")


## stacked plot of total content amount (categorical) by stand type (categorical), traps with 2 bags already merged
ggplot(moth_counts_clean,aes(x=stand_type, y=total_moth))+
  geom_col(position = "stack") +
  #scale_fill_viridis_d() +
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

## stacked plot of muck amount (categorical) by stand type (categorical), CHECK ERRORS FOR THIS WITH BELLA!!!
ggplot( t_muck,aes(x=stand_type, y=n,fill=muck_amount))+
  geom_col(position = "stack") +
  scale_fill_viridis_d() +
  labs(x = "Stand Type", y = "", fill = "Muck Count") +
  theme_classic()+
  theme(axis.title.x = element_text(size = 18L, face = "bold"), 
        legend.title = element_text(size = 15L, face = "bold"),
        legend.text = element_text(size = 12L),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=13))

ggsave("muck_count_plot.png",
       scale = 1,
       width = dpi = 400)


##Multinomial Logistic Regression Analysis (https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/)
library(foreign)
library(nnet)
library(reshape2)

moth_mlg <- moth_counts_2 %>%
  mutate(total_moth_count = str_replace(total_moth_count, "NA", ""))

with(moth_counts_2, do.call(rbind, tapply(total_moth_count, total_content, function(x) c(M = mean(x), SD = sd(x)))))

with(t_clean, do.call(rbind, tapply(stand_type, moth_count, function(x) c(M = mean(x), SD = sd(x)))))


## box & whisker plot of moth count (numberical) by Stand type (categorical) - each data point a totalled trap count
ggplot(moth_counts_2) +
  aes(x = Moths, y = stand_type) +
  geom_boxplot(fill = "#A429AF") +
  labs(x = "Number of Male Moths", 
       y = "Oak Presence") +
  theme_minimal() +
  theme(plot.caption = element_text(size = NA), axis.title.y = element_text(size = 25L, 
                                                                            face = "bold"), axis.title.x = element_text(size = 25L, face = "bold"), 
        axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))


#visualize data
#library(esquisse)

library(esquisse)
esquisser(moth_counts_2)



