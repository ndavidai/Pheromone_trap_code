#####################
### DATA CLEANING ###
#####################

#Run this to load packages 
source("2024_code/scripts/Packages.R")

#There is code at the top of all other scripts that runs this script automatically to load the data

#### 2024 moth count data ####
#loading data 

moth_counts_unedited <- read.csv("2024_code/input/Moth_count_data_2024.csv")

# # library(janitor) cleans up column names.It removes all unique characters and replaces spaces with _.
# # piping using `dplyr`
# 
# moth_counts_cleannames <- moth_counts_unedited %>%
#   clean_names() #Cleans names of an object (usually a data.frame)

# # if any column names need replacing
# colnames(moth_counts_cleannames)[colnames(moth_counts_cleannames)=="x_muck_mass_g"] <- "muck_mass"

# # looking for mistakes
# unique(moth_counts_cleannames$stand_type)
# unique(moth_counts_cleannames$patch_name)

# #above code found there to be 'oak' and 'Oak', same for pine. Following code replaces pine/oak with Pine/Oak
# moth_counts_pineoakfix <- moth_counts_cleannames %>%
#   mutate(stand_type = str_replace_all(stand_type, 'pine','Pine'))%>%
#   mutate(stand_type = str_replace_all(stand_type, 'oak', 'Oak'))
# 
# unique(moth_counts_pineoakfix$stand_type)


# in order to standardize all patch names, remove all spaces and replace with _'s

moth_counts_clean <- moth_counts_unedited %>%
  mutate(patch_name = str_replace_all(patch_name, " ","_"))

moth_counts_clean$stand_category[is.na(moth_counts_clean$stand_category)] <- "MOM" #replaces NA's with "MOM" in stand category

# quick visualizations if needed
# summary(moth_counts_clean)
# str(moth_counts_clean)

#####

#just total moths =

grouped_data <- moth_counts_clean %>%
  group_by(patch_name, stand_type,stand_category,trap_name) %>% #groups the data in these group(s) to perform the following operations
  summarize(total_moth_count = sum(moth_count, na.rm = T))

#write.csv(grouped_data, file = "Grouped_data.csv") #use this to download the grouped_data dateframe


  


