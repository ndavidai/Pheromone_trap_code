### Data cleaning ###

#### egg data ####
#loading data 
setwd("~/Noas data")
egg_counts <- read.csv("MSB egg mass counts.csv")

# quick visualizations
summary(egg_counts)
str(egg_counts)
unique(egg_counts$Egg.Count)
unique(egg_counts$Site.Name)

## there are 5 columns with nothing on them (X, X.1 and X.2...)- let's remove them ##
library(dplyr)
egg_counts_1 <-egg_counts %>% select(1:7,9:13)

unique(egg_counts_1$Egg.Count.Index)
unique(egg_counts_1$Egg.Count)
#[1] "medium"   "no entry" "none"     "low"      "high"     NA         ","  
## there are some rows that are completely empty- we want to replace those with NA##
# replace empty cells with NA
egg_counts_1 <- replace(egg_counts_1, egg_counts_1=='', NA)
# replace , with NA
egg_counts_1 <- replace(egg_counts_1, egg_counts_1==',', NA)
# replace "no entry" with NA
egg_counts_1 <- replace(egg_counts_1, egg_counts_1=='no entry', NA)
# replace "none" with NA ? maybe not as this seems categorical
egg_counts_1 <- replace(egg_counts_1, egg_counts_1=='none', 0)
unique(egg_counts_1$Egg.Count)
unique(egg_counts_1$Caterpillar.Count)
#replace "none " with "none"
egg_counts_1 <- replace(egg_counts_1, egg_counts_1=='none ', "none")

# if any column names need replacing
colnames(egg_counts_1)[colnames(egg_counts_1)=="Site.Name"] <- "Site_Name"

library(janitor) #janitor cleans up column names.It removes all unique characters and replaces spaces with _.
#piping through `dplyr`
egg_counts_1 <- egg_counts_1 %>%
  clean_names() #Cleans names of an object (usually a data.frame)

#visualize data
#library(esquisse)
#esquisser(egg_counts_1)

#### caterpillar data ####
#loading data 
caterp_counts <- read.csv("MSB Caterpillar Counts.csv")

# quick visualizations
summary(caterp_counts)
str(caterp_counts)
unique(caterp_counts$Egg.Count)
unique(caterp_counts$Site.Name)

#library(janitor) #janitor cleans up column names.It removes all unique characters and replaces spaces with _.
#piping through `dplyr`
caterp_counts <- caterp_counts %>%
  clean_names() #Cleans names of an object (usually a data.frame) 
  
## let's remove notes column ##
library(dplyr)
caterp_counts_1 <-caterp_counts %>% select(-13)
# let's check for inconsistencies in names etc
unique(caterp_counts_1$dbh_cm)
unique(caterp_counts_1$ )

## there are some rows that are completely empty- we want to replace those with NA##
# replace empty cells with NA
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='', NA)
# replace "no entry" with NA (or with something else?)
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='no entry', NA)
# replace "none" with NA ? maybe not as this seems categorical
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='none', 0)

unique(caterp_counts_1$dbh_cm) #see "no entry "
# replace "no entry " with NA (or with something else?)
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='no entry ', NA)
unique(caterp_counts_1$tree_id) #you may want to change these to separate common name and scientific name columns

## replace space between words with underscore
library(tidyverse)
caterp_counts_1 <- caterp_counts_1 %>%
  mutate(tree_id = str_replace(tree_id, " ", "_"))

# replace "none " with "none"
caterp_counts_1 <- replace(caterp_counts_1, caterp_counts_1=='none ', "none")

summary(caterp_counts_1)
str(caterp_counts_1)

## remove % and replace with nothing
caterp_counts_1 <- caterp_counts_1 %>%
  mutate(humidity = str_replace(humidity, "%", ""))

# if any column names need replacing
colnames(caterp_counts_1)[colnames(caterp_counts_1)=="humidity"] <- "humidity_percent"

## some numeric columns are character here- let's change them
caterp_counts_1$dbh_cm <- as.numeric(caterp_counts_1$dbh_cm)
caterp_counts_1$humidity_percent <- as.numeric(caterp_counts_1$humidity_percent)

summary(caterp_counts_1)
str(caterp_counts_1)

library(esquisse)
esquisser(caterp_counts_1)
esquisser(egg_counts_1)


library(ggplot2)

ggplot(egg_counts_1) +
 aes(x = human_use) +
 geom_bar(fill = "#21A05A") +
 theme_classic() +
 theme(plot.title = element_text(size = 20L, 
 face = "bold"), plot.subtitle = element_text(size = 12L, face = "bold.italic"), plot.caption = element_text(size = 12L, 
 face = "bold.italic"), axis.title.y = element_text(size = 18L, face = "bold"), axis.title.x = element_text(size = 18L, 
 face = "bold"))
library(dplyr)
library(ggplot2)

caterp_counts_1 %>%
 filter(!is.na(temp_c)) %>%
 filter(!is.na(humidity_percent)) %>%
 filter(!is.na(in_center)) %>%
 filter(!is.na(count_height)) %>%
 filter(!is.na(egg_count)) %>%
 filter(!is.na(caterpillar_count)) %>%
 filter(!is.na(entered_by)) %>%
 ggplot() +
 aes(x = dbh_cm, y = egg_count) +
 geom_boxplot(fill = "#2FD6BE") +
 scale_x_continuous(trans = "log") +
 coord_flip() +
 theme_minimal()

write.csv(caterp_counts_1, "MSB_caterp_counts_cleaned.csv")
write.csv(egg_counts_1, "MSB_egg_counts_cleaned.csv")


