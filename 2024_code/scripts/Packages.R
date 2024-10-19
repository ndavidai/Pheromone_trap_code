##################################
########### PACKAGES #############
##################################

#Run without the '#' to install the packages 

#install.packages(c("janitor","dplyr","tidyverse","ggplot2","forcats","foreign","nnet","reshape2","esquisse", "summarytools", "lme4", "tidyr","plotrix","MetBrewer","viridis","performance","car","effects","MASS","mgcv","Matrix","MetBrewer"))

#Packages to load
#There is code at the top of 2024_Data_Cleaning.R that runs these packages

library(dplyr)
library(janitor)
library(tidyverse)
library(ggplot2)
library(forcats)
library(foreign)
library(nnet)
library(reshape2)
library(esquisse)
library(summarytools)
library(lme4)
library(tidyr)
library(plotrix) #used for std.error
library(sjPlot) #install.packages("sjPlot")
library(MetBrewer)#nice colourblind friendly options
library(viridis)#nice colourblind friendly options
library(performance)
library(car) #install.packages("car")
library(effects) #install.packages("effects")
library(MASS) #install.packages("MASS")
library(mgcv) #install.packages("mgcv")
library(Matrix)
library(car)
library(effects)
library(MetBrewer)

#####

#Additional: Colourblind package obtained from the rcartocolor package (not installed)

safe_colorblind_palette_edited <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#661100") #HEX CODES OBTAINED FROM rcartocolor package
