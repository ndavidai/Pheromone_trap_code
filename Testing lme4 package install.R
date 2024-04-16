Test1 <- read.csv("input/moth_glm.csv")

options(repos = c(CRAN = "https://cloud.r-project.org"))
utils::install.packages("Matrix")
utils::install.packages("lme4")

library(Matrix)
library(lme4)



install.packages("MatrixModels")




tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
install.packages("lme4", type = "source")
library(lme4)



lmer(total_continuous ~ prop_oak + (total_continuous | patch_name),
     data = Test1, REML = TRUE)
