Test1 <- read.csv("input/moth_glm.csv")

install.packages("MatrixModels")

install.packages("Matrix")
library(lme4)

tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
install.packages("lme4", type = "source")
library(lme4)

lmer(total_continuous ~ prop_oak + (total_continuous | patch_name),
     data = Test1, REML = TRUE)