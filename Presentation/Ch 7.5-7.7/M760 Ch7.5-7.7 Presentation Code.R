#############
# Libraries #
#############

library(AICcmodavg)
library(readr)
library(readxl)

###########
# Dataset #
###########

bmi <- read.csv("D:/Coding/R Storage/bmi.data.csv")
attach(bmi)

##########
# Models #
##########

age.lm <- lm(bmi ~ age, data = bmi)
sex.lm <- lm(bmi ~ sex, data = bmi)
consume.lm <- lm(bmi ~ consumption, data = bmi)
ageSex.lm <- lm(bmi ~ age + sex, data = bmi)
all.lm <- lm(bmi ~ age + sex + consumption, data = bmi)
allInteract.lm <- lm(bmi ~ age*sex*consumption, data = bmi)

model <- list(age.lm, sex.lm, consume.lm, ageSex.lm, all.lm, allInteract.lm)
model.names <- c("Age", "Sex", "Consume", "Age/Sex", "All", "All Interact")
aictab(cand.set = model, modnames = model.names)
