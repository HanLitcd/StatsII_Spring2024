#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
#recode to factor
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, ordered = F), ref="None")
climateSupport$countries <- relevel(factor(climateSupport$countries, ordered = F), ref="20 of 192")
additive_log_model <- glm(choice~countries+sanctions, data=climateSupport, family = binomial(link = "logit"))
summary(additive_log_model)
#the global null is if all explanatory coefficent are zero, hence we can use anova
null_model<- glm(choice~1,data=climateSupport,family = binomial(link ='logit'))
anova(null_model, additive_log_model, test="LRT")

# Problem 2
#####################
#significant varialbes are: countries80,country160,sanctionNone,Sanction15,sanction20
additive_b <-predict(additive_log_model, newdata = data.frame(countries="80 of 192", sanctions="None"), type="response")
#c adding interative
interactive_log_model <- glm(choice~countries*sanctions, data=climateSupport, family = binomial(link = "logit"))
summary(interactive_log_model)

interactive_b<-predict(interactive_log_model, newdata = data.frame(countries="80 of 192", sanctions="None"), type="response")
change_b<-interactive_b-additive_b
# compare to using anova
anova(additive_log_model, interactive_log_model, test="LRT")
