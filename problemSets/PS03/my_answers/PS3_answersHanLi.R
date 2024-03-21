#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base", "package:aer")
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

lapply(c("nnet", "MASS","AER"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)
#create depvar
gdp_data[gdp_data$GDPWdiff==0, "GDPWdiff_cat"] <- "no change"
gdp_data[gdp_data$GDPWdiff>0, "GDPWdiff_cat"] <- "positive"
gdp_data[gdp_data$GDPWdiff<0, "GDPWdiff_cat"] <- "negative"
gdp_data$GDPWdiffcat <- relevel(as.factor(gdp_data$GDPWdiff_cat), ref="no change")
#(1)
gdp_mult <- multinom(GDPWdiff_cat ~ REG + OIL, data=gdp_data)
summary(gdp_mult)
#(2)
gdp_data$GDPWdiff_cat <- relevel(gdp_data$GDPWdiff_cat, ref="negative") #order ascending

gdp_ord <- polr(GDPWdiff_cat ~ REG + OIL, data=gdp_data)
summary(ordered_logit)
#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")
# poisson model
mpoisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family=poisson)
summary(mpoisson)
dispersiontest(mpoisson) #can't reject null as pvalue >0.05, no need for ZIP
predict(mpoisson, newdata=data.frame(competitive.district=1, marginality.06 = 0, PAN.governor.06=1), type="response")
